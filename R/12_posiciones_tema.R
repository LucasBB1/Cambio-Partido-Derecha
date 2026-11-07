# 12_posiciones_tema.R
# Posición previa a la salida por área temática. Para cada período
# legislativo y área (orden y seguridad, que incluye migración; género,
# sexualidad y familia; economía; y todas las votaciones) se estima un modelo
# IRT probit estático en el que cada diputado que sale aporta dos unidades,
# antes y después de su salida. Luego se calcula el percentil de la unidad
# previa dentro de su partido de origen y se aplica la prueba de rangos.

source(file.path("R", "00_funciones.R"))
set.seed(20261106)
convencional <- c("UDI", "RN", "EVOP", "AMPL")

pi <- readRDS(file.path(dir_proc, "puntos_ideales.rds"))
tm <- read_csv(file.path(dir_proc, "votaciones_temas.csv"), show_col_types = FALSE) |>
  mutate(orden = pmax(seguridad, migracion))
panel <- read_csv(file.path(dir_proc, "panel_trimestral.csv"), show_col_types = FALSE)
salidas <- read_csv(file.path(dir_proc, "salidas_verificadas.csv"), show_col_types = FALSE) |>
  mutate(ts = trimestre(fecha_salida),
         grupo = case_when(partido_origen %in% convencional & destino_bloque == "radical" ~ "Hacia la derecha radical",
                           partido_origen %in% convencional ~ "Otras salidas de la derecha convencional",
                           TRUE ~ "Salidas de la derecha radical"))
periodo_de <- function(t) case_when(t <= 17 ~ "2014-2018", t <= 33 ~ "2018-2022", t <= 49 ~ "2022-2026", TRUE ~ "2026-2030")
salidas <- salidas |> mutate(periodo = periodo_de(ts))

d0 <- pi$d |> select(votacion_id, diputado_id, y, t) |> left_join(tm |> select(votacion_id, orden, valorico, economia), by = "votacion_id") |>
  mutate(periodo = periodo_de(t))

estimar <- function(per, area) {
  d <- d0 |> filter(periodo == per)
  if (area != "todas") d <- d |> filter(.data[[area]] == 1)
  s <- salidas |> filter(periodo == per)
  # unidad: diputado, o diputado antes/después de su salida
  d <- d |> left_join(s |> select(id, ts) |> group_by(id) |> slice_min(ts, n = 1) |> ungroup(), by = c("diputado_id" = "id")) |>
    mutate(tramo = case_when(is.na(ts) ~ "completo", t < ts ~ "previo", t > ts ~ "posterior", TRUE ~ NA_character_)) |>
    filter(!is.na(tramo))
  repeat {
    n0 <- nrow(d)
    d <- d |> group_by(votacion_id) |> filter(n() >= 20, min(mean(y), 1 - mean(y)) >= 0.025) |> ungroup() |>
      group_by(diputado_id, tramo) |> filter(n() >= 10) |> ungroup()
    if (nrow(d) == n0) break
  }
  if (nrow(d) == 0) return(NULL)
  un <- d |> distinct(diputado_id, tramo) |> mutate(u = row_number(), leg = u, t = 1L)
  it <- d |> distinct(votacion_id) |> mutate(j = row_number())
  d <- d |> left_join(un |> select(diputado_id, tramo, u), by = c("diputado_id", "tramo")) |> left_join(it, by = "votacion_id")
  M <- matrix(NA_real_, nrow(un), nrow(it)); M[cbind(d$u, d$j)] <- d$y
  M <- sweep(M, 2, colMeans(M, na.rm = TRUE)); M[is.na(M)] <- 0
  x0 <- as.vector(scale(svd(M, nu = 1, nv = 0)$u[, 1]))
  fit <- irt_dinamico(d$u, d$j, d$y, un |> select(u, leg, t), omega2 = 1, x0 = x0, verbose = FALSE)
  un$x <- fit$x
  # orientación: la UDI a la derecha
  part <- panel |> filter(periodo_de(t) == per) |> count(diputado_id, partido) |> group_by(diputado_id) |>
    slice_max(n, n = 1, with_ties = FALSE) |> ungroup() |> select(diputado_id, partido_mayor = partido)
  un <- un |> left_join(part, by = "diputado_id")
  if (median(un$x[un$partido_mayor == "UDI"], na.rm = TRUE) < 0) un$x <- -un$x
  un$z <- (un$x - mean(un$x)) / sd(un$x)
  # percentil previo de cada salida dentro del partido de origen
  s |> rowwise() |> mutate(percentil = {
    mem <- panel |> filter(partido == partido_origen, t == ts - 1, diputado_id != id) |> pull(diputado_id)
    ref <- un |> filter(diputado_id %in% mem, tramo %in% c("completo", "previo"))
    yo <- un |> filter(diputado_id == id, tramo == "previo")
    if (nrow(yo) == 0 || nrow(ref) < 2) NA_real_ else mean(c(ref$z, yo$z) <= yo$z)
  }, n_votos = nrow(it)) |> ungroup() |>
    transmute(id, nombre, grupo, periodo, area, percentil, n_votos)
}

areas <- c("todas", "orden", "valorico", "economia")
res <- map_dfr(c("2014-2018", "2018-2022", "2022-2026"), function(per)
  map_dfr(areas, function(a) { message(per, " ", a); r <- estimar(per, a); if (!is.null(r)) mutate(r, area = a) }))
write_csv(res, file.path(dir_out, "tablas", "12_percentil_por_tema.csv"))

rangos <- res |> filter(grupo != "Salidas de la derecha radical", !is.na(percentil)) |>
  group_by(grupo, area) |>
  summarise(n = n(), percentil_medio = mean(percentil),
            p_derecha = mean(replicate(1e5, mean(runif(n()))) >= mean(percentil)),
            p_izquierda = mean(replicate(1e5, mean(runif(n()))) <= mean(percentil)), .groups = "drop")
print(rangos)
print(res |> filter(grupo == "Hacia la derecha radical") |> select(nombre, area, percentil) |>
        pivot_wider(names_from = area, values_from = percentil), width = 200)
write_csv(rangos, file.path(dir_out, "tablas", "12_rangos_por_tema.csv"))
