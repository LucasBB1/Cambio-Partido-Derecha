# 13_senado.R
# Extensión al Senado (marzo de 2014 a la última votación descargada). Se estiman posiciones
# por senador y trimestre con el mismo modelo IRT dinámico y las dos
# especificaciones de suavizamiento, se reconstruye la militancia de cada
# senador por fecha y se calcula, para cada salida de un partido de derecha,
# el percentil previo dentro del partido de origen y el cambio posterior.
# Con ocho salidas no se estima un modelo de riesgo; el Senado se usa como
# comparación fuera de la muestra de la Cámara.

source(file.path("R", "00_funciones.R"))
set.seed(20261106)
convencional <- c("UDI", "RN", "EVOP", "AMP")
radical <- c("PREP", "PSC", "PNL")
derecha <- c(convencional, radical)
fin_muestra <- as.Date("2099-01-01")   # se usan todas las votaciones descargadas

# 1. Votos ------------------------------------------------------------------
vt <- read_csv(file.path(dir_proc, "votaciones_senado.csv"), show_col_types = FALSE,
               col_types = cols(.default = "c")) |>
  mutate(fecha = as.Date(substr(fecha, 1, 10), format = "%d-%m-%Y"), id_votacion = as.integer(id_votacion))
vo <- read_csv(file.path(dir_proc, "votos_senado.csv.gz"), show_col_types = FALSE)
d <- vo |> filter(opcion %in% c("SI", "NO")) |>
  inner_join(vt |> filter(!is.na(boletin), boletin != "", fecha >= as.Date("2014-03-11"), fecha < fin_muestra) |>
               select(id_votacion, fecha), by = "id_votacion") |>
  transmute(votacion_id = id_votacion, senador_id = parlid, y = as.integer(opcion == "SI"), t = trimestre(fecha))
repeat {
  n0 <- nrow(d)
  d <- d |> group_by(votacion_id) |> filter(min(mean(y), 1 - mean(y)) >= 0.025) |> ungroup() |>
    group_by(senador_id, t) |> filter(n() >= 10) |> ungroup()
  if (nrow(d) == n0) break
}
unidades <- d |> distinct(senador_id, t) |> arrange(senador_id, t) |>
  mutate(u = row_number(), leg = as.integer(factor(senador_id)))
items <- d |> distinct(votacion_id) |> arrange(votacion_id) |> mutate(j = row_number())
d <- d |> left_join(unidades, by = c("senador_id", "t")) |> left_join(items, by = "votacion_id")
message("Senado. Votos: ", nrow(d), " | votaciones: ", nrow(items), " | senadores: ",
        n_distinct(d$senador_id), " | unidades: ", nrow(unidades))

# 2. Militancia por fecha -----------------------------------------------------
norm <- function(s) stringi::stri_trans_general(s, "Latin-ASCII") |> tolower() |> str_replace_all("-", " ") |> str_squish()
sen <- read_csv(file.path(dir_proc, "senadores_votaciones.csv"), show_col_types = FALSE) |>
  mutate(across(c(nombre, ap_paterno, ap_materno), ~ norm(coalesce(.x, ""))))
per <- read_delim(file.path(dir_fuentes, "senadores_periodos.csv"), ";", show_col_types = FALSE) |>
  filter(periodo %in% c("2014-2018", "2018-2022", "2022-2026")) |> mutate(clave = norm(senador))
cam <- read_delim(file.path(dir_fuentes, "cambios_militancia_senado.csv"), ";", show_col_types = FALSE) |>
  mutate(clave = norm(senador))
asignar <- function(clave) {
  ok <- vapply(seq_len(nrow(sen)), function(i) {
    s <- sen[i, ]
    str_detect(clave, fixed(s$ap_paterno)) && (s$ap_materno == "" || str_detect(clave, fixed(s$ap_materno))) &&
      word(clave, 1) == word(s$nombre, 1)
  }, TRUE)
  if (sum(ok) == 1) sen$parlid[ok] else NA_integer_
}
claves <- tibble(clave = unique(c(per$clave, cam$clave))) |> mutate(senador_id = map_int(clave, asignar))
if (any(is.na(claves$senador_id))) print(claves |> filter(is.na(senador_id)))
per <- per |> left_join(claves, by = "clave")
cam <- cam |> left_join(claves, by = "clave")
inicio_per <- c("2014-2018" = "2014-03-11", "2018-2022" = "2018-03-11", "2022-2026" = "2022-03-11",
                "2026-2030" = "2026-03-11")

partido_en <- function(id, f) {
  pp <- per |> filter(senador_id == id, as.Date(inicio_per[periodo]) <= f,
                      as.Date(inicio_per[periodo]) + 365.25 * 4 > f)
  if (nrow(pp) == 0) return(NA_character_)
  pp <- pp[nrow(pp), ]
  cc <- cam |> filter(senador_id == id, periodo == pp$periodo, fecha <= f) |> arrange(fecha)
  if (nrow(cc) > 0) cc$a[nrow(cc)] else pp$partido_inicio
}
# Composición 2026–2030: senadores electos en 2021 (militancia al 10 de marzo
# de 2026, con los cambios documentados) y electos en 2025 (sigla y condición
# de militante o independiente según el SERVEL). Los cambios forzados por la
# disolución de partidos en 2026 no se registran como salidas.
el <- readRDS(file.path(dir_fuentes, "candidatos_servel.rds")) |>
  filter(camara == "Senado", anio %in% c(2021, 2025), electo) |>
  mutate(clave = norm(candidato), senador_id = map_int(clave, asignar))
if (any(is.na(el$senador_id))) { message("Electos sin PARLID:"); print(el |> filter(is.na(senador_id)) |> select(anio, candidato)) }
per26 <- el |> filter(!is.na(senador_id)) |> rowwise() |>
  mutate(partido_inicio = if (anio == 2025) { if (condicion == "militante") sigla else paste0("IND-", sigla) }
                          else partido_en(senador_id, as.Date("2026-03-10"))) |>
  ungroup() |> transmute(periodo = "2026-2030", circunscripcion = unidad, senador = candidato, partido_inicio, clave, senador_id)
print(per26 |> select(senador, partido_inicio), n = 60)
per <- bind_rows(per |> mutate(circunscripcion = as.character(circunscripcion)), per26)

fecha_ref <- function(t) as.Date(paste0(2014 + (t - 1) %/% 4, "-", sprintf("%02d", ((t - 1) %% 4) * 3 + 1), "-01"))

salidas <- read_delim(file.path(dir_fuentes, "salidas_senado.csv"), ";", show_col_types = FALSE) |>
  mutate(fecha_salida = as.Date(fecha_salida), ts = trimestre(fecha_salida),
         grupo = case_when(partido_origen %in% convencional & destino_bloque == "radical" ~ "Hacia la derecha radical",
                           partido_origen %in% convencional ~ "Otras salidas de la derecha convencional",
                           TRUE ~ "Salidas de la derecha radical"))

mil <- unidades |> distinct(senador_id, t) |>
  mutate(partido = map2_chr(senador_id, t, \(i, tt) partido_en(i, fecha_ref(tt) + 45)))
# después de la salida verificada, el senador ya no figura en el partido de origen
for (k in seq_len(nrow(salidas))) {
  s <- salidas[k, ]
  idx <- mil$senador_id == s$parlid & fecha_ref(mil$t) + 45 > s$fecha_salida & mil$partido == s$partido_origen
  mil$partido[idx] <- "IND"
}
mil <- mil |> mutate(partido = ifelse(is.na(partido), "IND", partido))

# 3. Estimación, dos especificaciones -----------------------------------------
M <- matrix(NA_real_, max(unidades$leg), nrow(items)); M[cbind(d$leg, d$j)] <- d$y
M <- sweep(M, 2, colMeans(M, na.rm = TRUE)); M[is.na(M)] <- 0
x_leg <- as.vector(scale(svd(M, nu = 1, nv = 0)$u[, 1]))
udi_leg <- unidades |> left_join(mil, by = c("senador_id", "t")) |> filter(partido == "UDI") |> pull(leg) |> unique()
x0 <- (sign(mean(x_leg[udi_leg])) * x_leg)[unidades$leg]

mediana_loo <- function(z, grupo) {
  out <- rep(NA_real_, length(z))
  for (g in unique(grupo)) {
    i <- which(grupo == g); if (length(i) < 3) next
    for (k in i) out[k] <- median(z[setdiff(i, k)])
  }
  out
}
bloque_de <- function(p) case_when(p %in% convencional ~ "convencional", p %in% radical ~ "radical", TRUE ~ p)

resultados <- map(c(w01 = 0.1, w1 = 1), function(om) {
  fit <- irt_dinamico(d$u, d$j, d$y, unidades, omega2 = om, x0 = x0, verbose = FALSE)
  message("omega2 = ", om, " | iteraciones: ", fit$iter, " | clasificación: ",
          round(100 * clasificacion(fit, d$u, d$j, d$y), 2), " %")
  pos <- unidades |> mutate(x = fit$x) |> left_join(mil, by = c("senador_id", "t"))
  if (median(pos$x[pos$partido == "UDI"]) < 0) pos$x <- -pos$x
  pos <- pos |> group_by(t) |> mutate(z = (x - mean(x)) / sd(x)) |>
    group_by(t, partido) |> mutate(n_partido = n()) |> ungroup() |>
    mutate(bloque = bloque_de(partido)) |>
    group_by(t) |> mutate(med_part = mediana_loo(z, partido), med_bloque = mediana_loo(z, bloque),
                          dist = z - ifelse(n_partido >= 3, med_part, med_bloque)) |> ungroup()
  # Grupo de referencia: los militantes del partido de origen en el último
  # trimestre con votos antes de la salida (en algunos trimestres de receso
  # ningún senador alcanza diez votos), o su bloque si son menos de tres. La
  # posición relativa de cada miembro es su z menos la mediana de los demás
  # miembros del grupo presentes en el trimestre. El percentil usa el promedio
  # de los cuatro trimestres previos; el cambio compara el promedio de hasta
  # cuatro trimestres posteriores con el de los cuatro previos.
  pct <- salidas |> rowwise() |> mutate(
    tr = max(pos$t[pos$t < ts]),
    ref_grupo = if (sum(pos$t == tr & pos$partido == partido_origen) >= 3) "partido" else "bloque",
    datos = list({
      g <- pos |> filter(t == tr, if (ref_grupo == "partido") partido == partido_origen
                         else bloque == bloque_de(partido_origen), senador_id != parlid) |> pull(senador_id)
      v <- pos |> filter(senador_id %in% c(g, parlid), abs(t - ts) <= 4) |> group_by(t) |>
        mutate(rel = map_dbl(seq_len(n()), \(i) { o <- z[-i][senador_id[-i] %in% g]; if (length(o) < 2) NA_real_ else z[i] - median(o) })) |>
        ungroup()
      pre <- v |> filter(t < ts) |> group_by(senador_id) |> summarise(r = mean(rel, na.rm = TRUE), .groups = "drop") |> filter(!is.na(r))
      yo <- v |> filter(senador_id == parlid)
      list(n_ref = length(g),
           percentil = if (!parlid %in% pre$senador_id || nrow(pre) < 3) NA_real_ else mean(pre$r <= pre$r[pre$senador_id == parlid]),
           cambio = mean(yo$rel[yo$t > ts], na.rm = TRUE) - mean(yo$rel[yo$t < ts], na.rm = TRUE),
           trimestres_post = sum(yo$t > ts))
    }),
    n_ref = datos$n_ref, percentil = datos$percentil, cambio = datos$cambio, trimestres_post = datos$trimestres_post) |>
    ungroup() |> select(-datos)
  list(fit = fit, pos = pos, pct = pct)
})

tabla <- resultados$w01$pct |>
  select(nombre, partido_origen, fecha_salida, tipo, grupo, motivo_principal, ref_grupo, n_ref,
         percentil_w01 = percentil, cambio_w01 = cambio, trimestres_post) |>
  mutate(percentil_w1 = resultados$w1$pct$percentil, cambio_w1 = resultados$w1$pct$cambio)
print(tabla, width = 200)
write_csv(tabla, file.path(dir_out, "tablas", "13_senado_salidas.csv"))
write_csv(bind_rows(w01 = resultados$w01$pos, w1 = resultados$w1$pos, .id = "especificacion"),
          file.path(dir_proc, "posiciones_senado.csv"))

# 4. Prueba de rangos combinada (Cámara y Senado) para salidas hacia la
# derecha radical desde la derecha convencional. La versión principal usa
# solo renuncias voluntarias, como en la Cámara; la segunda agrega las
# suspensiones de militancia.
camara <- read_csv(file.path(dir_out, "tablas", "08_matriz_pruebas.csv"), show_col_types = FALSE) |>
  filter(grupo == "Hacia la derecha radical")
combinada <- map_dfr(c("solo renuncias", "con suspensiones"), function(v) map_dfr(c("w01", "w1"), function(e) {
  col <- paste0("percentil_", e)
  sen <- tabla |> filter(grupo == "Hacia la derecha radical", v == "con suspensiones" | tipo == "renuncia")
  p <- c(camara[[col]], sen[[col]]); p <- p[!is.na(p)]
  tibble(version = v, especificacion = e, n = length(p), n_senado = sum(!is.na(sen[[col]])), percentil_medio = mean(p),
         p_derecha = mean(replicate(1e5, mean(runif(length(p)))) >= mean(p)))
}))
print(combinada)
write_csv(combinada, file.path(dir_out, "tablas", "13_rangos_camara_senado.csv"))

resumen <- tibble(votos = nrow(d), votaciones = nrow(items), senadores = n_distinct(d$senador_id),
                  unidades = nrow(unidades),
                  clas_w01 = clasificacion(resultados$w01$fit, d$u, d$j, d$y),
                  clas_w1 = clasificacion(resultados$w1$fit, d$u, d$j, d$y))
write_csv(resumen, file.path(dir_out, "tablas", "13_senado_resumen.csv"))
