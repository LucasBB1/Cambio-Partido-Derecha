# 02_puntos_ideales.R
# Posiciones de cada diputado por trimestre (2014–2026) a partir de las
# votaciones nominales de Sala, con un modelo IRT probit dinámico.

source(file.path("R", "00_funciones.R"))

args_tipo <- if (exists("tipos_incluidos")) tipos_incluidos else "Proyecto de Ley"
omega2    <- if (exists("omega2_usado")) omega2_usado else 0.1
sufijo    <- if (exists("sufijo_salida")) sufijo_salida else ""

vt <- read_csv(file.path(dir_proc, "votaciones.csv"), show_col_types = FALSE) |>
  mutate(fecha = as.Date(substr(as.character(fecha), 1, 10)))
vo <- read_csv(file.path(dir_proc, "votos.csv.gz"), show_col_types = FALSE)

# Votos nominales: sí = 1, no = 0; abstención, dispensado y no vota = ausente
d <- vo |>
  filter(opcion_valor %in% c(0, 1)) |>
  inner_join(vt |> filter(tipo %in% args_tipo, fecha >= as.Date("2014-03-11")) |>
               select(votacion_id, fecha), by = "votacion_id") |>
  transmute(votacion_id, diputado_id, y = as.integer(opcion_valor), t = trimestre(fecha))

# Depuración iterativa: votaciones con minoría < 2,5 % y unidades
# diputado-trimestre con menos de 20 votos
repeat {
  n0 <- nrow(d)
  d <- d |>
    group_by(votacion_id) |> filter(min(mean(y), 1 - mean(y)) >= 0.025) |> ungroup() |>
    group_by(diputado_id, t) |> filter(n() >= 20) |> ungroup()
  if (nrow(d) == n0) break
}

unidades <- d |> distinct(diputado_id, t) |> arrange(diputado_id, t) |>
  mutate(u = row_number(), leg = as.integer(factor(diputado_id)))
items <- d |> distinct(votacion_id) |> arrange(votacion_id) |> mutate(j = row_number())
d <- d |> left_join(unidades, by = c("diputado_id", "t")) |> left_join(items, by = "votacion_id")
message("Votos: ", nrow(d), " | votaciones: ", nrow(items), " | diputados: ",
        n_distinct(d$diputado_id), " | unidades diputado-trimestre: ", nrow(unidades))

# Valores iniciales: primer componente de la matriz diputado x votación
M <- matrix(NA_real_, max(unidades$leg), nrow(items))
M[cbind(d$leg, d$j)] <- d$y
M <- sweep(M, 2, colMeans(M, na.rm = TRUE)); M[is.na(M)] <- 0
x_leg <- svd(M, nu = 1, nv = 0)$u[, 1]
x_leg <- as.vector(scale(x_leg))

# Orientación: la UDI a la derecha (positivo)
milit <- read_delim(file.path(dir_fuentes, "militancias_diputados_2010_2030.csv"), ";", show_col_types = FALSE)
udi <- unique(milit$id[milit$partido_id == "UDI"])
signo <- sign(mean(x_leg[unidades$leg[match(udi, unidades$diputado_id)]], na.rm = TRUE))
x0 <- (signo * x_leg)[unidades$leg]

# Si existe una estimación previa con las mismas unidades, se parte de ella
previo <- file.path(dir_proc, paste0("puntos_ideales", sufijo, ".rds"))
if (file.exists(previo)) {
  ant <- readRDS(previo)
  if (identical(ant$unidades$u, unidades$u) && identical(ant$unidades$diputado_id, unidades$diputado_id)) x0 <- ant$fit$x
}
fit <- irt_dinamico(d$u, d$j, d$y, unidades, omega2 = omega2, x0 = x0)
message("Iteraciones: ", fit$iter, " | convergió: ", fit$convergio)
message("Votos clasificados correctamente: ", round(100 * clasificacion(fit, d$u, d$j, d$y), 1), " %")

posiciones <- unidades |> mutate(x = fit$x) |>
  left_join(d |> count(u, name = "n_votos"), by = "u") |>
  select(diputado_id, t, x, n_votos)
items_est <- items |> mutate(a = fit$a, b = fit$b)

saveRDS(list(posiciones = posiciones, items = items_est, fit = fit, d = d, unidades = unidades, omega2 = omega2),
        file.path(dir_proc, paste0("puntos_ideales", sufijo, ".rds")))
write_csv(posiciones, file.path(dir_proc, paste0("posiciones_trimestre", sufijo, ".csv")))
