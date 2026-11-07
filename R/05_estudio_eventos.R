# 05_estudio_eventos.R
# Trayectoria del voto de quienes salen de su partido, antes y después de la
# salida, frente a colegas del mismo partido que permanecen (placebo).
# Dos medidas: posición relativa a la mediana del partido de origen y tasa
# de acuerdo con la mayoría del partido de origen.

source(file.path("R", "00_funciones.R"))
set.seed(20261106)

convencional <- c("UDI", "RN", "EVOP", "AMPL")
radical      <- c("PREP", "PNL", "PSC", "PCC")
ventana      <- -8:8

sufijo <- if (exists("sufijo_salida")) sufijo_salida else ""
panel   <- read_csv(file.path(dir_proc, paste0("panel_trimestral", sufijo, ".csv")), show_col_types = FALSE)
salidas <- read_csv(file.path(dir_proc, "salidas_verificadas.csv"), show_col_types = FALSE) |>
  mutate(t_salida = trimestre(fecha_salida),
         grupo = case_when(partido_origen %in% convencional & destino_bloque == "radical" ~ "Hacia la derecha radical",
                           partido_origen %in% convencional ~ "Otras salidas de la derecha convencional",
                           TRUE ~ "Salidas de partidos de derecha radical"))

# 1. Mediana de cada partido por trimestre (todos sus militantes) ------------
med <- panel |> group_by(t, partido) |> summarise(med = median(z), n_mil = n(), .groups = "drop")

# 2. Acuerdo con la mayoría de cada partido, votación por votación ----------
pi <- readRDS(file.path(dir_proc, paste0("puntos_ideales", sufijo, ".rds")))
d <- pi$d |> select(votacion_id, diputado_id, y, t) |>
  left_join(panel |> select(diputado_id, t, partido), by = c("diputado_id", "t"))
conteo <- d |> group_by(votacion_id, partido) |> summarise(si = sum(y), tot = n(), .groups = "drop")

acuerdo_con <- function(dip, partido_ref) {
  # acuerdo del diputado `dip` con la mayoría de `partido_ref` (sin su propio voto)
  v <- d |> filter(diputado_id == dip) |>
    left_join(conteo |> filter(partido == partido_ref) |> select(votacion_id, si, tot), by = "votacion_id") |>
    mutate(propio = partido == partido_ref,
           si_o = si - ifelse(propio, y, 0), tot_o = tot - ifelse(propio, 1, 0)) |>
    filter(!is.na(tot_o), tot_o >= 2, si_o * 2 != tot_o) |>
    mutate(mayoria = as.integer(si_o * 2 > tot_o), acuerdo = as.integer(y == mayoria))
  v |> group_by(t) |> summarise(acuerdo = mean(acuerdo), n = n(), .groups = "drop")
}

# 3. Trayectorias en tiempo relativo a la salida ----------------------------
trayectoria <- function(dip, origen, t0) {
  p <- panel |> filter(diputado_id == dip, (t - t0) %in% ventana) |>
    left_join(med |> filter(partido == origen) |> select(t, med), by = "t") |>
    mutate(k = t - t0, rel = z - ifelse(partido == origen, mediana_partido, med))
  a <- acuerdo_con(dip, origen) |> mutate(k = t - t0) |> filter(k %in% ventana)
  p |> select(t, k, rel) |> full_join(a |> select(t, k, acuerdo, n), by = c("t", "k"))
}

tray <- salidas |>
  mutate(datos = pmap(list(id, partido_origen, t_salida), trayectoria)) |>
  select(id, nombre, grupo, partido_origen, destino, t_salida, datos) |>
  unnest(datos)
write_csv(tray, file.path(dir_proc, paste0("trayectorias_salidas", sufijo, ".csv")))

# Normalización: diferencia respecto del promedio de k = -8 a -4 (línea base)
normalizar <- function(df, var) {
  df |> group_by(id, t_salida) |>
    mutate(base = mean(.data[[var]][k >= -8 & k <= -4], na.rm = TRUE),
           valor = .data[[var]] - base) |> ungroup()
}

# 4. Placebo: militantes del mismo partido que no salen, con la misma fecha -
salieron <- unique(salidas$id)
placebo_una <- function(var) {
  salidas |>
    mutate(dip = map2_dbl(partido_origen, t_salida, function(p0, t0) {
      cand <- panel |> filter(partido == p0, t == t0 - 1, !diputado_id %in% salieron) |> pull(diputado_id)
      if (length(cand) == 0) NA_real_ else cand[sample.int(length(cand), 1)]
    })) |>
    filter(!is.na(dip)) |>
    mutate(datos = pmap(list(dip, partido_origen, t_salida), function(a, b, c) {
      p <- panel |> filter(diputado_id == a, (t - c) %in% ventana) |>
        mutate(k = t - c, rel = z - mediana_partido)
      p |> select(k, rel)
    })) |>
    select(id = dip, t_salida, grupo, datos) |> unnest(datos)
}

resumen <- function(df, var) {
  normalizar(df, var) |> filter(!is.na(valor)) |>
    group_by(grupo, k) |> summarise(media = mean(valor), n = n(), .groups = "drop")
}
obs_rel <- resumen(tray, "rel")
B <- 500
plac <- map_dfr(seq_len(B), function(b) resumen(placebo_una("rel"), "rel") |> mutate(b = b))
banda <- plac |> group_by(grupo, k) |>
  summarise(p025 = quantile(media, .025), p975 = quantile(media, .975), .groups = "drop")
evento_rel <- obs_rel |> left_join(banda, by = c("grupo", "k"))
write_csv(evento_rel, file.path(dir_out, "tablas", paste0("05_evento_posicion", sufijo, ".csv")))
print(evento_rel |> mutate(across(where(is.numeric), \(v) round(v, 3))), n = 60)

obs_ac <- resumen(tray, "acuerdo")
write_csv(obs_ac, file.path(dir_out, "tablas", paste0("05_evento_acuerdo", sufijo, ".csv")))
print(obs_ac |> mutate(across(where(is.numeric), \(v) round(v, 3))), n = 60)

# 5. Deriva previa y salto, caso por caso ----------------------------------
casos <- tray |> group_by(id, nombre, grupo, partido_origen, destino, t_salida) |>
  summarise(base   = mean(rel[k >= -8 & k <= -4], na.rm = TRUE),
            previo = mean(rel[k >= -2 & k <= -1], na.rm = TRUE),
            post   = mean(rel[k >= 1 & k <= 4], na.rm = TRUE),
            ac_base = mean(acuerdo[k >= -8 & k <= -4], na.rm = TRUE),
            ac_prev = mean(acuerdo[k >= -2 & k <= -1], na.rm = TRUE),
            ac_post = mean(acuerdo[k >= 1 & k <= 4], na.rm = TRUE),
            .groups = "drop") |>
  mutate(deriva = previo - base, salto = post - previo)
write_csv(casos, file.path(dir_out, "tablas", paste0("05_casos_deriva_salto", sufijo, ".csv")))
print(casos |> select(nombre, grupo, deriva, salto, ac_base, ac_prev, ac_post) |>
        mutate(across(where(is.numeric), \(v) round(v, 2))), n = 30, width = 200)
