# 04_riesgo_salida.R
# Riesgo de salida de diputados militantes de partidos de derecha, 2014–2026.
# Modelo logit de tiempo discreto (trimestres) con efectos fijos de período.

source(file.path("R", "00_funciones.R"))

derecha      <- c("UDI", "RN", "EVOP", "AMPL", "PREP", "PNL", "PSC", "PCC")
convencional <- c("UDI", "RN", "EVOP", "AMPL")
radical      <- c("PREP", "PNL", "PSC", "PCC")

sufijo <- if (exists("sufijo_salida")) sufijo_salida else ""
panel   <- read_csv(file.path(dir_proc, paste0("panel_trimestral", sufijo, ".csv")), show_col_types = FALSE)
salidas <- read_csv(file.path(dir_proc, "salidas_verificadas.csv"), show_col_types = FALSE) |>
  mutate(t_salida = trimestre(fecha_salida))

# Períodos legislativos y límite a la reelección (Ley 21.238, junio de 2020:
# los diputados pueden reelegirse sucesivamente hasta por dos períodos, con
# cómputo retroactivo)
reg <- read_delim(file.path(dir_fuentes, "militancias_diputados_2010_2030.csv"), ";",
                  show_col_types = FALSE) |> distinct(id, periodo)
inicio_periodo <- c("2010-2014" = 2010, "2014-2018" = 2014, "2018-2022" = 2018,
                    "2022-2026" = 2022, "2026-2030" = 2026)
reg <- reg |> mutate(a = inicio_periodo[periodo]) |> arrange(id, a) |>
  group_by(id) |>
  mutate(consecutivos = {
    k <- integer(n()); for (s in seq_len(n())) k[s] <- if (s > 1 && a[s] - a[s - 1] == 4) k[s - 1] + 1L else 1L; k
  }) |> ungroup()

periodo_de <- function(t) cut(t, c(-Inf, 17, 33, 49, 65, Inf),
                              labels = c("2010-2014", "2014-2018", "2018-2022", "2022-2026", "2026-2030"))
panel <- panel |>
  mutate(periodo = as.character(periodo_de(t - 1 + 1)))  # t=1 es 2014T1
# 2014T1 pertenece aún al período 2010-2014; desde 2014T2 (t=2) rige 2014-2018
panel <- panel |>
  mutate(periodo = case_when(t <= 1 ~ "2010-2014", t <= 17 ~ "2014-2018", t <= 33 ~ "2018-2022",
                             t <= 49 ~ "2022-2026", TRUE ~ "2026-2030")) |>
  left_join(reg |> select(id, periodo, consecutivos), by = c("diputado_id" = "id", "periodo")) |>
  mutate(fecha_ref = as.Date(paste0(2014 + (t - 1) %/% 4, "-", sprintf("%02d", ((t - 1) %% 4) * 3 + 1), "-01")),
         sin_reeleccion = as.integer(consecutivos >= 3 & fecha_ref >= as.Date("2020-07-01")))

# Panel en riesgo: militante de un partido de derecha al inicio del trimestre,
# con posición observada en el trimestre anterior
panel <- panel |> arrange(diputado_id, t) |>
  group_by(diputado_id) |>
  mutate(dist_l = ifelse(lag(t) == t - 1, lag(dist), NA_real_),
         # promedio de t-4 a t-2: excluye el trimestre inmediatamente anterior
         dist_l24 = sapply(seq_along(t), function(k) {
           v <- dist[t %in% (t[k] - 4):(t[k] - 2)]; if (length(v) == 0) NA_real_ else mean(v, na.rm = TRUE) }),
         z_l    = ifelse(lag(t) == t - 1, lag(z), NA_real_),
         partido_l = ifelse(lag(t) == t - 1, lag(partido), NA_character_)) |>
  ungroup()

riesgo <- panel |>
  filter(partido_l %in% derecha, !is.na(dist_l)) |>
  left_join(salidas |> select(id, t_salida, partido_origen, destino, destino_bloque),
            by = c("diputado_id" = "id", "t" = "t_salida", "partido_l" = "partido_origen")) |>
  mutate(salida = as.integer(!is.na(destino)),
         hacia_radical = as.integer(salida == 1 & partido_l %in% convencional & destino_bloque == "radical"),
         dist_der = pmax(dist_l, 0), dist_izq = pmax(-dist_l, 0),
         dist_der24 = pmax(dist_l24, 0), dist_izq24 = pmax(-dist_l24, 0),
         tipo_partido = ifelse(partido_l %in% radical, "radical", "convencional"))

# Control: todas las salidas verificadas deben estar en el panel en riesgo
faltan <- anti_join(salidas, riesgo |> filter(salida == 1), by = c("id" = "diputado_id", "t_salida" = "t"))
if (nrow(faltan) > 0) { message("Salidas fuera del panel en riesgo:"); print(faltan |> select(nombre, fecha_salida)) }

message("Diputado-trimestres en riesgo: ", nrow(riesgo), " | diputados: ", n_distinct(riesgo$diputado_id),
        " | salidas: ", sum(riesgo$salida), " | hacia la derecha radical: ", sum(riesgo$hacia_radical))
# Variables del distrito (desde 2018: demanda de derecha radical y votación personal)
vd <- read_csv(file.path(dir_proc, "variables_distrito.csv"), show_col_types = FALSE)
riesgo <- riesgo |> left_join(vd, by = c("diputado_id" = "id", "periodo")) |>
  mutate(demanda_c = demanda_rad - ave(demanda_rad, periodo, FUN = function(v) mean(v, na.rm = TRUE)))
write_csv(riesgo, file.path(dir_proc, paste0("panel_riesgo", sufijo, ".csv")))

# Modelos -------------------------------------------------------------------
cl <- riesgo$diputado_id
# Derecha en el gobierno: Piñera (2018–2022) y Kast (desde marzo de 2026)
riesgo <- riesgo |> mutate(gobierno = as.integer(periodo %in% c("2018-2022", "2026-2030")))
conv <- riesgo |> filter(tipo_partido == "convencional")
clc <- conv$diputado_id
modelos <- list(
  m1_toda_salida   = glm(salida ~ dist_der + dist_izq + periodo, binomial("cloglog"), riesgo),
  m2_controles     = glm(salida ~ dist_der + dist_izq + sin_reeleccion + tipo_partido + periodo, binomial("cloglog"), riesgo),
  m3_hacia_radical = glm(hacia_radical ~ dist_der + dist_izq + periodo, binomial("cloglog"), conv),
  m4_otras_conv    = glm(I(salida == 1 & hacia_radical == 0) ~ dist_der + dist_izq + periodo, binomial("cloglog"), conv),
  m5_radical_rez   = glm(hacia_radical ~ dist_der24 + dist_izq24 + periodo, binomial("cloglog"), conv),
  m6_radical_logit = glm(hacia_radical ~ dist_der + dist_izq + periodo, binomial("logit"), conv),
  m7_radical_distrito = glm(hacia_radical ~ dist_der + dist_izq + I(demanda_c / 10) + I(cuota_propia / 10) + sin_reeleccion + periodo,
                            binomial("cloglog"), conv |> filter(periodo != "2014-2018")),
  m8_radical_gobierno = glm(hacia_radical ~ dist_der + dist_der:gobierno + dist_izq + periodo, binomial("cloglog"), conv)
)
tablas <- imap_dfr(modelos, function(m, nm) {
  d <- m$model
  base <- if (nm %in% c("m1_toda_salida", "m2_controles")) riesgo else if (nm == "m7_radical_distrito") conv |> filter(periodo != "2014-2018") else conv
  ids <- base$diputado_id[as.integer(rownames(d))]
  m$data$.row <- NULL
  tabla_glm(m, setNames(ids, rownames(d))) |>
    mutate(modelo = nm, n = nrow(d), eventos = sum(d[[1]]))
})
# tabla_glm usa los nombres de fila del modelo para ubicar el grupo
print(tablas |> filter(!grepl("periodo|Intercept", termino)) |> mutate(across(where(is.numeric), \(v) round(v, 3))), n = 40)
write_csv(tablas, file.path(dir_out, "tablas", paste0("04_modelos_riesgo", sufijo, ".csv")))
saveRDS(modelos, file.path(dir_proc, paste0("modelos_riesgo", sufijo, ".rds")))
