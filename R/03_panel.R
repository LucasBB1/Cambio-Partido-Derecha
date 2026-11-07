# 03_panel.R
# Militancia corregida por trimestre, posiciones relativas al partido y
# panel de riesgo de salida (diputados militantes de partidos de derecha).

source(file.path("R", "00_funciones.R"))

derecha <- c("UDI", "RN", "EVOP", "AMPL", "PREP", "PNL", "PSC", "PCC")

# 1. Episodios de militancia del registro de la Cámara ----------------------
reg <- read_delim(file.path(dir_fuentes, "militancias_diputados_2010_2030.csv"), ";",
                  show_col_types = FALSE)
corr <- read_delim(file.path(dir_fuentes, "correcciones_militancia_camara.csv"), ";",
                   show_col_types = FALSE)
episodios <- reg |>
  distinct(id, nombre, partido = partido_id, inicio = mil_inicio, fin = mil_termino) |>
  filter(!id %in% corr$id) |>
  bind_rows(corr |> transmute(id, nombre, partido, inicio = mil_inicio, fin = mil_termino)) |>
  arrange(id, inicio)

# 2. Salidas verificadas ----------------------------------------------------
salidas <- read_delim(file.path(dir_fuentes, "salidas_partido.csv"), ";",
                      show_col_types = FALSE) |>
  mutate(fecha_salida = as.Date(fecha_salida))

# Fecha en que el registro de la Cámara anota la salida (primer episodio
# posterior al partido de origen)
salidas <- salidas |>
  left_join(episodios |> select(id, partido, fin), by = c("id", "partido_origen" = "partido"),
            relationship = "many-to-many") |>
  group_by(id, fecha_salida) |>
  slice_min(abs(as.numeric(fin - fecha_salida)), n = 1, with_ties = FALSE) |>
  ungroup() |>
  rename(fecha_registro = fin)

# 3. Partido de cada diputado en cada trimestre -----------------------------
sufijo <- if (exists("sufijo_salida")) sufijo_salida else ""
pos <- read_csv(file.path(dir_proc, paste0("posiciones_trimestre", sufijo, ".csv")), show_col_types = FALSE)
pos <- pos |> mutate(fecha_ref = as.Date(paste0(2014 + (t - 1) %/% 4, "-",
                                                sprintf("%02d", ((t - 1) %% 4) * 3 + 1), "-01")))

partido_reg <- pos |>
  select(diputado_id, t, fecha_ref) |>
  left_join(episodios, by = c("diputado_id" = "id"), relationship = "many-to-many") |>
  filter(fecha_ref >= inicio, fecha_ref <= fin) |>
  group_by(diputado_id, t) |> slice_max(inicio, n = 1, with_ties = FALSE) |> ungroup() |>
  select(diputado_id, t, partido_reg = partido)

pos <- pos |> left_join(partido_reg, by = c("diputado_id", "t")) |>
  mutate(partido = coalesce(partido_reg, "IND"))

# Correcciones con las salidas verificadas: después de la fecha de salida,
# el diputado ya no figura en el partido de origen aunque el registro lo
# mantenga hasta que la Cámara anota el cambio.
for (k in seq_len(nrow(salidas))) {
  s <- salidas[k, ]
  idx <- pos$diputado_id == s$id & pos$fecha_ref > s$fecha_salida & pos$partido == s$partido_origen
  pos$partido[idx] <- "IND"
}
# De la Carrera renunció al PREP en diciembre de 2021 y asumió como
# independiente (reseña BCN); el registro lo anota en el PREP hasta 2022.
pos$partido[pos$diputado_id == 1123 & pos$partido == "PREP"] <- "IND"

# 4. Posición estandarizada por trimestre y distancia al propio partido -----
pos <- pos |>
  group_by(t) |>
  mutate(z = (x - mean(x)) / sd(x)) |>
  ungroup()

# Mediana del partido sin el propio diputado
mediana_loo <- function(z, grupo) {
  out <- rep(NA_real_, length(z))
  for (g in unique(grupo)) {
    i <- which(grupo == g)
    if (length(i) < 3) next
    for (k in i) out[k] <- median(z[setdiff(i, k)])
  }
  out
}
# Partidos con menos de tres diputados en el trimestre se comparan con su
# bloque (derecha convencional o derecha radical)
bloque_de <- function(p) case_when(p %in% c("UDI", "RN", "EVOP", "AMPL") ~ "convencional",
                                   p %in% c("PREP", "PNL", "PSC", "PCC") ~ "radical",
                                   TRUE ~ p)
pos <- pos |>
  group_by(t, partido) |> mutate(n_partido = n()) |> ungroup() |>
  mutate(bloque = bloque_de(partido)) |>
  group_by(t) |>
  mutate(med_part = mediana_loo(z, partido),
         med_bloque = mediana_loo(z, bloque),
         mediana_partido = ifelse(n_partido >= 3, med_part, med_bloque),
         mediana_derecha = mediana_loo(z, ifelse(partido %in% derecha, "D", "O"))) |>
  ungroup() |>
  mutate(dist = z - mediana_partido)

write_csv(pos |> select(diputado_id, t, x, z, partido, mediana_partido, dist, mediana_derecha, n_votos),
          file.path(dir_proc, paste0("panel_trimestral", sufijo, ".csv")))
write_csv(salidas, file.path(dir_proc, "salidas_verificadas.csv"))

# 5. Comparación de fechas: anuncio público frente al registro --------------
desfase <- salidas |>
  mutate(dias = as.numeric(fecha_registro - fecha_salida)) |>
  select(nombre, partido_origen, fecha_salida, fecha_registro, dias)
print(desfase, n = 50)
message("Mediana de desfase (días): ", median(abs(desfase$dias), na.rm = TRUE))
write_csv(desfase, file.path(dir_out, "tablas", "03_desfase_registro.csv"))
