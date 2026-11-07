# 06_distritos.R
# Variables electorales por diputado y período: distrito, votación personal y
# demanda de derecha radical en el distrito (primera vuelta presidencial).
#
# Demanda de derecha radical: porcentaje de Kast en la primera vuelta de 2017
# (período 2018–2022) y de 2021 (2022–2026), y de Kast más Kaiser en 2025
# (2026–2030), sobre los votos válidamente emitidos por candidatos.

source(file.path("R", "00_funciones.R"))
suppressPackageStartupMessages(library(readxl))

normalizar <- function(x) {
  x |> stringi::stri_trans_general("Latin-ASCII") |> toupper() |>
    str_replace_all("[^A-Z ]", " ") |> str_squish()
}

# 1. Presidenciales por distrito --------------------------------------------
archivos <- c("2017" = "2017_11_Presidencial_Datos_Eleccion.xlsx",
              "2021" = "2021_11_Presidencial_1V_Datos_Eleccion.xlsx",
              "2025" = "2025_11_Presidencial_1V_Datos_Eleccion_v2.xlsx")
pres <- imap_dfr(archivos, function(f, anio) {
  x <- read_excel(file.path("data", "raw", "presidencial", f), sheet = 2, skip = 5, col_names = FALSE)
  x <- x[, c(4, 5, 6, 8, 10)]
  names(x) <- c("distrito", "comuna", "nro", "apellido", "votos")
  x |> filter(!is.na(nro), grepl("^[0-9]+$", votos)) |>
    mutate(anio = as.integer(anio), votos = as.numeric(votos),
           distrito = as.integer(str_extract(distrito, "[0-9]+")), apellido = normalizar(apellido))
})
demanda <- pres |>
  group_by(anio, distrito) |>
  summarise(validos = sum(votos),
            rad = sum(votos[apellido %in% c("KAST", "KAISER")]),
            .groups = "drop") |>
  mutate(demanda_rad = 100 * rad / validos)
print(demanda |> group_by(anio) |> summarise(nacional = 100 * sum(rad) / sum(validos), min = min(demanda_rad), max = max(demanda_rad)))
write_csv(demanda, file.path(dir_proc, "demanda_derecha_radical_distrito.csv"))

# 2. Distrito y votación personal de cada diputado electo -------------------
electos <- read_csv(file.path(dir_fuentes, "electos_diputados_servel.csv"), show_col_types = FALSE)
cand_all <- readRDS(file.path("data", "fuentes", "candidatos_diputados_servel.rds"))
cuotas <- cand_all |> group_by(anio, unidad) |> mutate(cuota_propia = 100 * votos / sum(votos)) |>
  ungroup() |> filter(electo) |> select(anio, unidad, candidato, cuota_propia)
electos <- electos |> left_join(cuotas, by = c("anio", "unidad", "candidato"))

reg <- read_delim(file.path(dir_fuentes, "militancias_diputados_2010_2030.csv"), ";", show_col_types = FALSE) |>
  distinct(periodo, id, nombre)
anio_eleccion <- c("2014-2018" = 2013, "2018-2022" = 2017, "2022-2026" = 2021, "2026-2030" = 2025)
reg <- reg |> filter(periodo %in% names(anio_eleccion)) |> mutate(anio = anio_eleccion[periodo])

tok <- function(x) strsplit(normalizar(x), " ")[[1]]
emparejar <- function(nombre, anio_) {
  t <- tok(nombre); c <- electos[electos$anio == anio_, ]
  s <- vapply(c$candidato, function(k) { u <- tok(k); length(intersect(t, u)) / min(length(t), length(u)) }, 0)
  ap_ok <- vapply(c$candidato, function(k) any(tail(t, 2) %in% tok(k)), TRUE)
  s[!ap_ok] <- 0
  if (max(s) >= 0.66 && sum(s == max(s)) == 1) c$candidato[which.max(s)] else NA_character_
}
reg$candidato <- mapply(emparejar, reg$nombre, reg$anio)
dip <- reg |>
  left_join(electos |> select(anio, candidato, unidad, cuota_propia), by = c("anio", "candidato")) |>
  mutate(distrito = as.integer(str_extract(unidad, "[0-9]+")))
print(dip |> group_by(periodo) |> summarise(n = n(), con_distrito = sum(!is.na(distrito))))

# 3. Unión con la demanda del distrito (período siguiente a la elección presidencial)
anio_pres <- c("2018-2022" = 2017, "2022-2026" = 2021, "2026-2030" = 2025)
dip <- dip |>
  mutate(anio_pres = anio_pres[periodo]) |>
  left_join(demanda |> select(anio_pres = anio, distrito, demanda_rad), by = c("anio_pres", "distrito")) |>
  select(periodo, id, distrito, cuota_propia, demanda_rad)
write_csv(dip, file.path(dir_proc, "variables_distrito.csv"))
