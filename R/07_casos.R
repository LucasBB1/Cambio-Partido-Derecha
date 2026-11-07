# 07_casos.R
# Universo de casos para el análisis cualitativo: todas las salidas (casos
# positivos) y casos negativos emparejados (militantes del mismo partido, en
# el mismo trimestre, con la posición previa más cercana, que no salen).
# Para todos: candidatura en la elección siguiente (SERVEL).

source(file.path("R", "00_funciones.R"))

convencional <- c("UDI", "RN", "EVOP", "AMPL")
radical      <- c("PREP", "PNL", "PSC", "PCC")

panel   <- read_csv(file.path(dir_proc, "panel_trimestral.csv"), show_col_types = FALSE)
salidas <- read_csv(file.path(dir_proc, "salidas_verificadas.csv"), show_col_types = FALSE) |>
  mutate(ts = trimestre(fecha_salida),
         grupo = case_when(partido_origen %in% convencional & destino_bloque == "radical" ~ "hacia_radical",
                           partido_origen %in% convencional ~ "otras_convencional",
                           TRUE ~ "desde_radical"))
reg <- read_delim(file.path(dir_fuentes, "militancias_diputados_2010_2030.csv"), ";", show_col_types = FALSE)
nombres <- reg |> distinct(id, nombre)
periodos <- reg |> distinct(id, periodo)
periodo_de <- function(t) case_when(t <= 1 ~ "2010-2014", t <= 17 ~ "2014-2018", t <= 33 ~ "2018-2022",
                                    t <= 49 ~ "2022-2026", TRUE ~ "2026-2030")

prom_previo <- function(ids, ts) {
  panel |> filter(diputado_id %in% ids, t >= ts - 4, t <= ts - 1) |>
    group_by(diputado_id) |> summarise(d = mean(dist, na.rm = TRUE), .groups = "drop")
}

# Casos negativos: uno por cada salida hacia la derecha radical y por cada
# salida disciplinaria o de otro tipo desde la derecha convencional
salieron_periodo <- salidas |> mutate(periodo = periodo_de(ts)) |> distinct(id, periodo)
negativos <- salidas |> filter(grupo != "desde_radical") |> rowwise() |>
  mutate(neg = {
    per <- periodo_de(ts)
    mem <- panel |> filter(partido == partido_origen, t == ts - 1) |> pull(diputado_id)
    excl <- unique(salidas$id)  # nunca se usa como negativo a quien sale en algún período
    a <- prom_previo(unique(c(mem, id)), ts)
    d0 <- a$d[a$diputado_id == id]
    cand <- a |> filter(!diputado_id %in% excl)
    if (length(d0) == 0 || nrow(cand) == 0) NA_real_ else cand$diputado_id[which.min(abs(cand$d - d0))]
  }) |> ungroup() |>
  select(caso_positivo = nombre, id_positivo = id, grupo, partido_origen, ts, neg) |>
  left_join(nombres, by = c("neg" = "id")) |> rename(nombre_negativo = nombre)
print(negativos |> mutate(trimestre = etiqueta_trimestre(ts)) |> select(caso_positivo, partido_origen, trimestre, nombre_negativo), n = 30)
write_csv(negativos, file.path(dir_out, "tablas", "07_casos_negativos.csv"))

# Candidatura en la primera elección posterior a la salida ------------------------------------
cand <- readRDS(file.path(dir_fuentes, "candidatos_servel.rds"))
normalizar <- function(x) x |> stringi::stri_trans_general("Latin-ASCII") |> toupper() |>
  str_replace_all("[^A-Z ]", " ") |> str_squish()
tok <- function(x) strsplit(normalizar(x), " ")[[1]]
elecciones <- as.Date(c("2017-11-19", "2021-11-21", "2025-11-16"))
proxima_eleccion <- function(f) {
  e <- elecciones[elecciones > f]
  if (length(e) == 0) NA_integer_ else as.integer(format(min(e), "%Y"))
}
buscar <- function(nombre, anio_) {
  if (is.na(anio_)) return(tibble(camara_sig = NA, sigla_sig = NA, condicion_sig = NA, electo_sig = NA))
  t <- tok(nombre); c <- cand[cand$anio == anio_, ]
  # exige el primer nombre y los dos apellidos en el mismo orden
  ap <- paste(tail(t, 2), collapse = " ")
  ok <- vapply(c$candidato, function(k) { n <- normalizar(k); grepl(ap, n, fixed = TRUE) && t[1] %in% tok(k) }, TRUE)
  # nombre con el que figura en el SERVEL cuando difiere del registro de la Cámara
  alias <- c("JOSE MANUEL EDWARDS SILVA" = "ROJO EDWARDS SILVA")
  if (!any(ok) && normalizar(nombre) %in% names(alias)) ok <- normalizar(c$candidato) == alias[normalizar(nombre)]
  s <- as.numeric(ok)
  if (max(s) < 1) return(tibble(camara_sig = "no compite", sigla_sig = NA, condicion_sig = NA, electo_sig = NA))
  k <- which(s == max(s))[1]
  tibble(camara_sig = c$camara[k], sigla_sig = c$sigla[k], condicion_sig = c$condicion[k], electo_sig = c$electo[k])
}
casos <- bind_rows(
  salidas |> transmute(tipo = "positivo", grupo, id, nombre, partido_origen, destino, ts, fecha = fecha_salida),
  negativos |> filter(!is.na(neg)) |>
    left_join(salidas |> select(id, ts, fecha = fecha_salida), by = c("id_positivo" = "id", "ts")) |>
    transmute(tipo = "negativo", grupo, id = neg, nombre = nombre_negativo,
              partido_origen, destino = NA_character_, ts, fecha)
) |>
  mutate(periodo = periodo_de(ts), anio_sig = map_int(fecha, proxima_eleccion)) |>
  mutate(sig = map2(nombre, anio_sig, buscar)) |> unnest(sig)
print(casos |> select(tipo, nombre, partido_origen, destino, anio_sig, camara_sig, sigla_sig, condicion_sig, electo_sig), n = 60, width = 220)
write_csv(casos, file.path(dir_out, "tablas", "07_casos_candidatura_siguiente.csv"))
