# 01_descargar_votaciones.R
# Descarga las votaciones de Sala de la Cámara de Diputadas y Diputados
# (2014–2026) desde los datos abiertos y las deja en formato largo.
#
# Fuente: https://opendata.camara.cl/ (WSLegislativo.asmx)
#   retornarVotacionesXAnno?prmAnno=AAAA     lista de votaciones del año
#   retornarVotacionDetalle?prmVotacionId=ID voto de cada diputado
#
# Se puede interrumpir y volver a correr: guarda el XML original de cada
# votación en data/raw/votaciones/detalle_AAAA.rds y retoma donde quedó.
#
# Productos:
#   data/processed/votaciones.csv          una fila por votación
#   data/processed/votos.csv.gz            una fila por diputado y votación
#   data/processed/diputados_votaciones.csv  Id y nombre de cada diputado
#   descarga_resumen.txt                   control de la descarga

paquetes <- c("httr", "xml2", "dplyr", "purrr", "readr")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)
library(httr)
library(xml2)
library(dplyr)
library(purrr)
library(readr)

annos <- 2014:2026
base  <- "https://opendata.camara.cl/camaradiputados/WServices/WSLegislativo.asmx"
dir_raw  <- file.path("data", "raw", "votaciones")
dir_proc <- file.path("data", "processed")
dir.create(dir_raw,  recursive = TRUE, showWarnings = FALSE)
dir.create(dir_proc, recursive = TRUE, showWarnings = FALSE)

# Descarga con reintentos; devuelve el XML como texto o NA
traer <- function(url, intentos = 4) {
  for (i in seq_len(intentos)) {
    r <- tryCatch(GET(url, user_agent("R (investigacion academica)"), timeout(90)),
                  error = function(e) NULL)
    if (!is.null(r) && status_code(r) == 200) {
      return(content(r, "text", encoding = "UTF-8"))
    }
    Sys.sleep(2 * i)
  }
  NA_character_
}

texto  <- function(nodo, ruta) xml_text(xml_find_first(nodo, ruta))
atrib  <- function(nodo, ruta) xml_attr(xml_find_first(nodo, ruta), "Valor")

# 1. Lista de votaciones por año -------------------------------------------
lista <- map_dfr(annos, function(a) {
  archivo <- file.path(dir_raw, paste0("anno_", a, ".xml"))
  # el año en curso se vuelve a descargar siempre
  if (!file.exists(archivo) || a == max(annos)) {
    x <- traer(paste0(base, "/retornarVotacionesXAnno?prmAnno=", a))
    if (is.na(x)) stop("No se pudo descargar la lista del año ", a)
    writeLines(x, archivo, useBytes = TRUE)
  }
  v <- xml_find_all(xml_ns_strip(read_xml(archivo)), "//Votacion")
  message(a, ": ", length(v), " votaciones")
  tibble(
    votacion_id     = as.integer(texto(v, "./Id")),
    fecha           = texto(v, "./Fecha"),
    descripcion     = texto(v, "./Descripcion"),
    tipo            = texto(v, "./Tipo"),
    tipo_valor      = atrib(v, "./Tipo"),
    resultado       = texto(v, "./Resultado"),
    quorum          = texto(v, "./Quorum"),
    total_si        = as.integer(texto(v, "./TotalSi")),
    total_no        = as.integer(texto(v, "./TotalNo")),
    total_abst      = as.integer(texto(v, "./TotalAbstencion")),
    total_dispensado = as.integer(texto(v, "./TotalDispensado")),
    anno_consulta   = a
  )
}) |>
  distinct(votacion_id, .keep_all = TRUE)

write_csv(lista, file.path(dir_proc, "votaciones.csv"))

# 2. Detalle de cada votación (XML original, por año) ------------------------
for (a in annos) {
  archivo <- file.path(dir_raw, paste0("detalle_", a, ".rds"))
  guardado <- if (file.exists(archivo)) readRDS(archivo) else list()
  ids <- setdiff(as.character(lista$votacion_id[lista$anno_consulta == a]),
                 names(guardado)[!is.na(unlist(guardado))])
  if (length(ids) == 0) next
  message("\n", a, ": faltan ", length(ids), " votaciones")
  for (k in seq_along(ids)) {
    guardado[[ids[k]]] <- traer(paste0(base, "/retornarVotacionDetalle?prmVotacionId=", ids[k]))
    Sys.sleep(0.25)
    if (k %% 100 == 0 || k == length(ids)) {
      saveRDS(guardado, archivo, compress = "xz")
      message("  ", k, " de ", length(ids))
    }
  }
}

# 3. Formato largo ----------------------------------------------------------
leer_detalle <- function(id, x) {
  if (is.na(x)) return(NULL)
  v <- xml_find_all(xml_ns_strip(read_xml(x)), "//Votos/Voto")
  if (length(v) == 0) return(NULL)
  tibble(
    votacion_id  = as.integer(id),
    diputado_id  = as.integer(texto(v, "./Diputado/Id")),
    nombre       = texto(v, "./Diputado/Nombre"),
    ap_paterno   = texto(v, "./Diputado/ApellidoPaterno"),
    ap_materno   = texto(v, "./Diputado/ApellidoMaterno"),
    opcion       = texto(v, "./OpcionVoto"),
    opcion_valor = atrib(v, "./OpcionVoto")
  )
}

votos <- map_dfr(annos, function(a) {
  archivo <- file.path(dir_raw, paste0("detalle_", a, ".rds"))
  if (!file.exists(archivo)) return(NULL)
  g <- readRDS(archivo)
  imap_dfr(g, \(x, id) leer_detalle(id, x))
})

diputados <- votos |>
  distinct(diputado_id, nombre, ap_paterno, ap_materno) |>
  arrange(diputado_id)
write_csv(diputados, file.path(dir_proc, "diputados_votaciones.csv"))

votos |>
  select(votacion_id, diputado_id, opcion, opcion_valor) |>
  write_csv(file.path(dir_proc, "votos.csv.gz"))

# 4. Control ----------------------------------------------------------------
sin_detalle <- setdiff(lista$votacion_id, unique(votos$votacion_id))
sink("descarga_resumen.txt", split = TRUE)
cat("Votaciones listadas:", nrow(lista), "\n")
cat("Votaciones con detalle:", n_distinct(votos$votacion_id), "\n")
cat("Votaciones sin detalle:", length(sin_detalle), "\n")
cat("Votos individuales:", nrow(votos), "\n")
cat("Diputados distintos:", n_distinct(votos$diputado_id), "\n\n")
cat("Votaciones por año y tipo:\n")
print(table(substr(lista$fecha, 1, 4), lista$tipo))
cat("\nOpciones de voto:\n")
print(count(votos, opcion, opcion_valor))
cat("\nResultados:\n")
print(count(lista, resultado))
cat("\nQuórum:\n")
print(count(lista, quorum))
cat("\nPrimeras votaciones sin detalle:", head(sin_detalle, 30), "\n")
sink()
message("\nListo. Sube descarga_resumen.txt y los tres archivos de data/processed/.")
