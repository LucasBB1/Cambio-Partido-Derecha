# 01c_descargar_senado.R
# Descarga las votaciones de Sala del Senado (legislaturas 362 a 374,
# marzo de 2014 a la fecha) desde el servicio que alimenta la sección de
# votaciones de www.senado.cl (web-back.senado.cl/api/votes).
#
# Se puede interrumpir y volver a correr: guarda la respuesta original de cada
# página en data/raw/senado/ y retoma donde quedó.
# Productos: data/processed/votaciones_senado.csv,
#            data/processed/votos_senado.csv.gz,
#            data/processed/senadores_votaciones.csv y descarga_senado_resumen.txt

paquetes <- c("httr", "jsonlite", "dplyr", "purrr", "readr")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)
library(httr); library(jsonlite); library(dplyr); library(purrr); library(readr)

api <- "https://web-back.senado.cl/api"
dir_raw <- file.path("data", "raw", "senado"); dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
dir_proc <- file.path("data", "processed")

traer <- function(url, intentos = 4) {
  for (i in seq_len(intentos)) {
    r <- tryCatch(GET(url, user_agent("R (investigacion academica)"), timeout(120)), error = function(e) NULL)
    if (!is.null(r) && status_code(r) == 200) return(content(r, "text", encoding = "UTF-8"))
    Sys.sleep(3 * i)
  }
  NA_character_
}

leg <- fromJSON(traer(paste0(api, "/legislatures?limit=100")))$data
leg <- leg |> filter(NUMERO >= 362) |> arrange(NUMERO)
print(leg)

por_pagina <- 100
for (k in seq_len(nrow(leg))) {
  idl <- leg$ID_LEGISLATURA[k]
  archivo <- file.path(dir_raw, paste0("legislatura_", leg$NUMERO[k], ".rds"))
  paginas <- if (file.exists(archivo)) readRDS(archivo) else list()
  primera <- if (length(paginas) > 0) paginas[[1]] else
    traer(sprintf("%s/votes?id_legislatura=%s&limit=%s&offset=0&palabra_clave=&desde=&hasta=", api, idl, por_pagina))
  total <- fromJSON(primera)$data$total
  n_pag <- ceiling(total / por_pagina)
  paginas[["0"]] <- primera
  message("Legislatura ", leg$NUMERO[k], ": ", total, " votaciones, ", n_pag, " páginas")
  for (p in seq_len(n_pag) - 1) {
    clave <- as.character(p)
    if (!is.null(paginas[[clave]]) && !is.na(paginas[[clave]])) next
    paginas[[clave]] <- traer(sprintf("%s/votes?id_legislatura=%s&limit=%s&offset=%s&palabra_clave=&desde=&hasta=",
                                      api, idl, por_pagina, p * por_pagina))
    Sys.sleep(0.5)
  }
  saveRDS(paginas, archivo, compress = "xz")
}

# Lectura ---------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
n_raros <- 0
leer_pagina <- function(txt) {
  if (is.na(txt)) return(NULL)
  d <- fromJSON(txt, simplifyVector = FALSE)$data$data
  map(d, function(v) {
    cab <- tibble(id_votacion = v$ID_VOTACION, id_sesion = v$ID_SESION, numero_sesion = v$NUMERO_SESION,
                  fecha = v$FECHA_VOTACION, tema = v$TEMA %||% NA_character_, quorum = v$QUORUM %||% NA_character_,
                  boletin = v$BOLETIN %||% NA_character_, si = v$SI, no = v$NO, abs = v$ABS, pareo = v$PAREO)
    votos <- imap_dfr(v$VOTACIONES, function(lista, opcion) {
      if (length(lista) == 0) return(NULL)
      # un solo senador puede venir como objeto y no como lista de objetos
      if (!is.null(names(lista)) && "PARLID" %in% names(lista)) lista <- list(lista)
      raros <- !vapply(lista, is.list, TRUE)
      if (any(raros)) assign("n_raros", n_raros + sum(raros), envir = .GlobalEnv)
      lista <- lista[!raros]
      if (length(lista) == 0) return(NULL)
      map_dfr(lista, \(s) tibble(parlid = s$PARLID %||% NA_integer_, nombre = s$NOMBRE %||% NA_character_,
                                 ap_paterno = s$APELLIDO_PATERNO %||% NA_character_,
                                 ap_materno = s$APELLIDO_MATERNO %||% NA_character_, opcion = opcion))
    })
    list(cab = cab, votos = if (nrow(votos) > 0) mutate(votos, id_votacion = v$ID_VOTACION) else NULL)
  })
}
todo <- list.files(dir_raw, pattern = "^legislatura_", full.names = TRUE) |>
  map(readRDS) |> unlist(recursive = FALSE) |> map(leer_pagina) |> unlist(recursive = FALSE)
votaciones <- map_dfr(todo, "cab") |> distinct(id_votacion, .keep_all = TRUE)
votos <- map_dfr(todo, "votos") |> distinct(id_votacion, parlid, .keep_all = TRUE)

write_csv(votaciones, file.path(dir_proc, "votaciones_senado.csv"))
votos |> select(id_votacion, parlid, opcion) |> write_csv(file.path(dir_proc, "votos_senado.csv.gz"))
votos |> distinct(parlid, nombre, ap_paterno, ap_materno) |> arrange(ap_paterno) |>
  write_csv(file.path(dir_proc, "senadores_votaciones.csv"))

sink("descarga_senado_resumen.txt", split = TRUE)
cat("Votaciones:", nrow(votaciones), "| votos individuales:", nrow(votos), "| senadores:", n_distinct(votos$parlid), "\n\n")
print(table(substr(votaciones$fecha, 7, 10)))
print(count(votos, opcion))
print(count(votaciones, quorum, sort = TRUE))
cat("\nElementos de voto con formato inesperado (omitidos):", n_raros, "\n")
cat("\nVotaciones con boletín:", sum(!is.na(votaciones$boletin) & votaciones$boletin != ""), "\n")
sink()
message("Listo. Sube descarga_senado_resumen.txt y los tres archivos del Senado de data/processed/.")
