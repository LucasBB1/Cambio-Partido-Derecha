# 01b_descargar_proyectos.R
# Descarga la ficha de cada proyecto de ley votado en la Sala de la Cámara
# (título, fecha de ingreso, iniciativa, cámara de origen y materias) desde
# los datos abiertos de la Cámara, para clasificar las votaciones por tema.
# Además prueba variantes del servicio de votaciones del Senado.
#
# Se puede interrumpir y volver a correr: retoma donde quedó.
# Productos: data/processed/proyectos.csv, data/processed/proyectos_materias.csv,
#            data/processed/materias.csv y senado_variantes_salida.txt

paquetes <- c("httr", "xml2", "dplyr", "purrr", "readr", "stringr")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)
library(httr); library(xml2); library(dplyr); library(purrr); library(readr); library(stringr)

base <- "https://opendata.camara.cl/camaradiputados/WServices/WSLegislativo.asmx"
dir_raw <- file.path("data", "raw", "proyectos"); dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
dir_proc <- file.path("data", "processed")

traer <- function(url, intentos = 4) {
  for (i in seq_len(intentos)) {
    r <- tryCatch(GET(url, user_agent("R (investigacion academica)"), timeout(90)), error = function(e) NULL)
    if (!is.null(r) && status_code(r) == 200) return(content(r, "text", encoding = "UTF-8"))
    Sys.sleep(2 * i)
  }
  NA_character_
}

# 1. Boletines votados en la Sala ------------------------------------------
vt <- read_csv(file.path(dir_proc, "votaciones.csv"), show_col_types = FALSE)
boletines <- vt$descripcion |> str_extract_all("\\d{3,5}-\\d{2}") |> unlist() |> unique() |> sort()
message("Boletines distintos: ", length(boletines))

# 2. Ficha de cada proyecto (XML original guardado en un solo archivo) -----
archivo <- file.path(dir_raw, "fichas_proyectos.rds")
fichas <- if (file.exists(archivo)) readRDS(archivo) else list()
faltan_b <- setdiff(boletines, names(fichas)[!is.na(unlist(fichas))])
for (k in seq_along(faltan_b)) {
  fichas[[faltan_b[k]]] <- traer(paste0(base, "/retornarProyectoLey?prmNumeroBoletin=", faltan_b[k]))
  Sys.sleep(0.25)
  if (k %% 100 == 0 || k == length(faltan_b)) { saveRDS(fichas, archivo, compress = "xz"); message("  ", k, " de ", length(faltan_b)) }
}

# 3. Lectura ------------------------------------------------------------------
txt1 <- function(x, ruta) { n <- xml_find_first(x, ruta); if (inherits(n, "xml_missing")) NA_character_ else xml_text(n) }
leer <- function(b, s) {
  if (is.na(s)) return(NULL)
  x <- tryCatch(xml_ns_strip(read_xml(s)), error = function(e) NULL)
  if (is.null(x)) return(NULL)
  list(
    ficha = tibble(boletin = b, id_proyecto = txt1(x, "/ProyectoLey/Id"), nombre = txt1(x, "/ProyectoLey/Nombre"),
                   fecha_ingreso = txt1(x, "/ProyectoLey/FechaIngreso"), iniciativa = txt1(x, "/ProyectoLey/TipoIniciativa"),
                   camara_origen = txt1(x, "/ProyectoLey/CamaraOrigen"),
                   nodos = paste(unique(xml_name(xml_children(xml_root(x)))), collapse = ";")),
    materias = {
      m <- xml_find_all(x, "/ProyectoLey/Materias/Materia")
      if (length(m) == 0) NULL else tibble(boletin = b, materia_id = xml_text(xml_find_first(m, "./Id")),
                                           materia = xml_text(xml_find_first(m, "./Nombre")))
    })
}
lst <- imap(fichas, \(s, b) leer(b, s))
proyectos <- map_dfr(lst, "ficha")
materias_p <- map_dfr(lst, "materias")
write_csv(proyectos, file.path(dir_proc, "proyectos.csv"))
write_csv(materias_p, file.path(dir_proc, "proyectos_materias.csv"))

# Catálogo de materias
cat_m <- traer(paste0(base, "/retornarMaterias"))
if (!is.na(cat_m)) writeLines(cat_m, file.path(dir_raw, "materias.xml"), useBytes = TRUE)

# 4. Servicio del Senado: variantes ------------------------------------------
sink("senado_variantes_salida.txt", split = TRUE)
cat("Proyectos leídos:", nrow(proyectos), "de", length(boletines), "| con materias:", n_distinct(materias_p$boletin), "\n")
cat("Nodos de la ficha:", proyectos$nodos[1], "\n\n")
print(head(proyectos |> select(boletin, nombre), 5))
print(count(materias_p, materia, sort = TRUE), n = 40)
for (url in c("https://tramitacion.senado.cl/wspublico/votaciones.php?boletin=8575",
              "https://tramitacion.senado.cl/wspublico/votaciones.php?boletin=8575-05",
              "https://tramitacion.senado.cl/wspublico/votaciones.php?boletin=13501-07",
              "https://tramitacion.senado.cl/wspublico/tramitacion.php?boletin=8575",
              "https://tramitacion.senado.cl/wspublico/senadores_vigentes.php")) {
  s <- traer(url, intentos = 1)
  cat("\n>>", url, "\n", substr(ifelse(is.na(s), "sin respuesta", s), 1, 800), "\n")
}
sink()
message("Listo. Sube senado_variantes_salida.txt y, de data/processed/, proyectos.csv y proyectos_materias.csv.")
