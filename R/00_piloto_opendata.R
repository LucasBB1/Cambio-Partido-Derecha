# 00_piloto_opendata.R
# Piloto: verifica las operaciones y la estructura XML de las votaciones
# en los datos abiertos de la Cámara antes de la descarga completa.
# Genera piloto_salida.txt y guarda los XML de prueba en data/raw/piloto/.

paquetes <- c("httr", "xml2")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)
library(httr)
library(xml2)

base <- "https://opendata.camara.cl/camaradiputados/WServices/WSLegislativo.asmx"
dir_piloto <- file.path("data", "raw", "piloto")
dir.create(dir_piloto, recursive = TRUE, showWarnings = FALSE)
salida <- "piloto_salida.txt"
sink(salida, split = TRUE)

traer <- function(url, archivo) {
  r <- GET(url, user_agent("R (investigacion academica)"), timeout(60))
  cat("\n>>", url, "\n   estado HTTP:", status_code(r), "\n")
  if (status_code(r) == 200) writeBin(content(r, "raw"), file.path(dir_piloto, archivo))
  invisible(r)
}

# 1. Operaciones disponibles según el WSDL
r <- traer(paste0(base, "?WSDL"), "wsdl.xml")
if (status_code(r) == 200) {
  w <- xml_ns_strip(read_xml(content(r, "raw")))
  ops <- unique(xml_attr(xml_find_all(w, "//portType/operation"), "name"))
  cat("\nOperaciones:\n"); print(ops)
  cat("\nParámetros de las operaciones con 'Votacion':\n")
  for (op in grep("Votacion", ops, value = TRUE)) {
    el <- xml_find_first(w, sprintf("//*[local-name()='element'][@name='%s']", op))
    cat(op, ":", paste(xml_attr(xml_find_all(el, ".//*[local-name()='element']"), "name"), collapse = ", "), "\n")
  }
}

# 2. Votaciones de un año
r <- traer(paste0(base, "/retornarVotacionesXAnno?prmAnno=2023"), "votaciones_2023.xml")
if (status_code(r) == 200) {
  x <- xml_ns_strip(read_xml(content(r, "raw")))
  v <- xml_find_all(x, "//Votacion")
  cat("\nVotaciones en 2023:", length(v), "\n\nEstructura de la primera:\n")
  xml_structure(v[[1]])
  ids <- xml_text(xml_find_all(x, "//Votacion/Id"))
  cat("\nPrimeros Id:", head(ids, 5), "\n")
  tipos <- xml_text(xml_find_all(x, "//Votacion/Tipo"))
  cat("\nTipos de votación en 2023:\n"); print(table(tipos))

  # 3. Detalle de tres votaciones
  for (id in ids[c(1, 50, 200)]) {
    r2 <- traer(paste0(base, "/retornarVotacionDetalle?prmVotacionId=", id),
                paste0("detalle_", id, ".xml"))
    if (status_code(r2) == 200) {
      d <- xml_ns_strip(read_xml(content(r2, "raw")))
      votos <- xml_find_all(d, "//Voto")
      cat("   votos individuales:", length(votos), "\n")
      if (length(votos) > 0) { cat("   estructura del primer voto:\n"); xml_structure(votos[[1]]) }
      cat("   nodos hijos de la votación:",
          paste(unique(xml_name(xml_children(xml_root(d)))), collapse = ", "), "\n")
    }
  }
}
sink()
cat("\nListo. Sube el archivo", normalizePath(salida), "\n")
