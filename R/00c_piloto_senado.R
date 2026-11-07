# 00c_piloto_senado.R
# Piloto: verifica el servicio de votaciones del Senado por número de
# boletín y la estructura de su respuesta. Genera piloto_senado_salida.txt.

paquetes <- c("httr", "xml2")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)
library(httr)
library(xml2)

dir_piloto <- file.path("data", "raw", "piloto_senado")
dir.create(dir_piloto, recursive = TRUE, showWarnings = FALSE)
sink("piloto_senado_salida.txt", split = TRUE)

candidatas <- c(
  "https://tramitacion.senado.cl/wspublico/votaciones.php?boletin=%s",
  "http://tramitacion.senado.cl/wspublico/votaciones.php?boletin=%s"
)
# Boletines de prueba: retiro de fondos previsionales (13501), Ley Naín-Retamal
# (15805) y un boletín sin votaciones esperadas (1)
boletines <- c("13501", "15805", "1")

for (plantilla in candidatas) {
  for (b in boletines) {
    url <- sprintf(plantilla, b)
    r <- tryCatch(GET(url, user_agent("R (investigacion academica)"), timeout(60)), error = function(e) NULL)
    cat("\n>>", url, "\n   estado HTTP:", if (is.null(r)) "error de conexión" else status_code(r), "\n")
    if (is.null(r) || status_code(r) != 200) next
    txt <- content(r, "text", encoding = "UTF-8")
    writeLines(txt, file.path(dir_piloto, paste0("boletin_", b, ".xml")), useBytes = TRUE)
    cat("   primeros caracteres:\n", substr(txt, 1, 600), "\n")
    x <- tryCatch(xml_ns_strip(read_xml(txt)), error = function(e) NULL)
    if (is.null(x)) { cat("   (no es XML válido)\n"); next }
    hijos <- xml_children(xml_root(x))
    cat("   nodos de primer nivel:", length(hijos), "| nombres:", paste(unique(xml_name(hijos)), collapse = ", "), "\n")
    if (length(hijos) > 0) { cat("   estructura del primero:\n"); xml_structure(hijos[[1]]) }
  }
}
# Títulos y materias de los proyectos: dos fuentes posibles
cat("\n\n==== TÍTULOS DE PROYECTOS ====\n")
for (url in c("https://tramitacion.senado.cl/wspublico/tramitacion.php?boletin=13501",
              "https://opendata.camara.cl/camaradiputados/WServices/WSLegislativo.asmx/retornarProyectoLey?prmNumeroBoletin=13501-07")) {
  r <- tryCatch(GET(url, user_agent("R (investigacion academica)"), timeout(60)), error = function(e) NULL)
  cat("\n>>", url, "\n   estado HTTP:", if (is.null(r)) "error de conexión" else status_code(r), "\n")
  if (!is.null(r) && status_code(r) == 200) cat(substr(content(r, "text", encoding = "UTF-8"), 1, 1500), "\n")
}
# Operaciones disponibles en el servicio legislativo de la Cámara
r <- GET("https://opendata.camara.cl/camaradiputados/WServices/WSLegislativo.asmx?WSDL", timeout(60))
ops <- unique(regmatches(content(r, "text", encoding = "UTF-8"),
                         gregexpr('operation name="[A-Za-z]+"', content(r, "text", encoding = "UTF-8")))[[1]])
cat("\nOperaciones de WSLegislativo:\n"); print(ops)
sink()
cat("\nListo. Sube el archivo", normalizePath("piloto_senado_salida.txt"), "\n")
