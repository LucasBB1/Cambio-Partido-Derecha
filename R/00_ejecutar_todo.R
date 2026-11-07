# 00_ejecutar_todo.R
# Reproduce todo el análisis en orden. Las votaciones de la Cámara (01), las
# fichas de proyectos (01b), las votaciones del Senado (01c) y los archivos
# presidenciales del SERVEL (data/raw/presidencial) deben estar descargados. El bootstrap (02b) tarda varias horas; si su resultado ya
# existe en data/processed, el script retoma donde quedó.

ejecutar <- function(script, sufijo = "") {
  e <- new.env()
  if (sufijo != "") { e$sufijo_salida <- sufijo; e$omega2_usado <- 1 }
  message("\n=== ", script, if (sufijo != "") paste0(" (", sufijo, ")"))
  sys.source(file.path("R", script), envir = e)
}

for (s in c("", "_omega1")) ejecutar("02_puntos_ideales.R", s)
for (s in c("", "_omega1")) ejecutar("03_panel.R", s)
ejecutar("06_distritos.R")
for (s in c("", "_omega1")) ejecutar("04_riesgo_salida.R", s)
for (s in c("", "_omega1")) ejecutar("05_estudio_eventos.R", s)
ejecutar("02b_bootstrap.R", "_omega1")
ejecutar("04b_incertidumbre.R")
ejecutar("07_casos.R")
ejecutar("08_pruebas_proceso.R")
ejecutar("04c_jackknife.R")
ejecutar("11_temas.R")
ejecutar("12_posiciones_tema.R")
ejecutar("13_senado.R")
ejecutar("09_figuras.R")
ejecutar("10_cifras.R")
