# 11_temas.R
# Clasificación temática de las votaciones de proyectos de ley a partir del
# título y las materias de cada proyecto (datos abiertos de la Cámara).
source(file.path("R", "00_funciones.R"))

norm <- function(x) x |> stringi::stri_trans_general("Latin-ASCII") |> tolower() |> str_squish()
proy <- read_csv(file.path(dir_proc, "proyectos.csv"), show_col_types = FALSE)
mat  <- read_csv(file.path(dir_proc, "proyectos_materias.csv"), show_col_types = FALSE) |>
  group_by(boletin) |> summarise(materias = paste(str_squish(materia), collapse = "; "), .groups = "drop")
proy <- proy |> left_join(mat, by = "boletin") |>
  mutate(texto = norm(nombre))

# Diccionarios aplicados al título del proyecto. Cada proyecto puede
# pertenecer a más de un tema.
dic <- list(
  seguridad = c("penal", "delito", "crimen", "criminal", "agravante", "carabineros", "policia", "investigaciones de chile",
                "gendarmeria", "seguridad publica", "seguridad ciudadana", "narcotrafico", "trafico ilicito", "armas",
                "terroris", "usurpacion", "estado de excepcion", "estado de sitio", "estado de emergencia",
                "infraestructura critica", "carcel", "penitenciari", "reincidencia", "flagrancia", "ministerio publico",
                "fiscalia", "orden publico", "robo", "hurto", "homicidio", "secuestro", "extorsion", "sancion penal",
                "responsabilidad penal", "legitima defensa", "fuerzas de orden", "control de identidad", "crimen organizado",
                "abigeato", "receptacion", "porte de armas", "explosivos"),
  migracion = c("migracion", "migrante", "inmigra", "extranjeria", "extranjeros", "frontera", "expulsiones administrativas",
                "expulsion como pena", "refugiad", "apatrida"),
  valorico  = c("aborto", "interrupcion voluntaria", "interrupcion del embarazo", "matrimonio", "union civil",
                "identidad de genero", "orientacion sexual", "diversidad sexual", "igualitario", "eutanasia",
                "muerte digna", "educacion sexual", "afectividad", "genero", "paridad", "objecion de conciencia",
                "libertad religiosa", "cannabis", "marihuana", "filiacion", "familias homoparentales", "lgbt",
                "violencia contra la mujer", "femicidio", "derechos sexuales", "reproductiv"),
  economia  = c("tribut", "impuesto", "renta", "royalty", "pensiones de vejez", "sistema de pensiones", "pension garantizada",
                "pension basica", "previsi", "fondos de capitalizacion",
                "retiro de fondos", "retiro excepcional", "salario", "ingreso minimo", "remuneracion", "jornada laboral",
                "codigo del trabajo", "laboral", "presupuesto", "reajuste", "subsidio", "deuda", "credito", "bancos",
                "libre competencia", "mercado de capitales", "isapre", "royalty", "aguinaldo", "bono", "sueldo")
)
# Cada término debe comenzar una palabra
for (k in names(dic)) proy[[k]] <- as.integer(grepl(paste0("\\b(", paste(dic[[k]], collapse = "|"), ")"), proy$texto))
# El femicidio y la violencia contra la mujer se cuentan solo en valórico
proy <- proy |> mutate(seguridad = ifelse(valorico == 1 & grepl("femicidio|violencia contra la mujer", texto), 0L, seguridad))

vt <- read_csv(file.path(dir_proc, "votaciones.csv"), show_col_types = FALSE) |>
  mutate(boletin = str_extract(descripcion, "\\d{3,5}-\\d{2}"))
temas <- vt |> select(votacion_id, boletin) |> left_join(proy |> select(boletin, nombre, all_of(names(dic))), by = "boletin")
write_csv(temas, file.path(dir_proc, "votaciones_temas.csv"))
write_csv(proy |> select(boletin, nombre, materias, all_of(names(dic))), file.path(dir_out, "tablas", "11_proyectos_temas.csv"))

pi <- readRDS(file.path(dir_proc, "puntos_ideales.rds"))
usadas <- tibble(votacion_id = unique(pi$d$votacion_id)) |> left_join(temas, by = "votacion_id")
print(usadas |> summarise(across(all_of(names(dic)), \(v) sum(v, na.rm = TRUE)), sin_proyecto = sum(is.na(nombre)), total = n()))
print(proy |> summarise(across(all_of(names(dic)), sum), total = n()))
