# 08_pruebas_proceso.R
# Matriz de evidencia por caso y pruebas de proceso (aro y pistola humeante)
# para tres explicaciones de la salida: programática, organizacional y de
# carrera. Las pruebas combinan la evidencia cuantitativa (posición previa y
# cambio posterior en ambas especificaciones), la codificación de fuentes
# (motivo declarado y detonante) y las candidaturas posteriores (SERVEL).

source(file.path("R", "00_funciones.R"))

convencional <- c("UDI", "RN", "EVOP", "AMPL")
radical      <- c("PREP", "PNL", "PSC", "PCC")

salidas <- read_csv(file.path(dir_proc, "salidas_verificadas.csv"), show_col_types = FALSE) |>
  mutate(ts = trimestre(fecha_salida))
det <- read_delim(file.path(dir_fuentes, "detonantes.csv"), ";", show_col_types = FALSE) |>
  mutate(fecha_salida = as.Date(fecha_salida), fecha_detonante = as.Date(fecha_detonante))
cand <- read_csv(file.path(dir_out, "tablas", "07_casos_candidatura_siguiente.csv"), show_col_types = FALSE) |>
  filter(tipo == "positivo") |> select(id, ts, anio_sig, camara_sig, sigla_sig, condicion_sig, electo_sig)

# Candidaturas presidenciales (no figuran en los archivos parlamentarios)
pres <- tibble(id = c(857, 1135), anio_pres = c(2017, 2025), sigla_pres = c("IND", "PNL"))

# Percentil previo dentro del partido y cambio posterior, por especificación
medidas <- function(suf) {
  p <- read_csv(file.path(dir_proc, paste0("panel_trimestral", suf, ".csv")), show_col_types = FALSE)
  tr <- read_csv(file.path(dir_proc, paste0("trayectorias_salidas", suf, ".csv")), show_col_types = FALSE)
  pct <- salidas |> rowwise() |> mutate(percentil = {
    mem <- p |> filter(partido == partido_origen, t == ts - 1) |> pull(diputado_id)
    a <- p |> filter(diputado_id %in% unique(c(mem, id)), t >= ts - 4, t <= ts - 1) |>
      group_by(diputado_id) |> summarise(d = mean(dist, na.rm = TRUE), .groups = "drop")
    if (!id %in% a$diputado_id || nrow(a) < 3) NA_real_ else mean(a$d <= a$d[a$diputado_id == id])
  }) |> ungroup() |> select(id, ts, percentil)
  cambio <- tr |> group_by(id, t_salida) |>
    summarise(previo = mean(rel[k >= -4 & k <= -1], na.rm = TRUE),
              posterior = mean(rel[k >= 1 & k <= 4], na.rm = TRUE), .groups = "drop") |>
    mutate(cambio = posterior - previo) |> select(id, ts = t_salida, cambio)
  pct |> left_join(cambio, by = c("id", "ts")) |> rename_with(~ paste0(.x, ifelse(suf == "", "_w01", "_w1")), c(percentil, cambio))
}
m <- medidas("") |> left_join(medidas("_omega1"), by = c("id", "ts"))

casos <- salidas |>
  left_join(det |> select(-any_of("nombre")), by = c("id", "fecha_salida")) |>
  left_join(cand, by = c("id", "ts")) |>
  left_join(pres, by = "id") |>
  left_join(m, by = c("id", "ts")) |>
  mutate(
    grupo = case_when(partido_origen %in% convencional & destino_bloque == "radical" ~ "Hacia la derecha radical",
                      partido_origen %in% convencional ~ "Otras salidas de la derecha convencional",
                      TRUE ~ "Salidas de la derecha radical"),
    dias_desde_detonante = as.numeric(fecha_salida - fecha_detonante),
    # Explicación programática
    aro_prog = case_when(is.na(percentil_w01) & is.na(percentil_w1) ~ NA,
                         TRUE ~ pmax(percentil_w01, percentil_w1, na.rm = TRUE) >= 0.5),
    # Pistola humeante programática: el motivo declarado es programático y la
    # posición previa ya estaba en el cuartil más alejado hacia la derecha
    # (o hacia la izquierda, si sale de un partido radical hacia el centro)
    # en ambas especificaciones. Se usa solo la conducta previa a la salida.
    pistola_prog = (motivo_principal == "PROG" | motivo_secundario %in% "PROG") &
                   coalesce(pmin(percentil_w01, percentil_w1) >= 0.75 |
                            (grupo == "Salidas de la derecha radical" & pmax(percentil_w01, percentil_w1) <= 0.25), FALSE),
    # Explicación organizacional
    aro_org = tipo_detonante %in% c("SANCION", "CONFLICTO") | motivo_principal %in% c("CONDUC", "DISC") |
              motivo_secundario %in% c("CONDUC", "DISC"),
    pistola_org = tipo_detonante == "SANCION" & coalesce(dias_desde_detonante, Inf) <= 60,
    # Explicación de carrera
    compite = (!is.na(camara_sig) & camara_sig != "no compite") | !is.na(anio_pres),
    aro_carr = compite,
    # Pistola humeante de carrera: la candidatura siguiente es a un cargo
    # superior (Senado o Presidencia) y fuera del partido de origen
    pistola_carr = (camara_sig %in% "Senado" & !is.na(sigla_sig) & sigla_sig != partido_origen) |
                   (!is.na(anio_pres) & sigla_pres != partido_origen)
  )

tabla <- casos |>
  transmute(nombre, grupo, partido_origen, destino, fecha_salida,
            percentil_w01 = round(percentil_w01, 2), percentil_w1 = round(percentil_w1, 2),
            cambio_w01 = round(cambio_w01, 2), cambio_w1 = round(cambio_w1, 2),
            motivo_principal, motivo_secundario, tipo_detonante, dias_desde_detonante,
            siguiente = case_when(!is.na(anio_pres) ~ paste0(anio_pres, " Presidencia (", sigla_pres, ")"),
                                  is.na(anio_sig) ~ "sin elección aún",
                                  camara_sig == "no compite" ~ paste0(anio_sig, " no compite"),
                                  TRUE ~ paste0(anio_sig, " ", camara_sig, " ", sigla_sig,
                                                ifelse(condicion_sig == "independiente", " (ind.)", ""),
                                                ifelse(electo_sig, ", electo", ", no electo"))),
            aro_prog, pistola_prog, aro_org, pistola_org, aro_carr, pistola_carr)
write_csv(tabla, file.path(dir_out, "tablas", "08_matriz_pruebas.csv"))
print(tabla |> select(nombre, grupo, percentil_w01, percentil_w1, cambio_w01, cambio_w1, siguiente,
                      aro_prog, pistola_prog, aro_org, pistola_org, aro_carr, pistola_carr), n = 30, width = 250)

# Resumen por grupo
res <- casos |> group_by(grupo) |>
  summarise(n = n(), across(c(aro_prog, pistola_prog, aro_org, pistola_org, aro_carr, pistola_carr),
                            ~ sum(.x, na.rm = TRUE)), .groups = "drop")
print(res)
write_csv(res, file.path(dir_out, "tablas", "08_resumen_pruebas.csv"))

# Prueba de rangos: bajo la hipótesis nula de que la posición previa no se
# relaciona con la salida, el percentil de cada saliente dentro de su partido
# se distribuye uniforme entre 0 y 1. Se compara el percentil medio observado
# con 100.000 simulaciones.
set.seed(20261106)
rangos <- casos |> filter(grupo != "Salidas de la derecha radical") |>
  pivot_longer(c(percentil_w01, percentil_w1), names_to = "especificacion", values_to = "p") |>
  filter(!is.na(p)) |>
  group_by(grupo, especificacion) |>
  summarise(n = n(), percentil_medio = mean(p),
            p_derecha = mean(replicate(1e5, mean(runif(n()))) >= mean(p)),
            p_izquierda = mean(replicate(1e5, mean(runif(n()))) <= mean(p)), .groups = "drop")
print(rangos)
write_csv(rangos, file.path(dir_out, "tablas", "08_prueba_rangos.csv"))
