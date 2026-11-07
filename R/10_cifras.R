# 10_cifras.R
# Cifras citadas en el documento (se imprimen en output/cifras.txt).
source(file.path("R", "00_funciones.R"))
sink(file.path(dir_out, "cifras.txt"), split = TRUE)
options(width = 160, pillar.sigfig = 4)
convencional <- c("UDI", "RN", "EVOP", "AMPL"); radical <- c("PREP", "PNL", "PSC", "PCC")

for (suf in c("", "_omega1")) {
  cat("\n================ ESPECIFICACIÓN", ifelse(suf == "", "omega2 = 0,1", "omega2 = 1"), "\n")
  pi <- readRDS(file.path(dir_proc, paste0("puntos_ideales", suf, ".rds")))
  cat("votos", nrow(pi$d), "| votaciones", max(pi$d$j), "| diputados", n_distinct(pi$d$diputado_id),
      "| unidades", nrow(pi$unidades), "| iter", pi$fit$iter, "| clasif", round(100 * clasificacion(pi$fit, pi$d$u, pi$d$j, pi$d$y), 2), "\n")
  # APRE
  pred <- (pi$fit$a[pi$d$j] + pi$fit$b[pi$d$j] * pi$fit$x[pi$d$u]) > 0
  err <- sum(pred != (pi$d$y == 1))
  minor <- pi$d |> group_by(j) |> summarise(m = min(sum(y), n() - sum(y))) |> pull(m) |> sum()
  cat("APRE", round(1 - err / minor, 3), "\n")
  p <- read_csv(file.path(dir_proc, paste0("panel_trimestral", suf, ".csv")), show_col_types = FALSE)
  per <- function(t) case_when(t <= 17 ~ "2014-2018", t <= 33 ~ "2018-2022", t <= 49 ~ "2022-2026", TRUE ~ "2026-2030")
  print(p |> mutate(periodo = per(t)) |> group_by(periodo, partido) |>
          summarise(mediana_z = median(z), n = n_distinct(diputado_id), .groups = "drop") |>
          filter(n >= 4) |> arrange(periodo, mediana_z), n = 80)
  m <- read_csv(file.path(dir_out, "tablas", paste0("04_modelos_riesgo", suf, ".csv")), show_col_types = FALSE)
  print(m |> filter(!grepl("Intercept|periodo", termino)) |> mutate(HR_01 = exp(coef / 10)), n = 50)
  e <- read_csv(file.path(dir_out, "tablas", paste0("05_evento_posicion", suf, ".csv")), show_col_types = FALSE)
  print(e |> mutate(tramo = cut(k, c(-9, -5, -1, 0, 4, 8), labels = c("-8..-5", "-4..-1", "0", "1..4", "5..8"))) |>
          group_by(grupo, tramo) |> summarise(media = mean(media), p025 = mean(p025), p975 = mean(p975), n = max(n), .groups = "drop"))
  a <- read_csv(file.path(dir_out, "tablas", paste0("05_evento_acuerdo", suf, ".csv")), show_col_types = FALSE)
  print(a |> mutate(tramo = cut(k, c(-9, -5, -1, 0, 4, 8), labels = c("-8..-5", "-4..-1", "0", "1..4", "5..8"))) |>
          group_by(grupo, tramo) |> summarise(media = 100 * mean(media), .groups = "drop"))
}
cat("\n================ UNIVERSO\n")
r <- read_csv(file.path(dir_proc, "panel_riesgo.csv"), show_col_types = FALSE)
print(r |> group_by(periodo) |> summarise(dip = n_distinct(diputado_id), trimestres = n(), salidas = sum(salida), hacia_rad = sum(hacia_radical)))
s <- read_csv(file.path(dir_proc, "salidas_verificadas.csv"), show_col_types = FALSE)
print(s |> count(partido_origen, destino_bloque))
d <- read_csv(file.path(dir_out, "tablas", "03_desfase_registro.csv"), show_col_types = FALSE)
cat("desfase mediana", median(abs(d$dias)), "| >90 días", sum(d$dias > 90), "| >365", sum(d$dias > 365), "| max", max(d$dias), "\n")
cat("\n================ PRUEBAS\n")
print(read_csv(file.path(dir_out, "tablas", "08_prueba_rangos.csv"), show_col_types = FALSE))
print(read_csv(file.path(dir_out, "tablas", "08_resumen_pruebas.csv"), show_col_types = FALSE))
dm <- read_csv(file.path(dir_proc, "demanda_derecha_radical_distrito.csv"), show_col_types = FALSE)
print(dm |> group_by(anio) |> summarise(nac = 100 * sum(rad) / sum(validos), min = min(demanda_rad), max = max(demanda_rad)))
sink()
