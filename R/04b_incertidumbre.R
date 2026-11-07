# 04b_incertidumbre.R
# Propaga la incertidumbre de medición de las posiciones (bootstrap
# paramétrico, especificación ω² = 1) al modelo de riesgo de salida hacia la
# derecha radical y a la prueba de rangos.

source(file.path("R", "00_funciones.R"))
convencional <- c("UDI", "RN", "EVOP", "AMPL")

pi <- readRDS(file.path(dir_proc, "puntos_ideales_omega1.rds"))
X  <- readRDS(file.path(dir_proc, "bootstrap_posiciones_omega1.rds"))
panel  <- read_csv(file.path(dir_proc, "panel_trimestral_omega1.csv"), show_col_types = FALSE)
riesgo <- read_csv(file.path(dir_proc, "panel_riesgo_omega1.csv"), show_col_types = FALSE)
salidas <- read_csv(file.path(dir_proc, "salidas_verificadas.csv"), show_col_types = FALSE) |>
  mutate(ts = trimestre(fecha_salida))

bloque_de <- function(p) case_when(p %in% convencional ~ "convencional",
                                   p %in% c("PREP", "PNL", "PSC", "PCC") ~ "radical", TRUE ~ p)
mediana_loo <- function(z, grupo) {
  out <- rep(NA_real_, length(z))
  for (g in unique(grupo)) {
    i <- which(grupo == g); if (length(i) < 3) next
    zs <- z[i]; for (k in seq_along(i)) out[i[k]] <- median(zs[-k])
  }
  out
}
base <- pi$unidades |> select(u, diputado_id, t) |>
  left_join(panel |> select(diputado_id, t, partido), by = c("diputado_id", "t")) |>
  group_by(t, partido) |> mutate(n_partido = n()) |> ungroup() |>
  mutate(bloque = bloque_de(partido))

distancias <- function(x) {
  base |> mutate(x = x) |>
    group_by(t) |>
    mutate(z = (x - mean(x)) / sd(x),
           med = ifelse(n_partido >= 3, mediana_loo(z, partido), mediana_loo(z, bloque)),
           dist = z - med) |>
    ungroup() |> select(diputado_id, t, dist)
}

res <- map_dfr(seq_len(ncol(X)), function(b) {
  dd <- distancias(X[, b])
  r <- riesgo |> select(-dist_l, -dist_der, -dist_izq) |>
    left_join(dd |> mutate(t = t + 1) |> rename(dist_l = dist), by = c("diputado_id", "t")) |>
    filter(tipo_partido == "convencional", !is.na(dist_l)) |>
    mutate(dist_der = pmax(dist_l, 0), dist_izq = pmax(-dist_l, 0))
  m <- glm(hacia_radical ~ dist_der + dist_izq + periodo, binomial("cloglog"), r)
  # percentil previo de quienes salen hacia la derecha radical
  pct <- salidas |> filter(partido_origen %in% convencional, destino_bloque == "radical") |>
    rowwise() |> mutate(p = {
      mem <- panel |> filter(partido == partido_origen, t == ts - 1) |> pull(diputado_id)
      a <- dd |> filter(diputado_id %in% unique(c(mem, id)), t >= ts - 4, t <= ts - 1) |>
        group_by(diputado_id) |> summarise(d = mean(dist, na.rm = TRUE), .groups = "drop")
      if (!id %in% a$diputado_id || nrow(a) < 3) NA_real_ else mean(a$d <= a$d[a$diputado_id == id])
    }) |> ungroup()
  tibble(b = b, coef_der = coef(m)[["dist_der"]], percentil_medio = mean(pct$p, na.rm = TRUE))
})
resumen <- res |> summarise(replicas = n(),
                            coef_mediana = median(coef_der), coef_p05 = quantile(coef_der, 0.05),
                            coef_p95 = quantile(coef_der, 0.95), coef_positivo = mean(coef_der > 0),
                            pct_mediana = median(percentil_medio), pct_p05 = quantile(percentil_medio, 0.05),
                            pct_p95 = quantile(percentil_medio, 0.95))
print(resumen)
write_csv(res, file.path(dir_out, "tablas", "04b_bootstrap_replicas.csv"))
write_csv(resumen, file.path(dir_out, "tablas", "04b_bootstrap_resumen.csv"))
