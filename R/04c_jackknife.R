# 04c_jackknife.R
# Robustez caso a caso: reestima el modelo de salida hacia la derecha radical
# y la prueba de rangos excluyendo cada salida, una a la vez.

source(file.path("R", "00_funciones.R"))
nombres <- read_csv(file.path(dir_proc, "salidas_verificadas.csv"), show_col_types = FALSE) |> distinct(id, nombre)
res <- map_dfr(c("", "_omega1"), function(suf) {
  conv <- read_csv(file.path(dir_proc, paste0("panel_riesgo", suf, ".csv")), show_col_types = FALSE) |>
    filter(tipo_partido == "convencional")
  mz <- read_csv(file.path(dir_out, "tablas", "08_matriz_pruebas.csv"), show_col_types = FALSE) |>
    filter(grupo == "Hacia la derecha radical")
  col_pct <- ifelse(suf == "", "percentil_w01", "percentil_w1")
  casos <- conv |> filter(hacia_radical == 1) |> distinct(diputado_id)
  map_dfr(casos$diputado_id, function(id) {
    m <- glm(hacia_radical ~ dist_der + dist_izq + periodo, binomial("cloglog"), conv |> filter(diputado_id != id))
    tg <- tabla_glm(m, setNames(conv$diputado_id[conv$diputado_id != id], rownames(m$model))) |> filter(termino == "dist_der")
    nom <- nombres$nombre[nombres$id == id][1]
    pc <- mz |> filter(nombre != nom) |> pull(!!col_pct)
    tibble(especificacion = ifelse(suf == "", "omega2 = 0,1", "omega2 = 1"), excluido = id,
           coef = tg$coef, p = tg$p, percentil_medio = mean(pc, na.rm = TRUE))
  })
})
res <- res |> left_join(nombres, by = c("excluido" = "id"))
print(res, n = 30)
print(res |> group_by(especificacion) |>
        summarise(coef_min = min(coef), coef_max = max(coef), p_max = max(p),
                  pct_min = min(percentil_medio), pct_max = max(percentil_medio)))
write_csv(res, file.path(dir_out, "tablas", "04c_jackknife.csv"))
