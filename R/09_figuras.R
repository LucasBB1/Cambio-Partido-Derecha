# 09_figuras.R
# Figuras del documento.

source(file.path("R", "00_funciones.R"))

col <- c(radical = "#2a78d6", otras = "#eb6834", desde = "#1baf7a")
gris_banda <- "#d9d8d2"; tinta <- "#2b2b29"; tinta2 <- "#6b6a63"
tema <- theme_minimal(base_size = 10, base_family = "sans") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#ecebe6", linewidth = 0.3),
        axis.text = element_text(color = tinta2), axis.title = element_text(color = tinta2),
        strip.text = element_text(color = tinta, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "top", legend.justification = "left", legend.title = element_blank(),
        legend.text = element_text(color = tinta))
guardar <- function(p, nombre, ancho = 16, alto = 9) {
  ggsave(file.path(dir_out, "figuras", nombre), p, width = ancho, height = alto, units = "cm", dpi = 300, bg = "white")
}
etq_grupo <- c("Hacia la derecha radical" = "Hacia la derecha radical",
               "Otras salidas de la derecha convencional" = "Otras salidas de la derecha convencional")
esp <- c("_w01" = "Más suavizado (ω² = 0,1)", "_w1" = "Menos suavizado (ω² = 1)")
orden_esp <- function(x) factor(x, levels = unname(esp))
# siglas de presentación (los datos usan PREP, DC y PC)
sig <- function(x) dplyr::recode(x, PREP = "PRCh", DC = "PDC", PC = "PCCh")

# Figura 1. Salidas por fecha, partido de origen y destino ------------------
s <- read_csv(file.path(dir_proc, "salidas_verificadas.csv"), show_col_types = FALSE) |>
  mutate(grupo = case_when(partido_origen %in% c("UDI", "RN", "EVOP", "AMPL") & destino_bloque == "radical" ~ "Hacia la derecha radical",
                           partido_origen %in% c("UDI", "RN", "EVOP", "AMPL") ~ "Otras salidas de la derecha convencional",
                           TRUE ~ "Salidas de la derecha radical"),
         apellido = word(nombre, -2), apellido = ifelse(apellido == "Real", "Del Real", apellido),
         apellido = ifelse(grepl("Kaiser", nombre), "Kaiser", apellido),
         fila = factor(sig(partido_origen), levels = rev(c("UDI", "RN", "AMPL", "PRCh", "PCC"))))
# Etiquetas: salidas de un mismo partido con menos de 60 días de diferencia
# se agrupan; dentro de cada fila se alternan arriba y abajo
etiquetas1 <- s |> arrange(fila, fecha_salida) |> group_by(fila) |>
  mutate(bloque = cumsum(c(1, diff(as.numeric(fecha_salida)) > 60))) |>
  group_by(fila, bloque) |>
  summarise(fecha = mean(fecha_salida), texto = paste(unique(apellido), collapse = "\n"), .groups = "drop") |>
  group_by(fila) |>
  mutate(arriba = !word(texto, 1, sep = "\n") %in% c("Labbé", "Verdugo", "Romero", "Del Real", "Kaiser"),
         y = as.numeric(fila) + ifelse(arriba, 0.16, -0.16), vj = ifelse(arriba, 0, 1)) |> ungroup()
f1 <- ggplot(s, aes(fecha_salida, fila, color = grupo)) +
  geom_vline(xintercept = as.Date(c("2018-03-11", "2022-03-11", "2026-03-11")), color = "#d9d8d2", linewidth = 0.4) +
  geom_point(size = 2.6) +
  geom_text(data = etiquetas1, aes(x = fecha, y = y, label = texto, vjust = vj), inherit.aes = FALSE,
            size = 2.3, color = tinta, lineheight = 0.85) +
  scale_color_manual(values = c("Hacia la derecha radical" = col[["radical"]],
                                "Otras salidas de la derecha convencional" = col[["otras"]],
                                "Salidas de la derecha radical" = col[["desde"]])) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", limits = as.Date(c("2014-03-01", "2027-01-01"))) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1))) + coord_cartesian(clip = "off") +
  labs(x = NULL, y = "Partido de origen") + tema +
  theme(panel.grid.major.y = element_line(color = "#ecebe6", linewidth = 0.3)) +
  guides(color = guide_legend(nrow = 2))
guardar(f1, "fig1_salidas.png", alto = 8)

# Figura 2. Estudio de eventos: posición relativa al partido de origen ------
ev <- bind_rows(
  read_csv(file.path(dir_out, "tablas", "05_evento_posicion.csv"), show_col_types = FALSE) |> mutate(spec = esp[["_w01"]]),
  read_csv(file.path(dir_out, "tablas", "05_evento_posicion_omega1.csv"), show_col_types = FALSE) |> mutate(spec = esp[["_w1"]])) |>
  filter(grupo %in% names(etq_grupo)) |> mutate(spec = orden_esp(spec))
banda <- ev |> filter(grupo == "Hacia la derecha radical")
f2 <- ggplot(ev, aes(k, media, color = grupo)) +
  geom_ribbon(data = banda, aes(x = k, ymin = p025, ymax = p975), inherit.aes = FALSE, fill = gris_banda, alpha = 0.8) +
  geom_hline(yintercept = 0, color = tinta2, linewidth = 0.3) +
  geom_vline(xintercept = 0, color = tinta2, linewidth = 0.3, linetype = "22") +
  geom_line(linewidth = 0.7) + geom_point(size = 1.4) +
  facet_wrap(~ spec) +
  scale_color_manual(values = c("Hacia la derecha radical" = col[["radical"]],
                                "Otras salidas de la derecha convencional" = col[["otras"]])) +
  scale_x_continuous(breaks = seq(-8, 8, 2)) +
  labs(x = "Trimestres respecto de la salida", y = "Posición respecto de la mediana\ndel partido de origen (DE)") +
  tema
guardar(f2, "fig2_evento_posicion.png", alto = 9)

# Figura 3. Acuerdo con la mayoría del partido de origen --------------------
ac <- read_csv(file.path(dir_out, "tablas", "05_evento_acuerdo.csv"), show_col_types = FALSE) |>
  filter(grupo %in% names(etq_grupo))
f3 <- ggplot(ac, aes(k, 100 * media, color = grupo)) +
  geom_hline(yintercept = 0, color = tinta2, linewidth = 0.3) +
  geom_vline(xintercept = 0, color = tinta2, linewidth = 0.3, linetype = "22") +
  geom_line(linewidth = 0.7) + geom_point(size = 1.4) +
  scale_color_manual(values = c("Hacia la derecha radical" = col[["radical"]],
                                "Otras salidas de la derecha convencional" = col[["otras"]])) +
  scale_x_continuous(breaks = seq(-8, 8, 2)) +
  labs(x = "Trimestres respecto de la salida", y = "Cambio en el acuerdo con la mayoría\ndel partido de origen (puntos)") +
  tema
guardar(f3, "fig3_evento_acuerdo.png", alto = 8.5)

# Figura 4. Percentil previo dentro del partido, por caso -------------------
mz <- read_csv(file.path(dir_out, "tablas", "08_matriz_pruebas.csv"), show_col_types = FALSE) |>
  filter(grupo != "Salidas de la derecha radical", !(is.na(percentil_w01) & is.na(percentil_w1))) |>
  mutate(apellido = word(nombre, -2), apellido = ifelse(apellido == "Real", "Del Real", apellido),
         etiqueta = paste0(apellido, " (", sig(partido_origen), ", ", format(fecha_salida, "%Y"), ")")) |>
  arrange(grupo, pmax(percentil_w01, percentil_w1, na.rm = TRUE)) |>
  mutate(etiqueta = factor(etiqueta, levels = unique(etiqueta)))
mzl <- mz |> select(etiqueta, grupo, percentil_w01, percentil_w1) |>
  pivot_longer(starts_with("percentil"), names_to = "spec", values_to = "p") |>
  mutate(spec = ifelse(spec == "percentil_w01", esp[["_w01"]], esp[["_w1"]]))
f4 <- ggplot(mzl, aes(p, etiqueta)) +
  geom_vline(xintercept = 0.5, color = tinta2, linewidth = 0.3, linetype = "22") +
  geom_line(aes(group = etiqueta), color = gris_banda, linewidth = 1.2) +
  geom_point(aes(shape = spec, color = grupo), size = 2.2) +
  scale_shape_manual(values = c(16, 1)) +
  scale_color_manual(values = c("Hacia la derecha radical" = col[["radical"]],
                                "Otras salidas de la derecha convencional" = col[["otras"]]), guide = "none") +
  facet_grid(grupo ~ ., scales = "free_y", space = "free_y", labeller = label_wrap_gen(18)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(x = "Percentil de la posición en el año previo dentro del partido de origen", y = NULL) +
  tema + theme(strip.text.y = element_text(angle = 0, hjust = 0), panel.grid.major.y = element_blank())
guardar(f4, "fig4_percentil_previo.png", alto = 12)
