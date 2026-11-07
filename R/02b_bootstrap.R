# 02b_bootstrap.R
# Incertidumbre de las posiciones por bootstrap paramétrico (Lewis y Poole,
# 2004): se simulan los votos observados con las probabilidades del modelo
# ajustado y se reestima. Cada réplica parte de la estimación original.

source(file.path("R", "00_funciones.R"))
B      <- if (exists("replicas")) replicas else 40
sufijo <- if (exists("sufijo_salida")) sufijo_salida else ""
set.seed(20261106)

pi <- readRDS(file.path(dir_proc, paste0("puntos_ideales", sufijo, ".rds")))
d <- pi$d; fit <- pi$fit
prob <- pnorm(fit$a[d$j] + fit$b[d$j] * fit$x[d$u])

salida <- file.path(dir_proc, paste0("bootstrap_posiciones", sufijo, ".rds"))
X <- if (file.exists(salida)) readRDS(salida) else matrix(NA_real_, nrow(pi$unidades), 0)
for (b in seq_len(B)) {
  if (ncol(X) >= B) break
  y_sim <- rbinom(length(prob), 1, prob)
  f <- irt_dinamico(d$u, d$j, y_sim, pi$unidades, omega2 = pi$omega2, x0 = fit$x, verbose = FALSE)
  x_b <- f$x * sign(cor(f$x, fit$x))
  X <- cbind(X, x_b)
  saveRDS(X, salida)
  message("réplica ", ncol(X), " de ", B, " (", f$iter, " iteraciones)")
}
