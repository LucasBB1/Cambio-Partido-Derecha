# 00_funciones.R
# Paquetes, rutas y funciones comunes.

paquetes <- c("dplyr", "tidyr", "readr", "purrr", "stringr", "ggplot2", "survival", "scales")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)
invisible(lapply(paquetes, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))

dir_fuentes <- file.path("data", "fuentes")
dir_proc    <- file.path("data", "processed")
dir_out     <- "output"
for (d in c(dir_proc, file.path(dir_out, "tablas"), file.path(dir_out, "figuras"))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Trimestre como índice entero (2014T1 = 1)
trimestre <- function(fecha) {
  fecha <- as.Date(fecha)
  (as.integer(format(fecha, "%Y")) - 2014) * 4 + (as.integer(format(fecha, "%m")) - 1) %/% 3 + 1
}
etiqueta_trimestre <- function(t) paste0(2014 + (t - 1) %/% 4, "T", (t - 1) %% 4 + 1)

# ---------------------------------------------------------------------------
# Modelo IRT probit dinámico (una dimensión), estimado por EM.
#
#   y*_uj = a_j + b_j * x_u + e,  e ~ N(0, 1),  y = 1 si y* > 0
#   u = (diputado, trimestre). Para cada diputado, sus trimestres activos
#   t_1 < ... < t_K siguen un paseo aleatorio:
#   x_{t1} ~ N(0, 1);  x_{tk} - x_{tk-1} ~ N(0, omega2 * (t_k - t_{k-1}))
#   (a_j, b_j) ~ N(0, 25 I)
#
# Es la estimación de moda posterior por EM de Imai, Lo y Olmsted (2016),
# con la dinámica de Martin y Quinn (2002). Las posiciones de distintos
# trimestres quedan en una escala común porque cada diputado enlaza sus
# trimestres y cada votación enlaza a los diputados de su trimestre.
#
# Entradas: vectores de igual largo con la unidad (u), la votación (j) y el
# voto (y en 0/1); un data frame de unidades con columnas u, leg, t.
# ---------------------------------------------------------------------------
irt_dinamico <- function(u, j, y, unidades, omega2 = 0.1, x0 = NULL,
                         max_iter = 5000, tol = 1e-8, verbose = TRUE) {
  U <- nrow(unidades); J <- max(j)
  stopifnot(all(unidades$u == seq_len(U)))
  prior_ab <- 1 / 25
  x <- if (is.null(x0)) rep(0, U) else x0
  a <- rep(0, J); b <- rep(0, J)

  # Estructura del paseo aleatorio por diputado
  bloques <- split(unidades$u[order(unidades$leg, unidades$t)],
                   unidades$leg[order(unidades$leg, unidades$t)])
  prec_rw <- lapply(bloques, function(us) {
    k <- length(us)
    tt <- unidades$t[us]
    Q <- matrix(0, k, k); Q[1, 1] <- 1
    if (k > 1) for (s in 2:k) {
      w <- 1 / (omega2 * (tt[s] - tt[s - 1]))
      Q[s, s] <- Q[s, s] + w; Q[s - 1, s - 1] <- Q[s - 1, s - 1] + w
      Q[s, s - 1] <- Q[s, s - 1] - w; Q[s - 1, s] <- Q[s - 1, s] - w
    }
    Q
  })

  if (is.null(x0)) stop("Se requiere x0 (posiciones iniciales)")
  y1 <- y == 1
  for (iter in seq_len(max_iter)) {
    x_ant <- x
    # Paso M de las votaciones con y* del paso E
    m <- a[j] + b[j] * x[u]
    ys <- m + ifelse(y1, exp(dnorm(m, log = TRUE) - pnorm(m, log.p = TRUE)),
                         -exp(dnorm(m, log = TRUE) - pnorm(-m, log.p = TRUE)))
    xu <- x[u]
    S0 <- tabulate(j, J) + prior_ab
    S1 <- as.vector(rowsum(xu, j, reorder = TRUE))
    S2 <- as.vector(rowsum(xu^2, j, reorder = TRUE)) + prior_ab
    T0 <- as.vector(rowsum(ys, j, reorder = TRUE))
    T1 <- as.vector(rowsum(xu * ys, j, reorder = TRUE))
    det <- S0 * S2 - S1^2
    a <- (S2 * T0 - S1 * T1) / det
    b <- (S0 * T1 - S1 * T0) / det
    # Paso E de nuevo y paso M de las posiciones
    m <- a[j] + b[j] * x[u]
    ys <- m + ifelse(y1, exp(dnorm(m, log = TRUE) - pnorm(m, log.p = TRUE)),
                         -exp(dnorm(m, log = TRUE) - pnorm(-m, log.p = TRUE)))
    D <- as.vector(rowsum(b[j]^2, u, reorder = TRUE))
    C <- as.vector(rowsum(b[j] * (ys - a[j]), u, reorder = TRUE))
    for (k in seq_along(bloques)) {
      us <- bloques[[k]]
      Q <- prec_rw[[k]]
      diag(Q) <- diag(Q) + D[us]
      x[us] <- solve(Q, C[us])
    }
    # Criterio de emIRT: correlación entre iteraciones sucesivas
    cambio <- 1 - cor(x, x_ant)
    if (verbose && iter %% 50 == 0) message("  iteración ", iter, ": 1 - cor = ", signif(cambio, 3))
    if (iter > 1 && cambio < tol) break
  }
  list(x = x, a = a, b = b, iter = iter, convergio = cambio < tol)
}

# Porcentaje de votos clasificados correctamente
clasificacion <- function(fit, u, j, y) {
  pred <- (fit$a[j] + fit$b[j] * fit$x[u]) > 0
  mean(pred == (y == 1))
}

# Errores estándar agrupados (por diputado) para un glm binomial
vcov_cluster <- function(m, cluster) {
  X <- model.matrix(m)
  keep <- !is.na(coef(m))
  X <- X[, keep, drop = FALSE]
  u <- X * residuals(m, type = "working") * m$weights
  cl <- if (!is.null(names(cluster))) cluster[rownames(X)] else cluster
  meat <- crossprod(rowsum(u, cl))
  bread <- solve(crossprod(X * sqrt(m$weights)))
  G <- length(unique(cluster)); n <- nrow(X); k <- ncol(X)
  (G / (G - 1)) * ((n - 1) / (n - k)) * bread %*% meat %*% bread
}
tabla_glm <- function(m, cluster) {
  b <- coef(m)[!is.na(coef(m))]
  se <- sqrt(diag(vcov_cluster(m, cluster)))
  tibble(termino = names(b), coef = b, ee = se, z = b / se, p = 2 * pnorm(-abs(b / se)))
}
