# 07_distribucion_acumulada.R
#
# regresion de distribucion (Chernozhukov, Fernandez-Val y Melly 2013) sobre el precio
# menos su media nacional del mes, con test de dominancia de primer orden.
# panel_mensual.csv -> output/tablas/distribucion_acumulada.tex, output/graficos/distribucion_acumulada.pdf,
# output/graficos/distribucion_beta.pdf

library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

NC    <- 41L                               # umbrales de la grilla
KREF  <- c(0.10, 0.25, 0.50, 0.75, 0.90)   # cuantiles que van a la tabla
ALPHA <- 0.01                              # nivel del test de dominancia

p <- PANELES[["2012_2026"]]
panel <- leer_panel(p)

d <- muestra_estacion(panel, CTRL_ESTRICTO, p$focal)
d[, post := as.integer(treated == 1L & ym >= g_entry)]
for (fv in names(PRECIOS)) {   # media del mes sobre todo el panel, no la muestra
  media_mes <- panel[!is.na(get(fv)), .(mref = mean(get(fv))), by = miym]
  d[media_mes, on = "miym", (paste0(fv, "_dev")) := get(fv) - i.mref]
}

# beta(c): efecto de la entrada sobre 1[y <= c]; cdf_cf = cdf observada - beta(c)
res <- lapply(names(PRECIOS), \(fv) {
  yv <- paste0(fv, "_dev")
  z <- d[!is.na(get(yv))]
  ks <- seq(0.02, 0.98, length.out = NC)
  cs <- quantile(z[[yv]], ks, names = FALSE)
  ind <- sprintf("ind%02d", seq_along(cs))
  z[, (ind) := lapply(cs, \(cc) as.numeric(get(yv) <= cc))]
  m <- feols(as.formula(sprintf("c(%s) ~ post | %s", paste(ind, collapse = ", "), FE_PRINCIPAL)),
             data = z, cluster = ~comuna)
  ct <- rbindlist(lapply(seq_along(ind), \(j) as.list(coeftable(m[[j]])["post", c(1, 2, 4)])))
  setnames(ct, c("est", "se", "p"))
  trat_post <- z[treated == 1L & post == 1L]
  cdf <- data.table(outcome = fv, k = ks, c = cs, ct,
                    cdf = vapply(cs, \(cc) mean(trat_post[[yv]] <= cc), numeric(1)))
  cdf[, cdf_cf := cdf - est]
  n <- n_estaciones(m[[1]], z)
  list(cdf = cdf, n_trat_post = nrow(trat_post), tratadas = n[1], controles = n[2], n_obs = nobs(m[[1]]))
})
cdf <- rbindlist(lapply(res, `[[`, "cdf"))

# dominancia: max beta(c) contra el valor critico de Gail y Green (1976)
fosd <- data.table(outcome = names(PRECIOS),
                   max_b = cdf[, max(est), by = outcome]$V1,
                   min_b = cdf[, min(est), by = outcome]$V1,
                   vc = sqrt(-log(ALPHA) / sapply(res, `[[`, "n_trat_post")))
print(fosd)

ref <- cdf[, .SD[vapply(KREF, \(kk) which.min(abs(k - kk)), integer(1))], by = outcome]
ref[, kref := rep(KREF, length(PRECIOS))]
cuerpo <- unlist(lapply(KREF, \(kk) {
  r <- ref[kref == kk][match(names(PRECIOS), outcome)]
  filas_coef(sprintf("Cuantil %d", round(100 * kk)), r$est, r$se, r$p, dig = 4)
}))
escribir_tabla("lcccc",
               c(fila("", unname(PRECIOS)), "\\midrule", cuerpo, "\\midrule",
                 fila("$\\max_c \\beta(c)$", sprintf("%.4f", fosd$max_b)),
                 fila("$\\min_c \\beta(c)$", sprintf("%.4f", fosd$min_b)),
                 fila(sprintf("Valor crítico ($\\alpha = %.2f$)", ALPHA), sprintf("%.4f", fosd$vc)),
                 "\\midrule",
                 fila("Tratadas", sapply(res, `[[`, "tratadas")),
                 fila("Controles", sapply(res, `[[`, "controles")),
                 fila("Observaciones", format(sapply(res, `[[`, "n_obs"), big.mark = ","))),
               "distribucion_acumulada.tex")

cl <- melt(cdf, id.vars = c("outcome", "c"), measure.vars = c("cdf", "cdf_cf"),
           variable.name = "serie", value.name = "F")
cl[, `:=`(serie = factor(fifelse(serie == "cdf", "Con entrada (observada)", "Sin entrada (contrafactual)")),
          combustible = factor(PRECIOS[outcome], levels = PRECIOS))]
fig <- ggplot(cl, aes(c, F, colour = serie, linetype = serie)) +
  geom_line() +
  facet_wrap(~combustible, scales = "free_x") +
  labs(x = "Precio menos su media nacional del mes ($/L)", y = "Distribución acumulada",
       colour = NULL, linetype = NULL)
guardar(fig, "distribucion_acumulada.pdf", alto = 3.2)

# perfil de beta(c) sobre la grilla: desplazamiento de la distribucion por la entrada
cdf[, combustible := factor(PRECIOS[outcome], levels = PRECIOS)]
fig <- ggplot(cdf, aes(100 * k, est)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), alpha = 0.2) +
  geom_line() +
  facet_wrap(~combustible) +
  labs(x = "Cuantil de la distribución del precio menos su media nacional del mes",
       y = expression(beta(c)))
guardar(fig, "distribucion_beta.pdf", alto = 3.2)
