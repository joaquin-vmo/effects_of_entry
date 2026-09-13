# 07_distribucion_acumulada.R
#
# cambio en la distribucion acumulada de precios por efecto de la entrada: regresion
# de distribucion de Chernozhukov, Fernandez-Val y Melly (2013), como en la seccion
# 4.2 de Fischer, Martin y Schmidt-Dengler (2025). porta las partes B y C de
# 18_distribucional.R del proyecto anterior (dist_cdf.pdf, tab_dist_beta.tex,
# tab_dist_fosd.tex).
#
# para cada umbral c de una grilla (los cuantiles 2 a 98 del resultado) se estima
#     1[y <= c] = beta(c) * post + efectos fijos,
# de modo que beta(c) es el cambio que la entrada provoca en la acumulada evaluada en
# c. la acumulada observada de las tratadas despues de la entrada menos beta(c) es la
# CONTRAFACTUAL: la distribucion que habrian tenido sin la entrada. beta(c) > 0 es
# mas masa por debajo de c, es decir, precios mas bajos.
#
# dominancia estocastica de primer orden: si beta(c) >= 0 en todo c, la distribucion
# con entrada domina a la contrafactual (la entrada baja precios en toda la
# distribucion, no solo en promedio). estadistico max_c beta(c) contra el valor
# critico unilateral de Gail y Green (1976), sqrt(-log(alpha) / n).
#
# el resultado es el precio DESVIADO de su media nacional del mes, no el precio en
# nivel: el panel cubre un ciclo completo del petroleo y en nivel el cuantil 10 es
# "un mes barato", no "una estacion barata" (el mes explica ~99% de la varianza del
# precio). la media del mes se calcula sobre todo el panel, no sobre la muestra.
#
# setting de la estimacion principal: muestra_estacion() con control estricto (nunca
# una entrada a menos de RCTRL_B km), FE_PRINCIPAL, cluster por comuna, panel
# completo. el proyecto anterior usaba control amplio. no se porta la parte A (efectos
# cuantilicos incondicionales de Firpo, Fortin y Lemieux).
#
# toma data/procesado/panel_mensual.csv y produce output/graficos/distribucion_acumulada.pdf
# y output/tablas/distribucion_acumulada.tex

library(data.table)
library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

NC    <- 41L                     # umbrales de la grilla
KREF  <- c(0.10, 0.25, 0.50, 0.75, 0.90)   # cuantiles que van a la tabla
ALPHA <- 0.01                    # nivel del test de dominancia

p <- PANELES[["2012_2026"]]
panel <- fread(file.path(p$dir, "panel_mensual.csv"))
panel[, `:=`(ym = as.IDate(ym), g_entry = as.IDate(g_entry), g2_entry = as.IDate(g2_entry))]
panel[, year := year(ym)]

d <- muestra_estacion(panel, CTRL_ESTRICTO, p$focal)
d[, post := as.integer(treated == 1L & ym >= g_entry)]
for (fv in names(PRECIOS)) {
  ref <- panel[!is.na(get(fv)), .(mref = mean(get(fv))), by = miym]
  d[ref, on = "miym", (paste0(fv, "_dev")) := get(fv) - i.mref]
}

res <- lapply(names(PRECIOS), \(fv) {
  yv <- paste0(fv, "_dev")
  z <- d[!is.na(get(yv))]
  ks <- seq(0.02, 0.98, length.out = NC)
  cs <- quantile(z[[yv]], ks, names = FALSE)
  ind <- sprintf("ind%02d", seq_along(cs))
  z[, (ind) := lapply(cs, \(cc) as.numeric(get(yv) <= cc))]
  # todos los umbrales en una sola llamada (varias variables dependientes)
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

# dominancia estocastica
fosd <- data.table(outcome = names(PRECIOS),
                   max_b = cdf[, max(est), by = outcome]$V1,
                   min_b = cdf[, min(est), by = outcome]$V1,
                   vc = sqrt(-log(ALPHA) / sapply(res, `[[`, "n_trat_post")))
print(fosd)

# --- tabla: beta(c) en los cuantiles de referencia, test de dominancia y muestra ---
fila <- function(...) paste(paste(c(...), collapse = " & "), "\\\\")
estrellas <- \(p) fifelse(p < 0.01, "$^{***}$", fifelse(p < 0.05, "$^{**}$", fifelse(p < 0.10, "$^{*}$", "")))
ref <- cdf[, .SD[vapply(KREF, \(kk) which.min(abs(k - kk)), integer(1))], by = outcome]
ref[, kref := rep(KREF, length(PRECIOS))]
cuerpo <- unlist(lapply(KREF, \(kk) {
  r <- ref[kref == kk][match(names(PRECIOS), outcome)]
  c(fila(sprintf("Cuantil %d", round(100 * kk)), sprintf("%.4f%s", r$est, estrellas(r$p))),
    fila("", sprintf("(%.4f)", r$se)))
}))
tabla <- c("\\begin{tabular}{lcccc}", "\\toprule",
           fila("", unname(PRECIOS)), "\\midrule",
           cuerpo, "\\midrule",
           fila("$\\max_c \\beta(c)$", sprintf("%.4f", fosd$max_b)),
           fila("$\\min_c \\beta(c)$", sprintf("%.4f", fosd$min_b)),
           fila(sprintf("Valor crítico ($\\alpha = %.2f$)", ALPHA), sprintf("%.4f", fosd$vc)),
           "\\midrule",
           fila("Tratadas", sapply(res, `[[`, "tratadas")),
           fila("Controles", sapply(res, `[[`, "controles")),
           fila("Observaciones", format(sapply(res, `[[`, "n_obs"), big.mark = ",")),
           "\\bottomrule", "\\end{tabular}")
# TODO nota de la tabla: beta(c) = efecto de la entrada sobre 1[precio <= c], con c el
# cuantil indicado del precio menos su media nacional del mes; regresion de
# distribucion de Chernozhukov, Fernandez-Val y Melly (2013); beta(c) > 0 = mas masa
# bajo c (precios mas bajos); dominancia estocastica de primer orden si max beta(c)
# supera el valor critico de Gail y Green (1976) y min beta(c) no es negativo;
# especificacion principal (efectos fijos de estacion, mes, region x ano y marca x ano);
# control: estaciones sin entradas a menos de 5 km; errores estandar agrupados por
# comuna entre parentesis; * p<0,10 ** p<0,05 *** p<0,01
writeLines(tabla, here("output", "tablas", "distribucion_acumulada.tex"))

# --- figura: acumulada observada y contrafactual ---
cl <- melt(cdf, id.vars = c("outcome", "c"), measure.vars = c("cdf", "cdf_cf"),
           variable.name = "serie", value.name = "F")
cl[, `:=`(serie = factor(fifelse(serie == "cdf", "Con entrada (observada)", "Sin entrada (contrafactual)")),
          combustible = factor(PRECIOS[outcome], levels = PRECIOS))]
# TODO nota de la figura: acumulada del precio menos su media nacional del mes para
# las tratadas despues de la entrada (observada) y la que habrian tenido sin ella
# (observada menos beta(c)); regresion de distribucion sobre 41 umbrales;
# especificacion principal; control a mas de 5 km
fig <- ggplot(cl, aes(c, F, colour = serie, linetype = serie)) +
  geom_line() +
  facet_wrap(~combustible, scales = "free_x") +
  labs(x = "Precio menos su media nacional del mes ($/L)", y = "Distribución acumulada",
       colour = NULL, linetype = NULL) +
  theme(legend.position = "bottom")
ggsave(here("output", "graficos", "distribucion_acumulada.pdf"), fig, width = 9, height = 6)
