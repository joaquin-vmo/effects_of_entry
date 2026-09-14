# 01_conley.R
#
# especificacion principal con errores de Conley (1999) a 2 y 5 km junto a los agrupados
# por comuna. mismos coeficientes, solo cambia la varianza. kernel uniforme de fixest,
# no el Bartlett del paper (ver README)
# panel_mensual.csv -> output/graficos/conley_<ventana>.pdf

library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

VCOV <- list(Comuna = ~comuna,
             `Conley 2 km` = conley(RTREAT, distance = "spherical"),
             `Conley 5 km` = conley(RCTRL_B, distance = "spherical"))

for (nombre in names(PANELES)) {
  p <- PANELES[[nombre]]
  d <- muestra_estacion(leer_panel(p), CTRL_ESTRICTO, p$focal)

  res <- rbindlist(lapply(names(PRECIOS), \(fv) {
    m <- feols(as.formula(sprintf("log(%s) * 100 ~ i(rel, treated, ref = -1) | %s", fv, FE_PRINCIPAL)),
               data = d[!is.na(get(fv))])
    rbindlist(lapply(names(VCOV), \(v) tidy_es(summary(m, vcov = VCOV[[v]]))[, errores := v]))[
      , combustible := PRECIOS[[fv]]]
  }))
  res[, `:=`(combustible = factor(combustible, levels = PRECIOS),
             errores = factor(errores, levels = names(VCOV)))]

  fig <- ggplot(res, aes(event_time, estimate, colour = errores)) +
    geom_hline(yintercept = 0) +
    geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                    position = position_dodge(width = 0.5)) +
    facet_wrap(~combustible, scales = "free_y") +
    scale_x_continuous(breaks = -NBIN:NBIN) +
    labs(x = "Tiempo desde la entrada", y = "Efecto sobre el precio (%)", colour = NULL) +
    theme(legend.position = "bottom")
  ggsave(here("output", "graficos", sprintf("conley_%s.pdf", nombre)), fig, width = 9, height = 6)
  message("panel ", nombre, " listo")
}
