# 01_conley.R
#
# especificacion principal con errores de Conley (1999) a 2 y 5 km junto a los agrupados
# por comuna. mismos coeficientes, solo cambia la varianza. kernel uniforme de fixest,
# no el Bartlett del paper (ver README)
# panel_mensual.csv -> output/graficos/conley_2012_2026.pdf

library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

VCOV <- list(Comuna = ~comuna,
             `Conley 2 km` = conley(RTREAT, distance = "spherical"),
             `Conley 5 km` = conley(RCTRL_B, distance = "spherical"))

for (nombre in "2012_2026") {
  p <- PANELES[[nombre]]
  d <- muestra_estacion(leer_panel(p), CTRL_ESTRICTO, p$focal)

  res <- rbindlist(lapply(names(PRECIOS), \(fv) {
    m <- estimar_es(d[!is.na(get(fv))], fv)
    rbindlist(lapply(names(VCOV), \(v) tidy_es(summary(m, vcov = VCOV[[v]]))[, errores := v]))[
      , combustible := PRECIOS[[fv]]]
  }))
  res[, `:=`(combustible = factor(combustible, levels = PRECIOS),
             errores = factor(errores, levels = names(VCOV)))]

  guardar(grafico_es(res, "errores", dodge = 0.5), sprintf("conley_%s.pdf", nombre), alto = 3.2)
  message("panel ", nombre, " listo")
}
