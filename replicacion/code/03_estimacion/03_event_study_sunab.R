# 03_event_study_sunab.R
#
# TWFE frente a Sun y Abraham (2021), misma muestra, efectos fijos y bins que 01, ventana
# principal (las cortas estan en 04_robustez/08_ventanas_cortas.R).
# panel_mensual.csv -> output/graficos/sunab_2012_2026.pdf

library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

estimar <- function(d, fv) {
  x <- cohortes_sunab(d[!is.na(get(fv))])
  rbind(tidy_es(estimar_es(x, fv), "rel")[, estimador := "TWFE"],
        tidy_es(estimar_es(x, fv, "sunab(cohorte, per)"), "per")[, estimador := "Sun y Abraham"])[, outcome := fv]
}

for (nombre in "2012_2026") {
  p <- PANELES[[nombre]]
  d <- muestra_estacion(leer_panel(p), CTRL_ESTRICTO, p$focal)
  res <- rbindlist(lapply(names(PRECIOS), estimar, d = d))
  res[, `:=`(combustible = factor(PRECIOS[outcome], levels = PRECIOS),
             estimador = factor(estimador, levels = c("TWFE", "Sun y Abraham")))]

  guardar(grafico_es(res, "estimador"), sprintf("sunab_%s.pdf", nombre), alto = 3.2)
  message("panel ", nombre, " listo")
  print(dcast(res[, .(outcome, estimador, event_time, est = round(estimate, 2))],
              outcome + event_time ~ estimador, value.var = "est")[event_time %in% c(-4, -2, 0, 2, 4)])
}
