# 03_event_study_sunab.R
#
# estudio de eventos con el estimador de Sun y Abraham (2021) frente a TWFE, sobre el
# panel MENSUAL: misma muestra, mismos efectos fijos (FE_PRINCIPAL) y mismos bins que
# la estimacion principal (01_event_study.R); lo unico que cambia es el estimador.
# control estricto (nunca una entrada a menos de RCTRL_B km), una corrida por ventana.
#
# TWFE con cohortes escalonadas mezcla comparaciones entre cohortes y puede ponderar
# negativamente si el efecto varia entre ellas. Sun y Abraham estima un efecto por
# cohorte x periodo relativo contra las nunca tratadas y los promedia con el peso de
# cada cohorte en ese periodo relativo.
#
# cohortes MENSUALES: la cohorte es el mes de la entrada (mi(g_entry)). los periodos
# relativos se agrupan en los bins de BIN_M meses de la estimacion principal (+-NBIN,
# extremos agrupados, referencia -1), para que TWFE y Sun y Abraham se comparen bin a
# bin. como sunab() no acepta bin.rel desde una variable, se le pasa como periodo
# `cohorte + rel`: sunab() calcula periodo - cohorte, que entonces es exactamente el
# bin. el periodo de sunab() solo define el tiempo relativo; el efecto fijo de mes va
# en FE_PRINCIPAL. las nunca tratadas llevan una cohorte fuera del rango (COH_NUNCA).
# verificado contra el estimador armado a mano (interacciones cohorte x bin ponderadas
# por participacion de cada cohorte): coeficientes y ee identicos.
#
# el proyecto anterior (09_sunab_persistencia.R) lo estimaba sobre un panel semestral.
#
# toma data/procesado/.../panel_mensual.csv de cada ventana y produce
# output/graficos/sunab_<ventana>.pdf

library(data.table)
library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

COH_NUNCA <- 99999L   # cohorte fuera del rango de meses: sunab() la trata como nunca tratada

estimar <- function(d, fv) {
  x <- d[!is.na(get(fv))]
  x[, cohorte := fifelse(treated == 1L, mi(g_entry), COH_NUNCA)]
  x[, per := fifelse(treated == 1L, cohorte + rel, miym)]   # per - cohorte = rel (bin)
  f <- \(rhs) feols(as.formula(sprintf("log(%s) * 100 ~ %s | %s", fv, rhs, FE_PRINCIPAL)),
                    data = x, cluster = ~comuna)
  rbind(tidy_es(f("i(rel, treated, ref = -1)"), "rel")[, estimador := "TWFE"],
        tidy_es(f("sunab(cohorte, per)"), "per")[, estimador := "Sun y Abraham"])[, outcome := fv]
}

# TODO nota de la figura: panel mensual; Sun y Abraham (2021) con cohortes mensuales
# (mes de la entrada) y nunca tratadas como referencia; bins de seis meses desde la
# entrada, extremos agrupados; referencia en -1; misma muestra y efectos fijos que la
# estimacion principal; intervalos al 95% con errores agrupados por comuna; control a
# mas de 5 km
for (nombre in names(PANELES)) {
  p <- PANELES[[nombre]]
  panel <- fread(file.path(p$dir, "panel_mensual.csv"))
  panel[, `:=`(ym = as.IDate(ym), g_entry = as.IDate(g_entry), g2_entry = as.IDate(g2_entry))]
  panel[, year := year(ym)]

  d <- muestra_estacion(panel, CTRL_ESTRICTO, p$focal)
  res <- rbindlist(lapply(names(PRECIOS), estimar, d = d))
  res[, `:=`(combustible = factor(PRECIOS[outcome], levels = PRECIOS),
             estimador = factor(estimador, levels = c("TWFE", "Sun y Abraham")))]

  fig <- ggplot(res, aes(event_time, estimate, colour = estimador)) +
    geom_hline(yintercept = 0) +
    geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                    position = position_dodge(width = 0.4)) +
    facet_wrap(~combustible, scales = "free_y") +
    scale_x_continuous(breaks = -NBIN:NBIN) +
    labs(x = "Tiempo desde la entrada", y = "Efecto sobre el precio (%)", colour = NULL)
  ggsave(here("output", "graficos", sprintf("sunab_%s.pdf", nombre)), fig, width = 9, height = 6)
  message("panel ", nombre, " listo")
  print(dcast(res[, .(outcome, estimador, event_time, est = round(estimate, 2))],
              outcome + event_time ~ estimador, value.var = "est")[event_time %in% c(-4, -2, 0, 2, 4)])
}
