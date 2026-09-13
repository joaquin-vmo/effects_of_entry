# 03_event_study_sunab.R
#
# TWFE frente a Sun y Abraham (2021), misma muestra, efectos fijos y bins que 01, por
# ventana. panel_mensual.csv -> output/graficos/sunab_<ventana>.pdf

library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

COH_NUNCA <- 99999L   # cohorte fuera del rango de meses: sunab() la trata como nunca tratada

# sunab() no acepta bins: se le pasa per = cohorte + rel, de modo que per - cohorte = bin
estimar <- function(d, fv) {
  x <- d[!is.na(get(fv))]
  x[, cohorte := fifelse(treated == 1L, mi(g_entry), COH_NUNCA)]
  x[, per := fifelse(treated == 1L, cohorte + rel, miym)]
  f <- \(rhs) feols(as.formula(sprintf("log(%s) * 100 ~ %s | %s", fv, rhs, FE_PRINCIPAL)),
                    data = x, cluster = ~comuna)
  rbind(tidy_es(f("i(rel, treated, ref = -1)"), "rel")[, estimador := "TWFE"],
        tidy_es(f("sunab(cohorte, per)"), "per")[, estimador := "Sun y Abraham"])[, outcome := fv]
}

for (nombre in names(PANELES)) {
  p <- PANELES[[nombre]]
  d <- muestra_estacion(leer_panel(p), CTRL_ESTRICTO, p$focal)
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
