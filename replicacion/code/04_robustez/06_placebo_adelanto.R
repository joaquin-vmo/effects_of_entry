# 06_placebo_adelanto.R
#
# estudio de eventos con la entrada fechada un periodo (seis meses) antes de la registrada.
# resolucion mensual en los cuatro bins a cada lado de la entrada registrada (-24 a 24
# meses, extremos agrupados), referencia en el mes previo a la entrada adelantada, muestra
# completa. con bins de seis meses el desplazamiento moveria la figura exactamente un bin.
# panel_mensual.csv -> output/graficos/placebo_adelanto.pdf

library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

DELTA <- BIN_M          # meses que se adelanta la entrada: un periodo
NB_M  <- NBIN * BIN_M   # meses a cada lado de la entrada registrada; los extremos se agrupan

p <- PANELES[["2012_2026"]]
d <- muestra_estacion(leer_panel(p), CTRL_ESTRICTO, p$focal)
# tiempo desde la entrada adelantada, para que la referencia -1 sea el mes previo a ella
d[, relp := fifelse(treated == 1L, pmax(-NB_M, pmin(NB_M, miym - mi(g_entry))) + DELTA, -1L)]

res <- rbindlist(lapply(names(PRECIOS), \(fv)
  tidy_es(feols(as.formula(sprintf("log(%s) * 100 ~ i(relp, treated, ref = -1) | %s", fv, FE_PRINCIPAL)),
                data = d[!is.na(get(fv))], cluster = ~comuna), "relp")[, combustible := PRECIOS[[fv]]]))
res[, `:=`(mes = event_time - DELTA, combustible = factor(combustible, levels = PRECIOS))]

ENTRADAS <- data.table(x = c(-DELTA, 0), tipo = c("Entrada adelantada (placebo)", "Entrada registrada"))
fig <- ggplot(res, aes(mes, estimate)) +
  geom_hline(yintercept = 0) +
  geom_vline(data = ENTRADAS, aes(xintercept = x, linetype = tipo), colour = "grey35") +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), size = 0.15) +
  facet_wrap(~combustible, scales = "free_y") +
  scale_x_continuous(breaks = seq(-NB_M, NB_M, BIN_M)) +
  scale_linetype_manual(values = c("dotted", "dashed")) +
  labs(x = "Meses desde la entrada registrada", y = "Efecto sobre el precio (%)", linetype = NULL) +
  theme(legend.position = "bottom")
ggsave(here("output", "graficos", "placebo_adelanto.pdf"), fig, width = 9, height = 6.5)

print(dcast(res[mes %between% c(-8, 2), .(combustible, mes, v = sprintf("%.2f [%.2f, %.2f]", estimate,
            estimate - 1.96 * se, estimate + 1.96 * se))], mes ~ combustible, value.var = "v"))
