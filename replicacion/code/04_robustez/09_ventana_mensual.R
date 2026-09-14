# 09_ventana_mensual.R
#
# especificacion principal (TWFE) en la ventana 2021_2026 con tiempo-evento en meses en vez
# de semestres: de -3 a 5 meses desde la entrada, extremos agrupados, referencia en -1.
# con cohortes desde 2022 y fin en febrero de 2026, los bins de seis meses son gruesos.
# panel_mensual.csv de la ventana -> output/graficos/ventana_mensual_2021_2026.pdf

library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

MESES <- -3:5   # meses desde la entrada; los extremos agrupan todo lo anterior y lo posterior

p <- PANELES[["2021_2026"]]
d <- muestra_estacion(leer_panel(p), CTRL_ESTRICTO, p$focal)
d[, rel := fifelse(treated == 1L, pmax(min(MESES), pmin(max(MESES), miym - mi(g_entry))), -1L)]

res <- rbindlist(lapply(names(PRECIOS), \(fv) {
  x <- d[!is.na(get(fv))]
  m <- estimar_es(x, fv)
  n <- n_estaciones(m, x)
  message(sprintf("%s: %d tratadas, %d controles", fv, n[1], n[2]))
  tidy_es(m)[, combustible := factor(PRECIOS[[fv]], levels = PRECIOS)]
}))
print(dcast(res[, .(combustible, event_time, v = sprintf("%.2f [%.2f, %.2f]", estimate,
                                                         estimate - 1.96 * se, estimate + 1.96 * se))],
            event_time ~ combustible, value.var = "v"))

# plotmath: el dispositivo pdf no tiene los caracteres de menor o igual y mayor o igual
etiquetas <- parse(text = c(sprintf("''<=%d", min(MESES)), MESES[-c(1, length(MESES))], sprintf("''>=%d", max(MESES))))
fig <- suppressMessages(grafico_es(res) +   # reemplaza el eje en semestres de grafico_es()
  scale_x_continuous(breaks = MESES, labels = etiquetas) +
  labs(x = "Meses desde la entrada"))
guardar(fig, "ventana_mensual_2021_2026.pdf", alto = 3.2)
