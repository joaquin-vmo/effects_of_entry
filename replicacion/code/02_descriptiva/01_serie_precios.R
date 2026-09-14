# 01_serie_precios.R
#
# precio y margen de venta semanales, promedio simple entre estaciones.
# panel_semanal.csv.gz -> output/graficos/serie_{precios,margen}.pdf

library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

panel <- fread(file.path(PANELES[["2012_2026"]]$dir, "panel_semanal.csv.gz"),
               select = c("station_key", "fuel", "wk", "price", "margin"))
panel[, `:=`(wk = as.IDate(wk), combustible = factor(PRECIOS[paste0("p", fuel)], levels = PRECIOS))]

serie <- panel[, .(precio = mean(price), margen = mean(100 * margin / price, na.rm = TRUE)),
               by = .(wk, combustible)]

fig <- ggplot(serie, aes(wk, precio, colour = combustible)) +
  geom_line() +
  scale_x_date(expand = expansion(mult = c(0, .02)), date_breaks = "2 years", date_labels = "%Y") +
  labs(x = NULL, y = "Precio promedio ($/L)", colour = NULL)
guardar(fig, "serie_precios.pdf", alto = 2.6)

fig <- ggplot(serie[!is.na(margen)], aes(wk, margen, colour = combustible)) +
  geom_line() +
  # drop = FALSE: mismos colores que el grafico de precios, sin la 95
  scale_colour_discrete(drop = FALSE, breaks = unname(PRECIOS[names(PRECIOS) != "p95"])) +
  scale_x_date(expand = expansion(mult = c(0, .02)), date_breaks = "2 years", date_labels = "%Y") +
  labs(x = NULL, y = "Margen de venta (%)", colour = NULL)
guardar(fig, "serie_margen.pdf", alto = 2.6)
