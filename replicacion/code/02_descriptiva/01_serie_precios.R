# 01_serie_precios.R
#
# trayectoria semanal del precio y del margen, promedio simple entre estaciones,
# con el panel semanal completo.
#   - precio: $/L nominales, los cuatro combustibles, 2012 en adelante
#   - margen de venta: (precio - costo MEPCO) / precio, en %, desde que hay MEPCO
#     (2014-08-07). la 95 no tiene margen: el MEPCO no publica referencia para ella
#
# toma data/procesado/panel_semanal.csv.gz y produce
# output/graficos/serie_precios.pdf y output/graficos/serie_margen.pdf

library(data.table)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

panel <- fread(file.path(PANELES[["2012_2026"]]$dir, "panel_semanal.csv.gz"),
               select = c("station_key", "fuel", "wk", "price", "margin"))
panel[, `:=`(wk = as.IDate(wk), combustible = factor(PRECIOS[paste0("p", fuel)], levels = PRECIOS))]

serie <- panel[, .(precio = mean(price), margen = mean(100 * margin / price, na.rm = TRUE)),
               by = .(wk, combustible)]

# TODO nota de la figura: promedio simple semanal entre estaciones; precios nominales
# en $/L; el precio de una semana sin reporte es el ultimo informado (hasta 13 semanas)
fig <- ggplot(serie, aes(wk, precio, colour = combustible)) +
  geom_line() +
  labs(x = NULL, y = "Precio promedio ($/L)", colour = NULL)
ggsave(here("output", "graficos", "serie_precios.pdf"), fig, width = 9, height = 5)

# TODO nota de la figura: margen = (precio - precio mayorista con MEPCO) / precio;
# promedio simple semanal entre estaciones; la gasolina 95 no tiene referencia MEPCO
fig <- ggplot(serie[!is.na(margen)], aes(wk, margen, colour = combustible)) +
  geom_line() +
  # drop = FALSE: cada combustible conserva el color del grafico de precios
  scale_colour_discrete(drop = FALSE, breaks = unname(PRECIOS[names(PRECIOS) != "p95"])) +
  labs(x = NULL, y = "Margen de venta (%)", colour = NULL)
ggsave(here("output", "graficos", "serie_margen.pdf"), fig, width = 9, height = 5)
