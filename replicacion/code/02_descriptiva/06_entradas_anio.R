# 06_entradas_anio.R
#
# cantidad de entradas (estaciones nuevas) por año, 2013-2026.
# entradas.csv -> output/graficos/entradas_anio.pdf

library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

entradas <- fread(file.path(PANELES[["2012_2026"]]$dir, "entradas.csv"), select = "g")
entradas[, anio := year(as.IDate(g))]

conteo <- entradas[, .N, by = anio]

fig <- ggplot(conteo, aes(anio, N)) +
  geom_col() +
  scale_x_continuous(breaks = 2013:2026) +
  labs(x = NULL, y = "Entradas")
guardar(fig, "entradas_anio.pdf", alto = 2.4)
