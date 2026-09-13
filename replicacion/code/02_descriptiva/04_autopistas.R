# 04_autopistas.R
#
# desviacion del precio respecto del promedio comunal vs distancia a la autopista mas
# cercana, Gran Santiago. panel_mensual.csv + mapas -> output/graficos/autopistas.pdf

library(ggplot2)
library(sf)
library(here)

source(here("code", "00_utilidades.R"))

ANIO <- 2025L
MAX_M <- 5000   # distancia maxima graficada

gs <- comunas_gran_santiago()
est <- estaciones_gran_santiago(gs, ANIO)
pts <- st_as_sf(est, coords = c("x", "y"), crs = CRS_M)
est[, d_auto := as.numeric(st_distance(pts, st_union(vias_rm(1))))]

l <- melt(est, id.vars = c("station_key", "comuna", "d_auto"), measure.vars = names(PRECIOS),
          variable.name = "outcome", value.name = "precio", variable.factor = FALSE)
l <- l[!is.na(precio)]
l[, `:=`(dev_com = precio - mean(precio), n_com = .N), by = .(outcome, comuna)]
l <- l[n_com > 1]   # sola en su comuna: desviacion 0 por construccion
l[, combustible := factor(PRECIOS[outcome], levels = PRECIOS)]
message(sprintf("%d: %d estaciones en %d comunas; %d a mas de 5 km quedan fuera", ANIO,
                uniqueN(l$station_key), uniqueN(l$comuna), uniqueN(l[d_auto > MAX_M, station_key])))
l <- l[d_auto <= MAX_M]

fig <- ggplot(l, aes(d_auto, dev_com)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 200, linetype = "dashed") +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", formula = y ~ x) +
  facet_wrap(~combustible, nrow = 1) +
  scale_x_log10(breaks = c(10, 50, 200, 500, 1000, 5000)) +
  labs(x = "Distancia a la autopista (m)", y = "Desviación del promedio comunal ($/L)")
ggsave(here("output", "graficos", "autopistas.pdf"), fig, width = 14, height = 5)
