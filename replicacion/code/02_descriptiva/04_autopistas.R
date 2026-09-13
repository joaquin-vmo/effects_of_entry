# 04_autopistas.R
#
# precio y distancia a la autopista mas cercana en el Gran Santiago. version
# descriptiva de 23_autopistas_precio.R del proyecto anterior (figura
# autopistas_bandas.pdf), como scatter estacion por estacion en vez de medias por
# banda de distancia.
#
# la pregunta: si las estaciones pegadas a una autopista cobran mas (demanda de
# paso, costo de busqueda alto). las autopistas no se trazaron al azar: cruzan el
# sector oriente, que es caro por razones ajenas a la carretera. por eso el eje y
# es la desviacion del precio respecto del promedio de la PROPIA COMUNA: compara
# estaciones del mismo barrio, una junto a la autopista y otra no. sigue siendo
# descriptivo, no causal.
#
# autopista = clase 1 del inventario del MOP (concesionadas urbanas y rutas
# nacionales). se mide la distancia al EJE de la via, no a un enlace: una estacion
# a 80 m de una autopista sin acceso cercano entra como cercana, lo que atenua
# cualquier patron. en el proyecto anterior la desviacion era de unos +10 $/L a
# 200 m o menos y negativa entre 200 y 500 m.
#
# no se portan: las regresiones con efectos fijos de comuna y marca, la version con
# avenidas principales (clase 2) y los csv.
#
# toma data/procesado/panel_mensual.csv y data/input/mapas/{comunas,red_vial} (via
# los helpers de mapas de 00_utilidades.R), y produce output/graficos/autopistas.pdf

library(data.table)
library(ggplot2)
library(sf)
library(here)

source(here("code", "00_utilidades.R"))

ANIO <- 2025L

gs <- comunas_gran_santiago()
est <- estaciones_gran_santiago(gs, ANIO)
pts <- st_as_sf(est, coords = c("x", "y"), crs = CRS_M)
est[, d_auto := as.numeric(st_distance(pts, st_union(vias_rm(1))))]

l <- melt(est, id.vars = c("station_key", "comuna", "d_auto"), measure.vars = names(PRECIOS),
          variable.name = "outcome", value.name = "precio", variable.factor = FALSE)
l <- l[!is.na(precio)]
l[, `:=`(dev_com = precio - mean(precio), n_com = .N), by = .(outcome, comuna)]
l <- l[n_com > 1]   # una estacion sola en su comuna tiene desviacion 0 por construccion
l[, combustible := factor(PRECIOS[outcome], levels = PRECIOS)]
# fuera de la figura las estaciones a mas de 5 km: son un punado en el borde de la
# ciudad y estiran el eje (y el loess) sobre un tramo casi sin datos
message(sprintf("%d: %d estaciones en %d comunas; %d a mas de 5 km quedan fuera", ANIO,
                uniqueN(l$station_key), uniqueN(l$comuna), uniqueN(l[d_auto > 5000, station_key])))
l <- l[d_auto <= 5000]

# TODO nota de la figura: cada punto es una estacion del Gran Santiago; precio
# promedio 2025 menos el promedio de las estaciones de su comuna, en $/L; distancia
# al eje de la autopista mas cercana (clase 1 del MOP), escala logaritmica, hasta
# 5 km; linea punteada: 200 m; curva: loess con intervalo al 95%
fig <- ggplot(l, aes(d_auto, dev_com)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 200, linetype = "dashed") +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", formula = y ~ x) +
  facet_wrap(~combustible, nrow = 1) +
  scale_x_log10(breaks = c(10, 50, 200, 500, 1000, 5000)) +
  labs(x = "Distancia a la autopista (m)", y = "Desviación del promedio comunal ($/L)")
ggsave(here("output", "graficos", "autopistas.pdf"), fig, width = 14, height = 5)
