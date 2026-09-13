# 03_mapa_calor.R
#
# mapa de calor de precios en el Gran Santiago: donde se cobra caro y donde barato,
# descontado el nivel general. porta 22_mapa_calor_precios.R del proyecto anterior
# (su figura mapa_calor.pdf, las cuatro parrillas).
#
# que se grafica: para cada estacion se promedia el precio del anio ANIO y se resta
# el promedio de los promedios de las estaciones del Gran Santiago. la desviacion,
# en $/L, se interpola sobre una grilla regular con escala divergente centrada en 0.
#
# por que nucleo gaussiano y no IDW: IDW converge al dato puntual cerca de cada
# estacion y el mapa se ve como puntos, no como superficie. Nadaraya-Watson con
# nucleo gaussiano, z(s) = sum_i w_i(s) y_i / sum_i w_i(s), w_i(s) = exp(-d^2 / 2h^2),
# suaviza a escala h y deja ver la estructura de barrio. con h = 2 km cada celda
# promedia del orden de 15 a 30 estaciones; 1 km queda ruidoso y 4 km borra el
# contraste oriente-poniente.
#
# donde se corta: solo se pinta la grilla dentro de las 34 comunas del Gran Santiago
# y con al menos MIN_EST estaciones a DMAX_KM o menos. sin eso el suavizador
# extrapola sobre la precordillera y el secano, donde no hay estaciones. el minimo
# de dos descarta islas de una sola estacion (camino a Farellones, sur de San
# Bernardo). la mascara es la misma para los cuatro combustibles.
#
# no se portan del script anterior: el mapa de la 93 sola con nombres de comuna,
# los csv por estacion y por comuna, y el chequeo de cobertura desbalanceada
# (correlacion con la version que descuenta el promedio de cada mes).
#
# toma data/procesado/panel_mensual.csv y data/input/mapas/{comunas,red_vial} (via
# los helpers de mapas de 00_utilidades.R), y
# produce output/graficos/mapa_calor.pdf

library(data.table)
library(ggplot2)
library(sf)
library(here)

source(here("code", "00_utilidades.R"))

ANIO      <- 2025L   # ultimo anio completo del panel
H_KM      <- 2.0     # ancho de banda del nucleo gaussiano
DMAX_KM   <- 2.5     # radio maximo a una estacion para pintar la grilla
MIN_EST   <- 2L      # estaciones minimas a DMAX_KM para pintar una celda
RES_M     <- 250     # lado de la celda de la grilla, en metros

# --- capas de mapa y estaciones (00_utilidades.R) ---
gs <- comunas_gran_santiago()
gs_union <- st_union(gs)
vias <- vias_rm(1)   # autopistas, como referencia visual
est <- estaciones_gran_santiago(gs, ANIO)
pts <- st_as_sf(est, coords = c("x", "y"), crs = CRS_M, remove = FALSE)

# desviacion respecto del promedio de los promedios
for (fv in names(PRECIOS)) est[, (paste0("d_", fv)) := get(fv) - mean(get(fv), na.rm = TRUE)]
message(sprintf("%d: %d estaciones en el Gran Santiago", ANIO, nrow(est)))

# --- grilla, mascara e interpolacion ---
# la grilla cubre el rectangulo de las estaciones dilatado en DMAX_KM, que es lo
# unico que sobrevive a la mascara
h <- H_KM * 1000
dmax <- DMAX_KM * 1000
grilla <- CJ(y = seq(min(est$y) - dmax, max(est$y) + dmax, by = RES_M),
             x = seq(min(est$x) - dmax, max(est$x) + dmax, by = RES_M))
en_gs <- lengths(st_intersects(st_as_sf(grilla, coords = c("x", "y"), crs = CRS_M), gs_union)) > 0

# por bloques de celdas: la matriz celdas x estaciones completa no cabe comoda en memoria
por_bloques <- function(gx, gy, f, paso = 4000L) {
  unlist(lapply(split(seq_along(gx), ceiling(seq_along(gx) / paso)), \(j)
    f(outer(gx[j], est$x, "-")^2 + outer(gy[j], est$y, "-")^2)))
}
n_cerca <- por_bloques(grilla$x, grilla$y, \(d2) rowSums(d2 <= dmax^2))
grilla <- grilla[en_gs & n_cerca >= MIN_EST]

sup <- rbindlist(lapply(names(PRECIOS), \(fv) {
  val <- est[[paste0("d_", fv)]]
  ok <- !is.na(val)
  z <- por_bloques(grilla$x, grilla$y, \(d2) {
    w <- exp(-d2[, ok, drop = FALSE] / (2 * h^2))
    as.vector(w %*% val[ok]) / rowSums(w)
  })
  grilla[, .(x, y, z, combustible = factor(PRECIOS[[fv]], levels = PRECIOS))]
}))

# --- figura ---
# ventana fijada por la superficie pintada; escala simetrica recortada al percentil
# 99 de |z| para que un par de celdas extremas no fije el rango de color
lim_x <- range(sup$x) + c(-1500, 1500)
lim_y <- range(sup$y) + c(-1500, 1500)
tope <- signif(quantile(abs(sup$z), 0.99, names = FALSE), 2)

# TODO nota de la figura: promedio 2025 del precio de cada estacion menos el promedio
# de las estaciones del Gran Santiago, en $/L; suavizado con nucleo gaussiano de 2 km;
# solo zonas con al menos 2 estaciones a 2,5 km; lineas blancas: autopistas urbanas;
# puntos: estaciones; escala recortada en +-percentil 99
fig <- ggplot() +
  geom_tile(data = sup, aes(x, y, fill = z), width = RES_M, height = RES_M) +
  # suppressWarnings: st_crop avisa que los atributos se asumen constantes, da igual aqui
  geom_sf(data = suppressWarnings(st_crop(vias, st_bbox(c(xmin = lim_x[1], xmax = lim_x[2],
                                                          ymin = lim_y[1], ymax = lim_y[2]),
                                                        crs = CRS_M))),
          colour = "white", linewidth = 0.3) +
  geom_sf(data = gs, fill = NA, colour = "grey40", linewidth = 0.15) +
  geom_sf(data = pts, size = 0.2) +
  facet_wrap(~combustible, nrow = 1) +
  # barato en azul, caro en rojo
  scale_fill_gradient2(low = scales::muted("blue"), high = scales::muted("red"),
                       limits = c(-tope, tope), oob = scales::squish) +
  coord_sf(xlim = lim_x, ylim = lim_y, expand = FALSE, datum = NA) +
  labs(x = NULL, y = NULL, fill = "Desviación ($/L)") +
  theme(legend.position = "bottom")
ggsave(here("output", "graficos", "mapa_calor.pdf"), fig, width = 14, height = 6)
