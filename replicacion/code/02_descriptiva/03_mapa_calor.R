# 03_mapa_calor.R
#
# desviacion del precio promedio de cada estacion respecto del Gran Santiago,
# suavizada con nucleo gaussiano. panel_mensual.csv + mapas -> output/graficos/mapa_calor.pdf

library(ggplot2)
library(sf)
library(here)

source(here("code", "00_utilidades.R"))

ANIO      <- 2025L   # ultimo anio completo del panel
H_KM      <- 2.0     # ancho de banda del nucleo
DMAX_KM   <- 2.5     # radio a una estacion para pintar una celda
MIN_EST   <- 2L      # estaciones minimas a DMAX_KM
RES_M     <- 250     # lado de la celda, en metros

gs <- comunas_gran_santiago()
gs_union <- st_union(gs)
vias <- vias_rm(1)
est <- estaciones_gran_santiago(gs, ANIO)
pts <- st_as_sf(est, coords = c("x", "y"), crs = CRS_M, remove = FALSE)

for (fv in names(PRECIOS)) est[, (paste0("d_", fv)) := get(fv) - mean(get(fv), na.rm = TRUE)]
message(sprintf("%d: %d estaciones en el Gran Santiago", ANIO, nrow(est)))

h <- H_KM * 1000
dmax <- DMAX_KM * 1000
grilla <- CJ(y = seq(min(est$y) - dmax, max(est$y) + dmax, by = RES_M),
             x = seq(min(est$x) - dmax, max(est$x) + dmax, by = RES_M))
en_gs <- lengths(st_intersects(st_as_sf(grilla, coords = c("x", "y"), crs = CRS_M), gs_union)) > 0

# por bloques: la matriz celdas x estaciones completa no cabe comoda en memoria
por_bloques <- function(gx, gy, f, paso = 4000L) {
  unlist(lapply(split(seq_along(gx), ceiling(seq_along(gx) / paso)), \(j)
    f(outer(gx[j], est$x, "-")^2 + outer(gy[j], est$y, "-")^2)))
}
n_cerca <- por_bloques(grilla$x, grilla$y, \(d2) rowSums(d2 <= dmax^2))
grilla <- grilla[en_gs & n_cerca >= MIN_EST]

# Nadaraya-Watson
sup <- rbindlist(lapply(names(PRECIOS), \(fv) {
  val <- est[[paste0("d_", fv)]]
  ok <- !is.na(val)
  z <- por_bloques(grilla$x, grilla$y, \(d2) {
    w <- exp(-d2[, ok, drop = FALSE] / (2 * h^2))
    as.vector(w %*% val[ok]) / rowSums(w)
  })
  grilla[, .(x, y, z, combustible = factor(PRECIOS[[fv]], levels = PRECIOS))]
}))

lim_x <- range(sup$x) + c(-1500, 1500)
lim_y <- range(sup$y) + c(-1500, 1500)
tope <- signif(quantile(abs(sup$z), 0.99, names = FALSE), 2)   # escala recortada al p99

fig <- ggplot() +
  geom_tile(data = sup, aes(x, y, fill = z), width = RES_M, height = RES_M) +
  geom_sf(data = suppressWarnings(st_crop(vias, st_bbox(c(xmin = lim_x[1], xmax = lim_x[2],
                                                          ymin = lim_y[1], ymax = lim_y[2]),
                                                        crs = CRS_M))),
          colour = "white", linewidth = 0.3) +
  geom_sf(data = gs, fill = NA, colour = "grey40", linewidth = 0.15) +
  geom_sf(data = pts, size = 0.2) +
  facet_wrap(~combustible, nrow = 1) +
  scale_fill_gradient2(low = PALETA_DIVERGENTE[["bajo"]], high = PALETA_DIVERGENTE[["alto"]],
                       limits = c(-tope, tope), oob = scales::squish) +
  coord_sf(xlim = lim_x, ylim = lim_y, expand = FALSE, datum = NA) +
  labs(x = NULL, y = NULL, fill = "Desviación ($/L)")
guardar(fig, "mapa_calor.pdf", alto = 2.4)
