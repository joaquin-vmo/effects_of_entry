# 08_caso_entrada.R
#
# caso aplicado de la asignacion de tratamiento: una entrada en el Gran Santiago con
# circulos de 1, 2, 3 y 5 km y las estaciones presentes ese mes coloreadas por su
# distancia a la entrante. se elige la entrada (cohortes focales, entrante en el Gran
# Santiago, al menos una estacion en cada anillo) con mas estaciones a <= 2 km.
# panel_mensual.csv + entradas.csv + comunas -> output/graficos/caso_entrada.pdf

library(ggplot2)
library(sf)
library(here)

source(here("code", "00_utilidades.R"))

RADIOS  <- c(1, RTREAT, RCTRL_A, RCTRL_B)   # km
VENTANA <- 6.5                              # km alrededor de la entrante que muestra el mapa
GRUPOS  <- c("Tratada (0 a 1 km)", "Tratada (1 a 2 km)", "Excluida (2 a 3 km)",
             "Control amplio (3 a 5 km)", "Control amplio y estricto (5 km o más)")

p <- PANELES[["2012_2026"]]
gs <- comunas_gran_santiago()
gs_union <- st_union(gs)
panel <- leer_panel(p)
e <- fread(file.path(p$dir, "entradas.csv"))[, g := as.IDate(g)][eregion == "metropolitana" & year(g) >= p$focal]
e_sf <- st_transform(st_as_sf(e, coords = c("elon", "elat"), crs = 4326), CRS_M)
e <- e[as.vector(st_within(e_sf, gs_union, sparse = FALSE))]

# estaciones con registro en el mes de la entrada, por anillo
anillos <- function(ev) {
  s <- panel[ym == ev$g & station_key != ev$station_key & !is.na(lat)]
  s[, km := as.vector(dist_km(ev$elat, ev$elon, lat, lon))]
  s[, grupo := factor(GRUPOS[findInterval(km, c(0, RADIOS), left.open = TRUE)], levels = GRUPOS)]
  s[km <= VENTANA]
}
cuenta <- rbindlist(lapply(seq_len(nrow(e)), \(i) {
  n <- table(anillos(e[i])$grupo)
  data.table(i = i, minimo = min(n[1:4]), tratadas = sum(n[1:2]), total = sum(n))
}))
ev <- e[cuenta[minimo >= 1][order(-tratadas, -total)]$i[1]]
s <- anillos(ev)
message(sprintf("entrada %s (%s), %s: %s", ev$station_key, ev$edist, ev$g,
                paste(names(table(s$grupo)), table(s$grupo), collapse = "; ")))

centro <- st_transform(st_as_sf(ev, coords = c("elon", "elat"), crs = 4326), CRS_M)
caja <- st_bbox(st_buffer(centro, VENTANA * 1000))
circulos <- st_sf(radio = RADIOS, geometry = st_buffer(st_geometry(centro)[rep(1, length(RADIOS))], RADIOS * 1000))
pts <- st_transform(st_as_sf(s, coords = c("lon", "lat"), crs = 4326), CRS_M)
nombres <- suppressWarnings(st_point_on_surface(st_crop(gs, caja)))

fig <- ggplot() +
  geom_sf(data = gs, fill = "grey97", colour = "grey70", linewidth = 0.3) +
  geom_sf_text(data = nombres, aes(label = Comuna), size = 1.9, colour = "grey55") +
  geom_sf(data = circulos, fill = NA, colour = "grey25", linetype = "dashed") +
  geom_sf(data = pts, aes(colour = grupo), size = 1.2) +
  geom_sf(data = centro, shape = 8, size = 3, stroke = 1) +
  scale_colour_manual(values = PALETA_ANILLOS, drop = FALSE) +
  coord_sf(xlim = caja[c("xmin", "xmax")], ylim = caja[c("ymin", "ymax")], datum = NA) +
  labs(x = NULL, y = NULL, colour = NULL) +
  theme_void(base_size = 10) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(ncol = 2))
guardar(fig, "caso_entrada.pdf", alto = 4.6, ancho = 4.2)
