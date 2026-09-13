# 00_utilidades.R
#
# parametros y funciones comunes. los scripts leen de aqui todo numero que define la
# muestra o la especificacion. su justificacion esta en 01_buid/README.md y
# 03_estimacion/README.md. rutas con here(), raiz en replicacion/ (abrir code.Rproj)

library(data.table)
library(here)

# ==============================================================================
# parametros del build
# ==============================================================================

R_EARTH  <- 6371                   # km
WEEK0    <- as.Date("2012-01-05")  # jueves ancla de la grilla semanal
MAXGAP_W <- 13L                    # semanas maximas de arrastre de un precio
LINK_M   <- 50                     # m: id nuevo a esta distancia de uno apagado = mismo local

RTREAT   <- 2    # km: tratada si hubo una entrada competidora a <= RTREAT
RCTRL_A  <- 3    # km: control amplio, nunca una entrada a < RCTRL_A
RCTRL_B  <- 5    # km: control estricto, nunca una entrada a < RCTRL_B
SOLO_COMPETIDORAS <- TRUE   # FALSE: tambien trata una entrada de la misma marca
FOCAL_FROM <- 2014L         # primer anio de cohorte del panel completo

# ventanas: base = estaciones presentes en `desde`; cohortes desde `focal`
PANELES <- list(
  "2012_2026" = list(desde = 2012L, hasta = "2026-12-31", focal = FOCAL_FROM,
                     dir = here("data", "procesado")),
  "2014_2019" = list(desde = 2014L, hasta = "2019-12-31", focal = 2015L,
                     dir = here("data", "procesado", "panel_2014_2019")),
  "2021_2026" = list(desde = 2021L, hasta = "2026-02-28", focal = 2022L,
                     dir = here("data", "procesado", "panel_2021_2026"))
)

# ==============================================================================
# parametros del analisis
# ==============================================================================

CTRL_AMPLIO   <- c("ctrl_a", "ctrl_b")   # valores de role_entry
CTRL_ESTRICTO <- "ctrl_b"

BIN_M <- 6L   # meses por bin de tiempo-evento
NBIN  <- 4L   # bins a cada lado; los extremos se agrupan

FE_PRINCIPAL <- "station_key + ym + region^year + distribuidor^year"

# agregados del mercado local (06_mercado_min_media_max.R y 08_honestdid.R)
MIN_COMP  <- 1L   # competidoras minimas, ademas de la focal, en promedio antes de la entrada
CUANTILES <- c(p90 = 0.90, p50 = 0.50, p10 = 0.10)
SERIES    <- c(p90 = "Percentil 90", p50 = "Mediana", p10 = "Percentil 10")
VERSIONES <- c(con = "Con entrante", sin = "Sin entrante")

PRECIOS <- c(p93 = "Gasolina 93", p95 = "Gasolina 95",
             p97 = "Gasolina 97", pdi = "Diésel")

# ==============================================================================
# formato de tablas
# ==============================================================================

DICT <- c(setNames(c(sprintf("$\\leq -%d$", NBIN), sprintf("$%d$", -(NBIN - 1):(NBIN - 1)),
                     sprintf("$\\geq %d$", NBIN)),
                   sprintf("rel::%d:treated", -NBIN:NBIN)),
          treated = "Tratada", n = "Observaciones", ym = "Mes", station_key = "Estación",
          `region^year` = "Región $\\times$ año", `distribuidor^year` = "Marca $\\times$ año")

# defaults de etable(); el dict de cada llamada se agrega a DICT
fixest::setFixest_dict(DICT)
fixest::setFixest_etable(digits = "r3", fitstat = ~n, float = FALSE, depvar = FALSE,
                         style.tex = fixest::style.tex("aer", yesNo = c("Sí", "No"),
                                                       fixef.suffix = "", tablefoot = FALSE))

fila <- function(...) paste(paste(c(...), collapse = " & "), "\\\\")
estrellas <- \(p) fifelse(p < 0.01, "$^{***}$", fifelse(p < 0.05, "$^{**}$", fifelse(p < 0.10, "$^{*}$", "")))

# ==============================================================================
# calendario: indices enteros para la aritmetica de tiempo-evento
# ==============================================================================

mi <- function(d) {                          # mes continuo
  d <- as.IDate(d)
  year(d) * 12L + (month(d) - 1L)
}

mi2date <- function(m) {
  out <- rep(NA_integer_, length(m))
  ok <- !is.na(m)
  out[ok] <- as.integer(as.IDate(sprintf("%d-%02d-01",
                                         m[ok] %/% 12L, m[ok] %% 12L + 1L)))
  as.IDate(out)
}

# ==============================================================================
# distancia
# ==============================================================================

# haversine en km, matriz completa. h se topa en 1: el error de punto flotante
# daria NaN justo en pares casi coincidentes
dist_km <- function(lat1, lon1, lat2 = lat1, lon2 = lon1) {
  a1 <- lat1 * pi / 180; o1 <- lon1 * pi / 180
  a2 <- lat2 * pi / 180; o2 <- lon2 * pi / 180
  h <- sin(outer(a1, a2, "-") / 2)^2 +
    outer(cos(a1), cos(a2)) * sin(outer(o1, o2, "-") / 2)^2
  h[h > 1] <- 1
  2 * R_EARTH * asin(sqrt(h))
}

dist_propia <- function(lat, lon) {          # diagonal Inf: nadie compite consigo
  D <- dist_km(lat, lon)
  diag(D) <- Inf
  D
}

coords_estacion <- function(panel) {         # una coordenada por estacion
  s <- unique(panel[!is.na(lat), .(station_key, lat, lon)])
  s[, .SD[1], by = station_key]
}

modal <- function(x) {                       # valor mas frecuente, sin NA
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ==============================================================================
# estimacion
# ==============================================================================

leer_panel <- function(p) {
  panel <- fread(file.path(p$dir, "panel_mensual.csv"))
  panel[, `:=`(ym = as.IDate(ym), g_entry = as.IDate(g_entry), g2_entry = as.IDate(g2_entry))]
  panel[, year := year(ym)][]
}

# tratadas + controles `roles`, cortada en la segunda entrada, sin cohortes previas a
# `focal`, solo estaciones de base. rel: bin de BIN_M meses (+-NBIN); controles en -1
muestra_estacion <- function(panel, roles, focal) {
  d <- panel[role_entry %in% c("treated", roles)]
  d <- d[is.na(g2_entry) | ym < g2_entry]
  d <- d[!(role_entry == "treated" & year(g_entry) < focal)]
  d <- d[base == TRUE]
  d[, treated := as.integer(role_entry == "treated")]
  d[, rel := fifelse(treated == 1L,
                     pmax(-NBIN, pmin(NBIN, (miym - mi(g_entry)) %/% BIN_M)),
                     -1L)]
  d[]
}

# coeficientes `patron`::k de un modelo fixest, con la referencia -1 en cero
tidy_es <- function(m, patron = "rel") {
  ct <- fixest::coeftable(m)
  ct <- ct[grepl(sprintf("^%s::", patron), rownames(ct)), , drop = FALSE]
  d <- data.table(event_time = as.integer(sub(sprintf("^%s::(-?\\d+).*", patron), "\\1",
                                              rownames(ct))),
                  estimate = ct[, 1], se = ct[, 2])
  setorder(rbind(d, data.table(event_time = -1L, estimate = 0, se = 0)), event_time)[]
}

# tratadas y controles que usa el modelo (sin singletons descartados)
n_estaciones <- function(m, d) {
  u <- d[fixest::obs(m)]
  c(uniqueN(u[treated == 1L, station_key]), uniqueN(u[treated == 0L, station_key]))
}

# mercado de cada focal de `d` en el combustible fv: la focal (con precio) y las
# estaciones con precio a <= RTREAT km en el mes. agrega n y, por cuantil,
# <nombre>_con (todas) y <nombre>_sin (sin `entrantes`). filtra mercados con al menos
# min_comp competidoras en promedio antes de la entrada
mercado_local <- function(panel, d, entrantes, fv, cuantiles, min_comp) {
  sloc <- coords_estacion(panel)
  ix <- which(dist_km(sloc$lat, sloc$lon) <= RTREAT, arr.ind = TRUE)   # diagonal 0: incluye a la focal
  edges <- data.table(station_key = sloc$station_key[ix[, 1]], miembro = sloc$station_key[ix[, 2]])
  edges <- edges[station_key %in% d$station_key]
  px <- panel[!is.na(get(fv)), .(miembro = station_key, miym, p = get(fv),
                                 entrante = station_key %in% entrantes)]
  mk <- merge(edges, px, by = "miembro", allow.cartesian = TRUE)
  mk <- mk[, if (any(miembro == station_key)) {
    q <- p[!entrante | miembro == station_key]     # la focal nunca es entrante (base)
    c(list(n = .N),
      setNames(as.list(quantile(p, cuantiles, names = FALSE)), paste0(names(cuantiles), "_con")),
      setNames(as.list(quantile(q, cuantiles, names = FALSE)), paste0(names(cuantiles), "_sin")))
  }, by = .(station_key, miym)]
  z <- merge(d, mk, by = c("station_key", "miym"))
  n_pre <- z[treated == 0L | ym < g_entry, .(n_pre = mean(n)), by = station_key]
  z[station_key %in% n_pre[n_pre >= min_comp + 1L, station_key]]
}

# ==============================================================================
# mapas (necesitan sf)
# ==============================================================================

CRS_M <- 32719   # WGS84 / UTM 19S, la del shapefile de red vial

# las 32 comunas de la provincia de Santiago mas Puente Alto y San Bernardo
comunas_gran_santiago <- function() {
  comunas <- sf::st_transform(sf::st_read(here("data", "input", "mapas", "comunas", "comunas.shp"),
                                          quiet = TRUE), CRS_M)
  gs <- sf::st_make_valid(comunas[comunas$codregion == 13 &
                                    (comunas$Provincia == "Santiago" |
                                       comunas$Comuna %in% c("Puente Alto", "San Bernardo")), ])
  stopifnot(nrow(gs) == 34)
  gs
}

# red vial del MOP de una clase (1 = autopistas y rutas nacionales), RM o todo el pais
vias_rm <- function(clase, solo_rm = TRUE) {
  sf::st_transform(sf::st_read(here("data", "input", "mapas", "red_vial", "redvial2019.shp"),
                               quiet = TRUE,
                               query = sprintf("SELECT Nom_Ruta FROM redvial2019 WHERE %sClase_Ruta = %d",
                                               if (solo_rm) "Cod_Region = 13 AND " else "",
                                               clase)), CRS_M)
}

# estaciones dentro de `gs` con su precio promedio de `anio` (NA con menos de
# `min_meses` meses con precio), coordenadas UTM (x, y) y comuna. se recorta por
# geometria: el nombre de comuna del panel no calza con el shapefile
estaciones_gran_santiago <- function(gs, anio, min_meses = 9L) {
  panel <- fread(file.path(PANELES[["2012_2026"]]$dir, "panel_mensual.csv"),
                 select = c("station_key", "ym", names(PRECIOS), "region", "lat", "lon"))
  panel <- panel[year(as.IDate(ym)) == anio & region == "metropolitana"]
  est <- panel[, lapply(.SD, \(v) if (sum(!is.na(v)) >= min_meses) mean(v, na.rm = TRUE)
                        else NA_real_),
               by = station_key, .SDcols = names(PRECIOS)]
  est <- merge(est, coords_estacion(panel), by = "station_key")
  pts <- sf::st_transform(sf::st_as_sf(est, coords = c("lon", "lat"), crs = 4326), CRS_M)
  i_com <- as.integer(sf::st_within(pts, gs))
  est <- est[!is.na(i_com)]
  xy <- sf::st_coordinates(pts[!is.na(i_com), ])
  est[, `:=`(x = xy[, 1], y = xy[, 2], comuna = gs$Comuna[i_com[!is.na(i_com)]])]
  est[]
}
