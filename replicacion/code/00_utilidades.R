# 00_utilidades.R
#
# parametros y funciones comunes. un solo lugar para todo numero que define la
# muestra o la especificacion: los scripts los leen de aqui y no los redeclaran,
# de modo que cambiar el radio de tratamiento no exige tocar nueve archivos.
#
# las rutas se resuelven con here(), con raiz en replicacion/ (abrir code.Rproj).
#
# 1. librerias
# 2. parametros del build (01_buid/)
# 3. parametros del analisis
# 4. funciones: calendario, distancia, varias y estimacion
# 5. dependencias

# ==============================================================================
# 1. librerias
# ==============================================================================

library(data.table)
library(here)

# ==============================================================================
# 2. parametros del build
# ==============================================================================

R_EARTH  <- 6371    # km, radio terrestre para la distancia de gran circulo

# --- construccion del panel ---
WEEK0    <- as.Date("2012-01-05")  # ancla de jueves. el mepco arranca el
                                   # 2014-08-07, exactamente 135 semanas
                                   # despues, de modo que cae sobre la grilla
MAXGAP_W <- 13L     # semanas maximas de arrastre (~3 meses). un vacio mayor
                    # saca a la estacion del panel en vez de propagar su precio
CORTE_NUEVO <- "2023-01-01"  # desde aca rige el esquema de reporte actual de la
                             # CNE, el que consolida las modalidades de atencion
LINK_M   <- 50      # metros: un id nuevo que sucede a otro a esta distancia es
                    # el mismo local recodificado, no una entrada

# --- mercados locales ---
RTREAT   <- 2       # km: la incumbente esta tratada si hay una entrada a <= esto
# los dos grupos de control admisibles se definen por la distancia MINIMA a la que
# la estacion estuvo alguna vez de una entrada. Un control debe no haber recibido
# nunca una entrada dentro de ese radio:
#   RCTRL_A: control amplio, nunca una entrada a menos de 3 km
#   RCTRL_B: control estricto, nunca una entrada a menos de 5 km, subconjunto del
#            anterior
# las estaciones cuya entrada mas cercana quedo entre RTREAT y RCTRL_A no son ni
# tratadas ni control: la figura de atenuacion muestra efecto significativo en los
# cuatro combustibles en la banda de 2 a 3 km, de modo que usarlas como comparacion
# meteria estaciones parcialmente tratadas en el contrafactual
RCTRL_A  <- 3
RCTRL_B  <- 5
RCTRL    <- RCTRL_B # km: radio exterior, usado ademas por los ejercicios que
                    # describen el entorno de cada estacion

# --- definicion de eventos ---
# SOLO LAS COMPETIDORAS TRATAN. Una entrada que lleva la misma marca que la
# incumbente no es la llegada de un competidor sino la expansion de la red de la
# propia cadena: bajo consignacion el precio de ambas lo fija la misma mayorista,
# de modo que no hay contra quien competir. El marco teorico lo dice tambien, la n
# de la Proposicion 1 cuenta competidoras y no estaciones.
#
# Con este criterio la marca de la entrante se compara con la de CADA incumbente,
# de manera que un mismo evento trata a unas y no a otras. La mascara se aplica
# sobre la matriz estacion-evento en 05_armado_panel.R, antes de derivar g_entry,
# g2_entry, role_entry y mindist, de modo que la definicion es unica y arrastra a
# todo el analisis.
#
# El costo es de comparabilidad: Fischer, Martin y Schmidt-Dengler definen el
# tratamiento como cualquier entrada dentro del radio. Ponerlo en FALSE recupera su
# criterio exacto. Con TRUE los efectos crecen entre un 10 y un 42 % segun el
# resultado, porque el brazo tratado deja de incluir eventos cuyo efecto es cero.
SOLO_COMPETIDORAS <- TRUE

FOCAL_FROM <- 2014L # las entradas desde este ano son tratamiento focal. las de
                    # 2013 (79 contra 20-50 en un ano normal, 43 de bandera
                    # blanca) son incorporacion tardia al reporte: contaminan y
                    # cortan ventanas, pero nunca definen cohorte

# --- paneles ---
# cada ventana que construye 05_armado_panel.R. Las estaciones presentes en el
# anio `desde` son la base; las que aparecen despues son entradas, y definen
# cohorte desde `focal`. En las ventanas cortas focal es el anio siguiente a la
# base: la incorporacion tardia al reporte es un fenomeno de 2013.
# `hasta` es el ultimo dia de la ventana. La ventana reciente termina en febrero
# de 2026: el 26 de marzo el MEPCO salta de 1.083 a 1.455 $/L en la 93 y desde
# ahi el modo de competir es otro.
# El panel completo se escribe en data/procesado, que es de donde lee el analisis.
PANELES <- list(
  "2012_2026" = list(desde = 2012L, hasta = "2026-12-31", focal = FOCAL_FROM,
                     dir = here("data", "procesado")),
  "2014_2019" = list(desde = 2014L, hasta = "2019-12-31", focal = 2015L,
                     dir = here("data", "procesado", "panel_2014_2019")),
  "2021_2026" = list(desde = 2021L, hasta = "2026-02-28", focal = 2022L,
                     dir = here("data", "procesado", "panel_2021_2026"))
)

# ==============================================================================
# 3. parametros del analisis
# ==============================================================================

# --- grupos de comparacion ---
# en el vocabulario de role_entry. CTRL_AMPLIO es el de la especificacion principal
CTRL_AMPLIO  <- c("ctrl_a", "ctrl_b")
CTRL_ESTRICTO <- "ctrl_b"
RADIOS_F <- c(1, 2) # km: los dos radios de fischer et al., para sus figuras

# --- ventanas de estudio de eventos ---
PRE      <- 12L     # meses previos en el diseno apilado
POST     <- 12L     # meses posteriores en el diseno apilado
BIN_M    <- 6L      # meses por bin en el formato de reporte de fischer et al.
NBIN     <- 4L      # bins a cada lado; los extremos se agrupan
NSEM     <- 4L      # semestres a cada lado en sun-abraham y persistencia
VENT_W   <- 26L     # semanas a cada lado en el test de anticipacion

COHORTE_NUNCA <- 1000000L  # codigo de cohorte para las nunca tratadas

# --- efectos fijos ---
# la especificacion principal. el efecto fijo de marca x ano no esta en fischer
# et al. y responde a un rasgo del caso chileno: bajo consignacion el precio lo
# fija la mayorista, y con tres cadenas que concentran ~80% de las EDS un
# movimiento nacional de cualquiera de ellas es un confusor que ni el efecto
# fijo de estacion ni el de region x ano absorben. distribuidor es constante a
# nivel de estacion por construccion del panel, de modo que no es un mal control
FE_PRINCIPAL <- "station_key + ym + region^year + distribuidor^year"

# --- margen previo (variable de particion de la heterogeneidad) ---
MIN_N    <- 4L      # estaciones minimas en una comuna-mes para que el percentil
                    # signifique algo
CORTE    <- 2 / 3   # tercil superior de la distribucion de las tratadas

# participacion de cada combustible en el volumen nacional de ventas de 2017
# (FNE, Rol N 2538-19 par. 8), renormalizada sobre los cuatro analizados.
# pondera la medida compuesta de margen previo: el poder de mercado de una
# estacion no se expresa por igual en la 97, que es el 3,3% del volumen, y en el
# diesel, que es mas de la mitad
W_COMB   <- c(p93 = 16.0, p95 = 9.7, p97 = 3.3, pdi = 52.5)
W_COMB   <- W_COMB / sum(W_COMB)

# el combustible cuyo rank debe excluirse del margen previo al analizar cada
# resultado. el margen de la 93 se parte con las otras tres gasolinas mas el
# diesel, igual que su precio
EXCLUIR <- c(p93 = "p93", p95 = "p95", p97 = "p97", pdi = "pdi",
             m93 = "p93", m97 = "p97", mdi = "pdi")

# --- mercado como unidad ---
MIN_FIRMS <- 2L     # un mercado necesita dos firmas para que max y min difieran

# --- descriptivas de salidas ---
REEMP_M     <- 500  # metros: reemplazo del mercado local tras un cierre
REEMP_MESES <- 24L  # meses posteriores al cierre en que se busca el reemplazo
CENSURA     <- 3L   # meses antes del fin del panel en que un spell que termina
                    # esta censurado por la derecha y no es una salida

# --- etiquetas ---
PRECIOS  <- c(p93 = "Gasolina 93", p95 = "Gasolina 95",
              p97 = "Gasolina 97", pdi = "Diésel")
MARGENES <- c(m93 = "Gasolina 93", m97 = "Gasolina 97", mdi = "Diésel")

FUENTE <- "Fuente: elaboración propia a partir de la base de precios de la CNE."

# nombres de coeficientes y efectos fijos para etable()
DICT <- c(setNames(c(sprintf("$\\leq -%d$", NBIN), sprintf("$%d$", -(NBIN - 1):(NBIN - 1)),
                     sprintf("$\\geq %d$", NBIN)),
                   sprintf("rel::%d:treated", -NBIN:NBIN)),
          treated = "Tratada", n = "Observaciones", ym = "Mes", station_key = "Estación",
          `region^year` = "Región $\\times$ año", `distribuidor^year` = "Marca $\\times$ año")

# ==============================================================================
# 4. funciones
# ==============================================================================

# --- calendario ---
# los indices son enteros continuos y no fechas, porque el diseno necesita
# aritmetica de tiempo-evento (t - g) y las clases de fecha no la dan
# directamente. mi() es el mes, contado desde el ano 0

mi <- function(d) {
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

trim_i <- function(d) {                     # trimestre continuo
  d <- as.IDate(d)
  year(d) * 4L + (month(d) - 1L) %/% 3L
}

sem_i <- function(m) m %/% 6L               # semestre continuo, desde un mes mi()

# --- distancia ---
# gran circulo (haversine) en km. acepta uno o dos conjuntos de coordenadas y
# devuelve la matriz completa. h se topa en 1 porque el error de punto flotante
# puede empujarlo apenas por encima y sacar a asin de su dominio, lo que
# produciria NaN en pares practicamente coincidentes -- que es justo el caso que
# importa al detectar locales duplicados

dist_km <- function(lat1, lon1, lat2 = lat1, lon2 = lon1) {
  a1 <- lat1 * pi / 180; o1 <- lon1 * pi / 180
  a2 <- lat2 * pi / 180; o2 <- lon2 * pi / 180
  h <- sin(outer(a1, a2, "-") / 2)^2 +
    outer(cos(a1), cos(a2)) * sin(outer(o1, o2, "-") / 2)^2
  h[h > 1] <- 1
  2 * R_EARTH * asin(sqrt(h))
}

# matriz de distancias de un conjunto contra si mismo, con la diagonal en Inf:
# una estacion no es competidora de si misma
dist_propia <- function(lat, lon) {
  D <- dist_km(lat, lon)
  diag(D) <- Inf
  D
}

# una fila por estacion con su coordenada, a partir del panel
coords_estacion <- function(panel) {
  s <- unique(panel[!is.na(lat), .(station_key, lat, lon)])
  s[, .SD[1], by = station_key]
}

# --- varias ---
# valor modal de un vector de caracteres, ignorando NA. se usa para colapsar
# atributos de estacion que deberian ser constantes pero traen alguna
# inconsistencia de reporte
modal <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

redondear <- function(d, dig = 3) {
  d[, lapply(.SD, function(x) if (is.numeric(x)) round(x, dig) else x)]
}

# --- estimacion ---
# muestra de estimacion. las cuatro restricciones:
#   1. tratadas + grupo de control `roles` (CTRL_AMPLIO o CTRL_ESTRICTO); ninguna
#      estacion entre RTREAT y RCTRL_A km entra en ninguno de los dos brazos
#   2. se corta en la segunda entrada: el estimando es el de una PRIMERA entrada
#   3. fuera las tratadas de cohortes anteriores a `focal` (en el panel completo,
#      2013 es incorporacion tardia al reporte, no aperturas)
#   4. solo estaciones de base en ambos brazos (regla de Fischer et al.): una
#      entrante reciente esta en su propio transitorio de apertura
# rel: bins de BIN_M meses desde la entrada, extremos agrupados en +-NBIN. las no
# tratadas van al bin de referencia (-1)
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

# coeficientes de tiempo-evento de un modelo fixest (`patron`::k), con la
# referencia -1 en cero
tidy_es <- function(m, patron = "rel") {
  ct <- fixest::coeftable(m)
  ct <- ct[grepl(sprintf("^%s::", patron), rownames(ct)), , drop = FALSE]
  d <- data.table(event_time = as.integer(sub(sprintf("^%s::(-?\\d+).*", patron), "\\1",
                                              rownames(ct))),
                  estimate = ct[, 1], se = ct[, 2])
  setorder(rbind(d, data.table(event_time = -1L, estimate = 0, se = 0)), event_time)[]
}

# --- mapas (necesitan sf) ---
CRS_M <- 32719   # WGS84 / UTM 19S: metrica, y la del shapefile de red vial

# comunas del Gran Santiago: las 32 de la provincia de Santiago mas Puente Alto y
# San Bernardo
comunas_gran_santiago <- function() {
  comunas <- sf::st_transform(sf::st_read(here("data", "input", "mapas", "comunas", "comunas.shp"),
                                          quiet = TRUE), CRS_M)
  gs <- sf::st_make_valid(comunas[comunas$codregion == 13 &
                                    (comunas$Provincia == "Santiago" |
                                       comunas$Comuna %in% c("Puente Alto", "San Bernardo")), ])
  stopifnot(nrow(gs) == 34)
  gs
}

# red vial de una clase del inventario del MOP (1 = autopistas concesionadas y rutas
# nacionales, 2 = avenidas estructurantes), de la region metropolitana o, con
# solo_rm = FALSE, de todo el pais
vias_rm <- function(clase, solo_rm = TRUE) {
  sf::st_transform(sf::st_read(here("data", "input", "mapas", "red_vial", "redvial2019.shp"),
                               quiet = TRUE,
                               query = sprintf("SELECT Nom_Ruta FROM redvial2019 WHERE %sClase_Ruta = %d",
                                               if (solo_rm) "Cod_Region = 13 AND " else "",
                                               clase)), CRS_M)
}

# estaciones del Gran Santiago con su precio promedio de `anio` por combustible (NA
# si tiene menos de `min_meses` meses con precio: descarta las que abrieron o
# cerraron a mitad de anio), coordenadas UTM (x, y) y comuna del poligono que las
# contiene. el recorte es por geometria y no por el nombre de comuna del panel, que
# viene en minusculas y sin tildes y no calza con el shapefile
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

# tratadas y controles que efectivamente usa un modelo estimado sobre `d` (sin las
# filas que fixest descarta, como los singletons)
n_estaciones <- function(m, d) {
  u <- d[fixest::obs(m)]
  c(uniqueN(u[treated == 1L, station_key]), uniqueN(u[treated == 0L, station_key]))
}

# mercado local de cada estacion focal de la muestra `d` en el combustible fv: la
# focal mas todas las estaciones con precio a RTREAT km o menos en el mes (la focal
# con precio ese mes). devuelve `d` con n (estaciones) y, para cada cuantil de
# `cuantiles` (vector con nombre), <nombre>_con (toda estacion, la entrante incluida
# desde que abre) y <nombre>_sin (sin las estaciones de `entrantes`). solo mercados con
# al menos min_comp competidoras en promedio ANTES de la entrada (todos sus meses si
# es control): medirlo con el conteo contemporaneo seleccionaria sobre la entrada.
# ver 06_mercado_min_media_max.R
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
# 5. dependencias
#
# .chequear_dependencias() falla con un diagnostico explicito si falta un paquete
# o su version es insuficiente. replace_values() es de dplyr >= 1.2.0: con una
# version anterior 01_limpiar_precios.R falla con "could not find function", que
# es un error opaco. todavia no la llama nadie: es para el master.R.
# ==============================================================================

.dependencias <- c(data.table = "1.15.0", fixest = "0.11.0", ggplot2 = "3.4.0",
                   dplyr = "1.2.0", stringr = "1.5.0", readxl = "1.4.0",
                   HonestDiD = "0.2.8", readr = "2.1.0", purrr = "1.0.0",
                   tidyr = "1.3.0", here = "1.0.0")

.chequear_dependencias <- function() {
  falta <- character(0)
  viejo <- character(0)
  for (p in names(.dependencias)) {
    if (!requireNamespace(p, quietly = TRUE)) {
      falta <- c(falta, p)
    } else if (utils::packageVersion(p) < .dependencias[[p]]) {
      viejo <- c(viejo, sprintf("%s (%s instalada, se requiere >= %s)",
                                p, utils::packageVersion(p), .dependencias[[p]]))
    }
  }
  if (length(falta))
    stop("faltan paquetes: ", paste(falta, collapse = ", "),
         "\ninstala con: install.packages(c(",
         paste(sprintf('"%s"', falta), collapse = ", "), "))")
  if (length(viejo))
    stop("versiones insuficientes:\n  ", paste(viejo, collapse = "\n  "))
  invisible(TRUE)
}
