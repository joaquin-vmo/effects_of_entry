# 05_dispersion_precios.R
#
# precios y dispersion en el mercado local: replica la Tabla 2 de Fischer, Martin y
# Schmidt-Dengler (2025), como hacia 16_dispersion.R del proyecto anterior
# (tab_disp_fischer2.tex).
#
# mercado = la estacion focal mas todas las estaciones activas a r km o menos (r = 1
# y 2, los radios de Fischer et al.), en cada mes. solo mercado-mes con al menos dos
# estaciones con precio y en que la focal tiene precio: con una sola no hay
# dispersion. en cada mercado-mes se calcula:
#   precio medio, minimo y maximo
#   desviacion estandar de los precios
#   rango = maximo - minimo: la ganancia maxima posible de buscar
#   ahorro esperado de buscar = medio - minimo: cuanto paga de mas, en promedio,
#     quien elige una estacion al azar en vez de la mas barata
# y la tabla reporta la distribucion de cada medida entre mercado-mes.
#
# un solo anio (ANIO, el ultimo completo): el panel cubre un ciclo completo del
# petroleo y agrupar anios daria percentiles de calendario (la 93 va de ~690 a mas
# de 1.400 $/L). dentro del anio las cifras quedan en $/L sin normalizar.
#
# cambios respecto del proyecto anterior:
#   - no se excluyen los mercados cuya focal es una entrante. Fischer et al. lo
#     hacen porque no los observan antes de la apertura, que importa para el
#     estudio de eventos pero no para describir un anio (con la exclusion: 916 en
#     vez de 1.127 mercados de 1 km en la 93; las medias casi no cambian)
#   - fila con el numero de estaciones del mercado, columna con la mediana y numero
#     de mercados y de mercado-mes por radio
#   - los cuatro combustibles, una tabla cada uno
# se usan los precios del panel tal cual, con arrastre: una estacion que no informa
# no cambio su precio (la ley obliga a informar cada cambio).
#
# toma data/procesado/panel_mensual.csv y produce
# output/tablas/dispersion_<combustible>.tex

library(data.table)
library(here)

source(here("code", "00_utilidades.R"))

ANIO    <- 2025L
RADIOS  <- c(1, 2)   # km
MIN_EDS <- 2L        # estaciones con precio minimas para que haya dispersion

MEDIDAS <- c(n_eds = "Estaciones en el mercado", pmean = "Precio medio",
             pmin = "Precio mínimo", pmax = "Precio máximo", sd = "Desviación estándar",
             rango = "Rango (máximo $-$ mínimo)", ahorro = "Ahorro esperado de buscar")

panel <- fread(file.path(PANELES[["2012_2026"]]$dir, "panel_mensual.csv"),
               select = c("station_key", "ym", "miym", names(PRECIOS), "lat", "lon"))
panel <- panel[year(as.IDate(ym)) == ANIO]
sloc <- coords_estacion(panel)
D <- dist_km(sloc$lat, sloc$lon)   # la diagonal es 0: el mercado incluye a la focal

mercados <- function(fv, r) {
  ix <- which(D <= r, arr.ind = TRUE)
  ed <- data.table(foco = sloc$station_key[ix[, 1]], miembro = sloc$station_key[ix[, 2]])
  px <- panel[!is.na(get(fv)), .(miembro = station_key, miym, p = get(fv))]
  mk <- merge(ed, px, by = "miembro", allow.cartesian = TRUE)
  mk <- mk[, if (any(miembro == foco)) .SD, by = .(foco, miym)]   # la focal con precio
  mk[, .(n_eds = .N, pmean = mean(p), pmin = min(p), pmax = max(p), sd = sd(p),
         rango = max(p) - min(p), ahorro = mean(p) - min(p)),
     by = .(foco, miym)][n_eds >= MIN_EDS]
}

fila <- function(...) paste(paste(c(...), collapse = " & "), "\\\\")
num <- \(x, dig = 1) sprintf("%.*f", dig, x)   # sin modo matematico: babel pondria coma decimal
miles <- \(x) format(x, big.mark = ",")   # misma convencion que las tablas de etable

for (fv in names(PRECIOS)) {
  cuerpo <- unlist(lapply(seq_along(RADIOS), \(i) {
    m <- mercados(fv, RADIOS[i])
    filas <- vapply(names(MEDIDAS), \(v) {
      x <- m[[v]]
      q <- quantile(x, c(0.10, 0.25, 0.50, 0.75, 0.90), names = FALSE)
      # el numero de estaciones es entero: sus percentiles van sin decimales
      pct <- num(q, dig = if (v == "n_eds") 0 else 1)
      fila(MEDIDAS[[v]], c(pct[1:3], num(mean(x)), pct[4:5], num(sd(x))))
    }, character(1))
    c(if (i > 1) "\\midrule",
      sprintf("\\multicolumn{8}{l}{\\emph{Radio de %d km}} \\\\", RADIOS[i]),
      filas,
      sprintf("\\multicolumn{8}{l}{\\footnotesize %s mercados, %s mercado-mes} \\\\",
              miles(uniqueN(m$foco)), miles(nrow(m))))
  }))
  # TODO nota de la tabla: <combustible>, 2025, $/L; mercado = estacion focal y todas
  # las estaciones con precio a 1 (2) km o menos en el mes; solo mercado-mes con al
  # menos dos estaciones; ahorro esperado de buscar = precio medio - precio minimo;
  # percentiles, media y desviacion estandar de cada medida entre mercado-mes; fuente:
  # CNE. replica la Tabla 2 de Fischer, Martin y Schmidt-Dengler (2025)
  writeLines(c("\\begin{tabular}{lccccccc}", "\\toprule",
               fila("", "P10", "P25", "Mediana", "Media", "P75", "P90", "D.E."), "\\midrule",
               cuerpo, "\\bottomrule", "\\end{tabular}"),
             here("output", "tablas", sprintf("dispersion_%s.tex", fv)))
}
