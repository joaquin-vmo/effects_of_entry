# 05_dispersion_precios.R
#
# distribucion de precios y dispersion en el mercado local (Tabla 2 de Fischer, Martin
# y Schmidt-Dengler 2025). panel_mensual.csv -> output/tablas/dispersion_<combustible>.tex

library(here)

source(here("code", "00_utilidades.R"))

ANIO    <- 2025L
RADIOS  <- c(1, 2)   # km
MIN_EDS <- 2L        # estaciones con precio minimas en el mercado-mes

MEDIDAS <- c(n_eds = "Estaciones en el mercado", pmean = "Precio medio",
             pmin = "Precio mínimo", pmax = "Precio máximo", sd = "Desviación estándar",
             rango = "Rango (máximo $-$ mínimo)", ahorro = "Ahorro esperado de buscar")

panel <- fread(file.path(PANELES[["2012_2026"]]$dir, "panel_mensual.csv"),
               select = c("station_key", "ym", "miym", names(PRECIOS), "lat", "lon"))
panel <- panel[year(as.IDate(ym)) == ANIO]
sloc <- coords_estacion(panel)
D <- dist_km(sloc$lat, sloc$lon)   # diagonal 0: el mercado incluye a la focal

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

num <- \(x, dig = 1) sprintf("%.*f", dig, x)   # sin modo matematico: babel pondria coma decimal
miles <- \(x) format(x, big.mark = ",")

for (fv in names(PRECIOS)) {
  cuerpo <- unlist(lapply(seq_along(RADIOS), \(i) {
    m <- mercados(fv, RADIOS[i])
    filas <- vapply(names(MEDIDAS), \(v) {
      x <- m[[v]]
      q <- quantile(x, c(0.10, 0.25, 0.50, 0.75, 0.90), names = FALSE)
      pct <- num(q, dig = if (v == "n_eds") 0 else 1)
      fila(MEDIDAS[[v]], c(pct[1:3], num(mean(x)), pct[4:5], num(sd(x))))
    }, character(1))
    c(if (i > 1) "\\midrule",
      sprintf("\\multicolumn{8}{l}{\\emph{Radio de %d km}} \\\\", RADIOS[i]),
      filas,
      sprintf("\\multicolumn{8}{l}{\\footnotesize %s mercados, %s mercado-mes} \\\\",
              miles(uniqueN(m$foco)), miles(nrow(m))))
  }))
  writeLines(c("\\begin{tabular}{lccccccc}", "\\toprule",
               fila("", "P10", "P25", "Mediana", "Media", "P75", "P90", "D.E."), "\\midrule",
               cuerpo, "\\bottomrule", "\\end{tabular}"),
             here("output", "tablas", sprintf("dispersion_%s.tex", fv)))
}
