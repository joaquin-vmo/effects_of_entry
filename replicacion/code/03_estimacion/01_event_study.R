# 01_event_study.R
#
# estudio de eventos principal, control estricto, escalera de efectos fijos, por ventana.
# panel_mensual.csv -> output/tablas/es_<ventana>_<combustible>.tex (sin la 95), output/graficos/es_<ventana>.pdf

library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

# sin efecto fijo de estacion hay que controlar por el nivel de las tratadas
ESCALERA <- list(
  list(rhs = " + treated", fe = ""),
  list(rhs = " + treated", fe = " | ym"),
  list(rhs = "",           fe = " | ym + station_key"),
  list(rhs = "",           fe = " | ym + station_key + region^year"),
  list(rhs = "",           fe = paste(" |", FE_PRINCIPAL))
)

estimar <- function(d, fv) {
  lapply(ESCALERA, \(e) feols(
    as.formula(sprintf("log(%s) * 100 ~ i(rel, treated, ref = -1)%s%s", fv, e$rhs, e$fe)),
    data = d, cluster = ~comuna))
}

for (nombre in names(PANELES)) {
  p <- PANELES[[nombre]]
  d <- muestra_estacion(leer_panel(p), CTRL_ESTRICTO, p$focal)

  res <- rbindlist(lapply(names(PRECIOS), \(fv) {
    dd <- d[!is.na(get(fv))]
    modelos <- estimar(dd, fv)
    n <- sapply(modelos, n_estaciones, d = dd)
    if (fv != "p95")   # la tesis no usa la tabla de la 95; sus coeficientes si van a la figura
      etable(modelos, tex = TRUE, drop = "Constant", replace = TRUE,
             extralines = list(Tratadas = n[1, ], Controles = n[2, ]),
             file = here("output", "tablas", sprintf("es_%s_%s.tex", nombre, fv)))
    tidy_es(modelos[[length(modelos)]])[, outcome := fv]
  }))

  res[, combustible := factor(PRECIOS[outcome], levels = PRECIOS)]
  guardar(grafico_es(res), sprintf("es_%s.pdf", nombre), alto = 3.2)
  message("panel ", nombre, " listo")
}
