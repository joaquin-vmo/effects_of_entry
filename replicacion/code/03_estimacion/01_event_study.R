# 01_event_study.R
#
# estudio de eventos de la estimacion principal: log del precio (x100 = %) en los
# cuatro combustibles, control estricto (estaciones que nunca tuvieron una entrada
# a menos de RCTRL_B km), una corrida por ventana de PANELES.
#
# la tabla muestra la escalera de efectos fijos: sin efectos fijos y agregando uno
# a la vez hasta la especificacion preferida (FE_PRINCIPAL). misma muestra y mismo
# regresor en todas las columnas. el grafico es la especificacion preferida.
#
# toma data/procesado/.../panel_mensual.csv de cada ventana y produce, por ventana,
# output/tablas/es_<ventana>_<combustible>.tex y output/graficos/es_<ventana>.pdf

library(data.table)
library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

# escalera de efectos fijos. sin efecto fijo de estacion hay que controlar por el
# nivel del grupo tratado; desde que entra la estacion, el indicador queda absorbido
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

# TODO nota de la figura: bins de seis meses desde la entrada, extremos agrupados
# (<= -4 y >= 4); referencia en -1; intervalos al 95% con errores agrupados por comuna
grafico <- function(res, archivo) {
  res[, combustible := factor(PRECIOS[outcome], levels = PRECIOS)]
  fig <- ggplot(res, aes(event_time, estimate)) +
    geom_hline(yintercept = 0) +
    geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se)) +
    facet_wrap(~combustible, scales = "free_y") +
    labs(x = "Tiempo desde la entrada", y = "Efecto sobre el precio (%)")
  ggsave(archivo, fig, width = 9, height = 6)
}

for (nombre in names(PANELES)) {
  p <- PANELES[[nombre]]
  panel <- fread(file.path(p$dir, "panel_mensual.csv"))
  panel[, `:=`(ym = as.IDate(ym), g_entry = as.IDate(g_entry), g2_entry = as.IDate(g2_entry))]
  panel[, year := year(ym)]

  d <- muestra_estacion(panel, CTRL_ESTRICTO, p$focal)

  res <- rbindlist(lapply(names(PRECIOS), \(fv) {
    dd <- d[!is.na(get(fv))]
    modelos <- estimar(dd, fv)
    n <- sapply(modelos, n_estaciones, d = dd)
    # TODO nota de la tabla: errores estandar agrupados por comuna entre parentesis;
    # * p<0,10 ** p<0,05 *** p<0,01; bins de seis meses desde la entrada
    etable(modelos, tex = TRUE, float = FALSE, depvar = FALSE, dict = DICT,
           drop = "Constant", digits = "r3", fitstat = ~n, replace = TRUE,
           extralines = list(Tratadas = n[1, ], Controles = n[2, ]),
           style.tex = style.tex("aer", yesNo = c("Sí", "No"), fixef.suffix = "",
                                 tablefoot = FALSE),
           file = here("output", "tablas", sprintf("es_%s_%s.tex", nombre, fv)))
    tidy_es(modelos[[length(modelos)]])[, outcome := fv]
  }))

  grafico(res, here("output", "graficos", sprintf("es_%s.pdf", nombre)))
  message("panel ", nombre, " listo")
}
