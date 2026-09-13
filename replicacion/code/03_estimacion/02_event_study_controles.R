# 02_event_study_controles.R
#
# estudio de eventos de la especificacion preferida (FE_PRINCIPAL) con los dos
# grupos de control: amplio (nunca una entrada a menos de RCTRL_A km) y estricto
# (nunca a menos de RCTRL_B km, contenido en el amplio). solo panel completo.
#
# toma data/procesado/panel_mensual.csv y produce una tabla por combustible,
# output/tablas/es_controles_<combustible>.tex

library(data.table)
library(fixest)
library(here)

source(here("code", "00_utilidades.R"))

p <- PANELES[["2012_2026"]]
panel <- fread(file.path(p$dir, "panel_mensual.csv"))
panel[, `:=`(ym = as.IDate(ym), g_entry = as.IDate(g_entry), g2_entry = as.IDate(g2_entry))]
panel[, year := year(ym)]

GRUPOS <- setNames(list(CTRL_AMPLIO, CTRL_ESTRICTO),
                   sprintf("Control $>$%d km", c(RCTRL_A, RCTRL_B)))
muestras <- lapply(GRUPOS, \(roles) muestra_estacion(panel, roles, p$focal))

for (fv in names(PRECIOS)) {
  datos <- lapply(muestras, \(d) d[!is.na(get(fv))])
  modelos <- lapply(datos, \(d) feols(
    as.formula(sprintf("log(%s) * 100 ~ i(rel, treated, ref = -1) | %s", fv, FE_PRINCIPAL)),
    data = d, cluster = ~comuna))
  n <- mapply(n_estaciones, modelos, datos)
  # TODO nota de la tabla: errores estandar agrupados por comuna entre parentesis;
  # * p<0,10 ** p<0,05 *** p<0,01; bins de seis meses desde la entrada; efectos
  # fijos de estacion, mes, region x ano y marca x ano
  etable(modelos, tex = TRUE, float = FALSE, depvar = FALSE, dict = DICT, headers = names(GRUPOS),
         digits = "r3", fitstat = ~n, replace = TRUE,
         extralines = list(Tratadas = n[1, ], Controles = n[2, ]),
         style.tex = style.tex("aer", yesNo = c("Sí", "No"), fixef.suffix = "",
                               tablefoot = FALSE),
         file = here("output", "tablas", sprintf("es_controles_%s.tex", fv)))
}
