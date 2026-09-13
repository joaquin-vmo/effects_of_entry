# 02_event_study_controles.R
#
# especificacion principal con control amplio y estricto, panel completo.
# panel_mensual.csv -> output/tablas/es_controles_<combustible>.tex

library(fixest)
library(here)

source(here("code", "00_utilidades.R"))

p <- PANELES[["2012_2026"]]
panel <- leer_panel(p)

GRUPOS <- setNames(list(CTRL_AMPLIO, CTRL_ESTRICTO),
                   sprintf("Control $>$%d km", c(RCTRL_A, RCTRL_B)))
muestras <- lapply(GRUPOS, \(roles) muestra_estacion(panel, roles, p$focal))

for (fv in names(PRECIOS)) {
  datos <- lapply(muestras, \(d) d[!is.na(get(fv))])
  modelos <- lapply(datos, \(d) feols(
    as.formula(sprintf("log(%s) * 100 ~ i(rel, treated, ref = -1) | %s", fv, FE_PRINCIPAL)),
    data = d, cluster = ~comuna))
  n <- mapply(n_estaciones, modelos, datos)
  etable(modelos, tex = TRUE, headers = names(GRUPOS), replace = TRUE,
         extralines = list(Tratadas = n[1, ], Controles = n[2, ]),
         file = here("output", "tablas", sprintf("es_controles_%s.tex", fv)))
}
