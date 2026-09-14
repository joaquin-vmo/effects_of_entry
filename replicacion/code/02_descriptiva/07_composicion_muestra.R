# 07_composicion_muestra.R
#
# conteo de estaciones por rol en la ventana larga y muestra de estimacion de la 93.
# panel_mensual.csv + entradas.csv -> output/tablas/composicion_muestra.tex

library(here)

source(here("code", "00_utilidades.R"))

p <- PANELES[["2012_2026"]]
panel <- leer_panel(p)
entradas <- fread(file.path(p$dir, "entradas.csv"))
st <- unique(panel[, .(station_key, base, role_entry)])

# mismos n que las tablas de resultados: estaciones que usa el modelo, sin singletons
n_muestra <- \(roles) {
  d <- muestra_estacion(panel, roles, p$focal)[!is.na(p93)]
  n_estaciones(estimar_es(d, "p93"), d)
}
n_amplio <- n_muestra(CTRL_AMPLIO)
n_estricto <- n_muestra(CTRL_ESTRICTO)
stopifnot(n_amplio[1] == n_estricto[1])

filas <- list(
  c("Estaciones observadas (2012--2026)", nrow(st)),
  c("\\quad presentes en 2012 (incumbentes)", st[base == TRUE, .N]),
  c("\\quad entrantes posteriores a 2012", st[base == FALSE, .N]),
  c("Entradas registradas", nrow(entradas)),
  c(sprintf("\\quad cohortes focales (%d en adelante)", p$focal), entradas[year(as.IDate(g)) >= p$focal, .N]),
  c(sprintf("Tratadas (entrada competidora a %d km o menos)", RTREAT), st[role_entry == "treated", .N]),
  c(sprintf("Excluidas (entrada entre %d y %d km)", RTREAT, RCTRL_A), st[role_entry == "buffer", .N]),
  c(sprintf("Control amplio (sin entradas a %d km o menos)", RCTRL_A), st[role_entry %in% CTRL_AMPLIO, .N]),
  c(sprintf("\\quad control estricto (sin entradas a %d km o menos)", RCTRL_B), st[role_entry %in% CTRL_ESTRICTO, .N]),
  c("Muestra de estimación, gasolina 93", ""),
  c("\\quad tratadas", n_amplio[1]),
  c("\\quad control amplio", n_amplio[2]),
  c("\\quad control estricto", n_estricto[2])
)

escribir_tabla("lr", c(fila("", "N"), "\\midrule", vapply(filas, \(f) fila(f[1], f[2]), character(1))),
               "composicion_muestra.tex")
