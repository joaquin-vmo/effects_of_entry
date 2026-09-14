# 08_ventanas_cortas.R
#
# estudio de eventos principal en las ventanas antes (2014_2019) y despues (2021_2026) de
# la pandemia, TWFE y Sun y Abraham (2021) con la misma muestra, efectos fijos y bins.
# panel_mensual.csv de cada ventana -> output/graficos/ventana_<ventana>.pdf,
# output/tablas/ventana_<ventana>.tex

library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

VENTANAS    <- c("2014_2019", "2021_2026")
ESTIMADORES <- c(twfe = "TWFE", sa = "Sun y Abraham")

for (nombre in VENTANAS) {
  p <- PANELES[[nombre]]
  d <- cohortes_sunab(muestra_estacion(leer_panel(p), CTRL_ESTRICTO, p$focal))

  res <- lapply(names(PRECIOS), \(fv) {
    x <- d[!is.na(get(fv))]
    tw <- estimar_es(x, fv)
    list(es = rbind(tidy_es(tw, "rel")[, estimador := ESTIMADORES[["twfe"]]],
                    tidy_es(estimar_es(x, fv, "sunab(cohorte, per)"), "per")[, estimador := ESTIMADORES[["sa"]]])[
                      , combustible := PRECIOS[[fv]]],
         n = c(n_estaciones(tw, x), nobs(tw)))
  })
  es <- rbindlist(lapply(res, `[[`, "es"))

  # tabla: filas = bin, columnas = combustible x estimador
  cuerpo <- unlist(lapply(setdiff(-NBIN:NBIN, -1L), \(k) {
    r <- es[event_time == k][order(match(combustible, PRECIOS), match(estimador, ESTIMADORES))]
    filas_coef(DICT[[sprintf("rel::%d:treated", k)]], r$estimate, r$se, r$p)
  }))
  n <- sapply(res, `[[`, "n")
  dup <- \(v) rep(v, each = 2)
  escribir_tabla(sprintf("l%s", strrep("c", 2 * length(PRECIOS))),
                 c(fila("", sprintf("\\multicolumn{2}{c}{%s}", PRECIOS)),
                   paste(sprintf("\\cmidrule(lr){%d-%d}", seq(2, 8, 2), seq(3, 9, 2)), collapse = " "),
                   fila("", rep(c("TWFE", "SA"), length(PRECIOS))), "\\midrule",
                   cuerpo, "\\midrule",
                   fila("Tratadas", dup(n[1, ])), fila("Controles", dup(n[2, ])),
                   fila("Observaciones", dup(format(n[3, ], big.mark = ",")))),
                 sprintf("ventana_%s.tex", nombre))

  es[, `:=`(combustible = factor(combustible, levels = PRECIOS), estimador = factor(estimador, levels = ESTIMADORES))]
  guardar(grafico_es(es, "estimador"), sprintf("ventana_%s.pdf", nombre), alto = 3.2)
  message("ventana ", nombre, " lista: tratadas ", paste(n[1, ], collapse = "/"), ", controles ", paste(n[2, ], collapse = "/"))
  print(dcast(es[event_time != -1L, .(combustible, estimador, event_time,
                                      v = sprintf("%.2f%s", estimate, gsub("\\$|\\^|\\{|\\}", "", estrellas(p))))],
              event_time ~ combustible + estimador, value.var = "v"))
}
