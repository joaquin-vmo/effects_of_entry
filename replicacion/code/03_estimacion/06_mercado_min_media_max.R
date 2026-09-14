# 06_mercado_min_media_max.R
#
# efecto de la entrada sobre el p90, la mediana y el p10 de los precios del mercado
# local (entrante incluida), con Sun y Abraham (2021): ATT en la tabla, estudio de eventos
# en la figura. cohortes mensuales y bins como 03_event_study_sunab.R.
# panel_mensual.csv + entradas.csv -> output/tablas/mercado.tex, output/graficos/mercado.pdf

library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

p <- PANELES[["2012_2026"]]
panel <- leer_panel(p)
entrantes <- unique(fread(file.path(p$dir, "entradas.csv"))$station_key)

d <- cohortes_sunab(muestra_estacion(panel, CTRL_ESTRICTO, p$focal))

res <- lapply(names(PRECIOS), \(fv) {
  z <- mercado_local(panel, d, entrantes, fv, CUANTILES, MIN_COMP)
  ys <- paste0(names(SERIES), "_con")
  att <- lapply(ys, estimar_es, data = z, rhs = "sunab(cohorte, per, att = TRUE)")
  es <- rbindlist(lapply(seq_along(ys), \(j)
    tidy_es(estimar_es(z, ys[j], "sunab(cohorte, per)"), "per")[, serie := SERIES[[j]]]))
  message(sprintf("%s: %d mercados (%d tratados)", fv, uniqueN(z$station_key),
                  uniqueN(z[treated == 1L, station_key])))
  list(att = rbindlist(lapply(seq_along(ys), \(j)
         data.table(serie = SERIES[[j]], t(coeftable(att[[j]])["ATT", c(1, 2, 4)])))),
       es = es[, combustible := PRECIOS[[fv]]],
       n = c(n_estaciones(att[[1]], z), nobs(att[[1]])))
})

# tabla: filas = cuantil, columnas = combustible
cuerpo <- unlist(lapply(unname(SERIES), \(s) {
  r <- rbindlist(lapply(res, \(x) x$att[serie == s]))
  filas_coef(s, r$Estimate, r$`Std. Error`, r$`Pr(>|t|)`)
}))
n <- sapply(res, `[[`, "n")
escribir_tabla("lcccc",
               c(fila("", unname(PRECIOS)), "\\midrule", cuerpo, "\\midrule",
                 fila("Tratadas", n[1, ]), fila("Controles", n[2, ]),
                 fila("Observaciones", format(n[3, ], big.mark = ","))),
               "mercado.tex")

es <- rbindlist(lapply(res, `[[`, "es"))
es[, `:=`(serie = factor(serie, levels = SERIES), combustible = factor(combustible, levels = PRECIOS))]
guardar(grafico_es(es, "serie", dodge = 0.5, y = "Efecto sobre el precio del mercado (%)"),
        "mercado.pdf", alto = 3.2)
