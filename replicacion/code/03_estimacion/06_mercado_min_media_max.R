# 06_mercado_min_media_max.R
#
# efecto de la entrada sobre el p90, la mediana y el p10 de los precios del mercado
# local, con y sin entrante. panel_mensual.csv + entradas.csv ->
# output/tablas/mercado_<combustible>.tex, output/graficos/mercado_<combustible>.pdf

library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

p <- PANELES[["2012_2026"]]
panel <- leer_panel(p)
entrantes <- unique(fread(file.path(p$dir, "entradas.csv"))$station_key)

d <- muestra_estacion(panel, CTRL_ESTRICTO, p$focal)
d[, post := as.integer(treated == 1L & ym >= g_entry)]

estimar <- function(z, y, rhs) feols(as.formula(sprintf("log(%s) * 100 ~ %s | %s", y, rhs, FE_PRINCIPAL)),
                                     data = z, cluster = ~comuna)

for (fv in names(PRECIOS)) {
  z <- mercado_local(panel, d, entrantes, fv, CUANTILES, MIN_COMP)
  cols <- as.vector(outer(names(SERIES), names(VERSIONES), paste, sep = "_"))   # p90_con, ...

  modelos <- lapply(cols, estimar, z = z, rhs = "post")
  n <- sapply(modelos, n_estaciones, d = z)
  etable(modelos, tex = TRUE, replace = TRUE,
         headers = list("^:_:" = list(`Con entrante` = 3, `Sin entrante` = 3),
                        rep(unname(SERIES), 2)),
         dict = c(post = "Entrada"),
         extralines = list(Tratadas = n[1, ], Controles = n[2, ]),
         file = here("output", "tablas", sprintf("mercado_%s.tex", fv)))

  es <- rbindlist(lapply(cols, \(y) {
    tidy_es(estimar(z, y, "i(rel, treated, ref = -1)"))[
      , `:=`(serie = SERIES[[sub("_.*", "", y)]], version = VERSIONES[[sub(".*_", "", y)]])]
  }))
  es[, `:=`(serie = factor(serie, levels = SERIES), version = factor(version, levels = VERSIONES))]
  fig <- ggplot(es, aes(event_time, estimate, colour = serie)) +
    geom_hline(yintercept = 0) +
    geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                    position = position_dodge(width = 0.5)) +
    facet_wrap(~version) +
    scale_x_continuous(breaks = -NBIN:NBIN) +
    labs(x = "Tiempo desde la entrada", y = "Efecto sobre el precio del mercado (%)",
         colour = "Precio del mercado")
  ggsave(here("output", "graficos", sprintf("mercado_%s.pdf", fv)), fig, width = 10, height = 4.5)
  message(sprintf("%s: %d mercados (%d tratados)", fv, uniqueN(z$station_key),
                  uniqueN(z[treated == 1L, station_key])))
}
