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

d <- muestra_estacion(panel, CTRL_ESTRICTO, p$focal)
COH_NUNCA <- 99999L   # cohorte fuera del rango de meses: sunab() la trata como nunca tratada
d[, cohorte := fifelse(treated == 1L, mi(g_entry), COH_NUNCA)]
d[, per := fifelse(treated == 1L, cohorte + rel, miym)]   # per - cohorte = bin

estimar <- function(z, y, rhs) feols(as.formula(sprintf("log(%s) * 100 ~ %s | %s", y, rhs, FE_PRINCIPAL)),
                                     data = z, cluster = ~comuna)

res <- lapply(names(PRECIOS), \(fv) {
  z <- mercado_local(panel, d, entrantes, fv, CUANTILES, MIN_COMP)
  ys <- paste0(names(SERIES), "_con")
  att <- lapply(ys, estimar, z = z, rhs = "sunab(cohorte, per, att = TRUE)")
  es <- rbindlist(lapply(seq_along(ys), \(j)
    tidy_es(estimar(z, ys[j], "sunab(cohorte, per)"), "per")[, serie := SERIES[[j]]]))
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
  c(fila(s, sprintf("%.3f%s", r$Estimate, estrellas(r$`Pr(>|t|)`))),
    fila("", sprintf("(%.3f)", r$`Std. Error`)))
}))
n <- sapply(res, `[[`, "n")
writeLines(c("\\begin{tabular}{lcccc}", "\\toprule",
             fila("", unname(PRECIOS)), "\\midrule",
             cuerpo, "\\midrule",
             fila("Tratadas", n[1, ]), fila("Controles", n[2, ]),
             fila("Observaciones", format(n[3, ], big.mark = ",")),
             "\\bottomrule", "\\end{tabular}"),
           here("output", "tablas", "mercado.tex"))

es <- rbindlist(lapply(res, `[[`, "es"))
es[, `:=`(serie = factor(serie, levels = SERIES), combustible = factor(combustible, levels = PRECIOS))]
fig <- ggplot(es, aes(event_time, estimate, colour = serie)) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                  position = position_dodge(width = 0.5)) +
  facet_wrap(~combustible, scales = "free_y") +
  scale_x_continuous(breaks = -NBIN:NBIN) +
  labs(x = "Semestres desde la entrada", y = "Efecto sobre el precio del mercado (%)", colour = NULL) +
  theme(legend.position = "bottom")
ggsave(here("output", "graficos", "mercado.pdf"), fig, width = 9, height = 6)
