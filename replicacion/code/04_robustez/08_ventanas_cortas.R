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
COH_NUNCA   <- 99999L   # cohorte fuera del rango de meses: sunab() la trata como nunca tratada

# coeficientes k del patron, con p-valor
coefs <- function(m, patron) {
  ct <- coeftable(m)
  ct <- ct[grepl(sprintf("^%s::", patron), rownames(ct)), , drop = FALSE]
  data.table(event_time = as.integer(sub(sprintf("^%s::(-?\\d+).*", patron), "\\1", rownames(ct))),
             estimate = ct[, 1], se = ct[, 2], p = ct[, 4])
}

for (nombre in VENTANAS) {
  p <- PANELES[[nombre]]
  d <- muestra_estacion(leer_panel(p), CTRL_ESTRICTO, p$focal)
  # sunab() no acepta bins: per = cohorte + rel, de modo que per - cohorte = bin
  d[, cohorte := fifelse(treated == 1L, mi(g_entry), COH_NUNCA)]
  d[, per := fifelse(treated == 1L, cohorte + rel, miym)]

  res <- lapply(names(PRECIOS), \(fv) {
    x <- d[!is.na(get(fv))]
    f <- \(rhs) feols(as.formula(sprintf("log(%s) * 100 ~ %s | %s", fv, rhs, FE_PRINCIPAL)),
                      data = x, cluster = ~comuna)
    tw <- f("i(rel, treated, ref = -1)")
    list(es = rbind(coefs(tw, "rel")[, estimador := ESTIMADORES[["twfe"]]],
                    coefs(f("sunab(cohorte, per)"), "per")[, estimador := ESTIMADORES[["sa"]]])[
                      , combustible := PRECIOS[[fv]]],
         n = c(n_estaciones(tw, x), nobs(tw)))
  })
  es <- rbindlist(lapply(res, `[[`, "es"))

  # tabla: filas = bin, columnas = combustible x estimador
  bins <- setdiff(-NBIN:NBIN, -1L)
  etiqueta <- \(k) if (k == -NBIN) sprintf("$\\leq -%d$", NBIN) else if (k == NBIN) sprintf("$\\geq %d$", NBIN) else sprintf("$%d$", k)
  cuerpo <- unlist(lapply(bins, \(k) {
    r <- es[event_time == k][order(match(combustible, PRECIOS), match(estimador, ESTIMADORES))]
    c(fila(etiqueta(k), sprintf("%.3f%s", r$estimate, estrellas(r$p))), fila("", sprintf("(%.3f)", r$se)))
  }))
  n <- sapply(res, `[[`, "n")
  dup <- \(v) rep(v, each = 2)
  writeLines(c(sprintf("\\begin{tabular}{l%s}", strrep("c", 2 * length(PRECIOS))), "\\toprule",
               fila("", sprintf("\\multicolumn{2}{c}{%s}", PRECIOS)),
               paste(sprintf("\\cmidrule(lr){%d-%d}", seq(2, 8, 2), seq(3, 9, 2)), collapse = " "),
               fila("", rep(c("TWFE", "SA"), length(PRECIOS))), "\\midrule",
               cuerpo, "\\midrule",
               fila("Tratadas", dup(n[1, ])), fila("Controles", dup(n[2, ])),
               fila("Observaciones", dup(format(n[3, ], big.mark = ","))),
               "\\bottomrule", "\\end{tabular}"),
             here("output", "tablas", sprintf("ventana_%s.tex", nombre)))

  es <- rbind(es, es[, .(event_time = -1L, estimate = 0, se = 0, p = NA_real_), by = .(estimador, combustible)])
  es[, `:=`(combustible = factor(combustible, levels = PRECIOS), estimador = factor(estimador, levels = ESTIMADORES))]
  fig <- ggplot(es, aes(event_time, estimate, colour = estimador)) +
    geom_hline(yintercept = 0) +
    geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                    position = position_dodge(width = 0.4)) +
    facet_wrap(~combustible, scales = "free_y") +
    scale_x_continuous(breaks = -NBIN:NBIN) +
    labs(x = "Semestres desde la entrada", y = "Efecto sobre el precio (%)", colour = NULL) +
    theme(legend.position = "bottom")
  ggsave(here("output", "graficos", sprintf("ventana_%s.pdf", nombre)), fig, width = 9, height = 6)
  message("ventana ", nombre, " lista: tratadas ", paste(n[1, ], collapse = "/"), ", controles ", paste(n[2, ], collapse = "/"))
  print(dcast(es[event_time != -1L, .(combustible, estimador, event_time,
                                      v = sprintf("%.2f%s", estimate, gsub("\\$|\\^|\\{|\\}", "", estrellas(p))))],
              event_time ~ combustible + estimador, value.var = "v"))
}
