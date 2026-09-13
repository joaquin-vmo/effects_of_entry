# 08_honestdid.R
#
# cuanta violacion de tendencias paralelas resisten la estimacion principal
# (01_event_study.R) y la de cuantiles del mercado (06_mercado_min_media_max.R), con el
# analisis de sensibilidad de Rambachan y Roth (2023). porta 10_tendencias_paralelas.R
# del proyecto anterior.
#
# por que: no rechazar el test de leads no prueba tendencias paralelas; esos tests
# tienen poca potencia contra una deriva suave (Roth 2022). Rambachan y Roth acotan
# cuanto puede diferir la violacion POSTERIOR a la entrada de la PREVIA, que si se
# observa, y entregan un intervalo de confianza robusto.
#
# restriccion de MAGNITUDES RELATIVAS, Delta^RM(Mbar): el salto de la violacion entre
# periodos posteriores no supera Mbar veces el mayor salto observado entre periodos
# previos. Mbar = 1: la deriva despues de la entrada no es mayor que la de antes.
# es adimensional, comparable entre combustibles y series, y no depende del
# espaciamiento de los bins (los extremos -4 y 4 estan agrupados).
#
# se reporta el Mbar DE QUIEBRE: el mayor Mbar de la grilla (0,25 a 3, paso 0,25) con
# el que el intervalo robusto al 95% todavia excluye el cero. "<0,25": ni la relajacion
# minima lo sostiene; ">3": resiste una violacion posterior del triple de la previa.
# como el intervalo se ensancha con Mbar, el quiebre se busca por biseccion.
#
# dos parametros por estudio de eventos:
#   impacto        = bin 0, meses 0 a 5 desde la entrada
#   meses 6 a 11   = bin 1
# en Delta^RM la violacion se acumula bin a bin, de modo que el horizonte mas lejano
# siempre resiste menos.
#
# misma muestra, efectos fijos, cluster y bins que 01 y 06 (control estricto, panel
# completo). verificado contra el proyecto anterior: con control amplio y su grilla,
# los quiebres del bin de impacto son los mismos (1, 2, 0,5 y 2).
#
# toma data/procesado/panel_mensual.csv y entradas.csv, y produce
# output/tablas/honest_principal.tex y output/tablas/honest_mercado.tex

library(data.table)
library(fixest)
library(HonestDiD)
library(here)

source(here("code", "00_utilidades.R"))

MBAR    <- seq(0.25, 3, by = 0.25)
ALPHA   <- 0.05
GRID_SE <- 60   # semiancho de la busqueda del intervalo, en ee del parametro (el
                # default de HonestDiD, 20, truncaba intervalos a Mbar alto)
HORIZONTES <- c(impacto = 1L, meses_6_11 = 2L)   # posicion del bin entre los posteriores
HORIZ_LBL  <- c(impacto = "Impacto (meses 0 a 5)", meses_6_11 = "Meses 6 a 11")

# mismos agregados que 06_mercado_min_media_max.R
MIN_COMP  <- 1L
CUANTILES <- c(p90 = 0.90, p50 = 0.50, p10 = 0.10)
SERIES    <- c(p90 = "Percentil 90", p50 = "Mediana", p10 = "Percentil 10")
VERSIONES <- c(con = "Con entrante", sin = "Sin entrante")

p <- PANELES[["2012_2026"]]
panel <- fread(file.path(p$dir, "panel_mensual.csv"))
panel[, `:=`(ym = as.IDate(ym), g_entry = as.IDate(g_entry), g2_entry = as.IDate(g2_entry))]
panel[, year := year(ym)]
entrantes <- unique(fread(file.path(p$dir, "entradas.csv"))$station_key)
d <- muestra_estacion(panel, CTRL_ESTRICTO, p$focal)

# estudio de eventos principal sobre log(y) x 100 y sensibilidad de sus dos horizontes
sensibilidad <- function(z, y) {
  m <- feols(as.formula(sprintf("log(%s) * 100 ~ i(rel, treated, ref = -1) | %s", y, FE_PRINCIPAL)),
             data = z, cluster = ~comuna)
  nm <- grep("^rel::", names(coef(m)), value = TRUE)   # -4..-2 y 0..4, ya ordenados
  b <- unname(coef(m)[nm])
  V <- matrix(as.numeric(vcov(m)[nm, nm]), length(nm))   # HonestDiD no lee fixest_vcov
  n_pre <- sum(as.integer(sub("^rel::(-?\\d+).*", "\\1", nm)) < 0)
  n_post <- length(nm) - n_pre
  n <- n_estaciones(m, z)

  rbindlist(lapply(names(HORIZONTES), \(h) {
    lv <- replace(numeric(n_post), HORIZONTES[[h]], 1)
    est <- sum(lv * b[-seq_len(n_pre)])
    ee <- sqrt(drop(t(lv) %*% V[-seq_len(n_pre), -seq_len(n_pre)] %*% lv))
    excluye_cero <- \(mb) {
      r <- createSensitivityResults_relativeMagnitudes(
        b, V, n_pre, n_post, l_vec = lv, Mbarvec = mb, alpha = ALPHA,
        grid.lb = est - GRID_SE * ee, grid.ub = est + GRID_SE * ee)
      r$lb > 0 | r$ub < 0
    }
    # biseccion sobre la grilla: lo = ultimo indice que excluye el cero
    quiebre <- if (excluye_cero(MBAR[length(MBAR)])) Inf else if (!excluye_cero(MBAR[1])) 0 else {
      lo <- 1L; hi <- length(MBAR)
      while (hi - lo > 1L) { mid <- (lo + hi) %/% 2L; if (excluye_cero(MBAR[mid])) lo <- mid else hi <- mid }
      MBAR[lo]
    }
    data.table(y = y, horizonte = h, est = est, lb = est - qnorm(1 - ALPHA / 2) * ee,
               ub = est + qnorm(1 - ALPHA / 2) * ee, quiebre = quiebre,
               tratadas = n[1], controles = n[2])
  }))
}

fmt_q <- \(q) fifelse(q == 0, sprintf("$<$%.2f", MBAR[1]),
                      fifelse(is.infinite(q), sprintf("$>$%.0f", max(MBAR)), sprintf("%.2f", q)))
fila <- function(...) paste(paste(c(...), collapse = " & "), "\\\\")

# --- estimacion principal ---
principal <- rbindlist(lapply(names(PRECIOS), \(fv) sensibilidad(d[!is.na(get(fv))], fv)))
print(principal)

cuerpo <- unlist(lapply(names(HORIZONTES), \(h) {
  r <- principal[horizonte == h][match(names(PRECIOS), y)]
  c(sprintf("\\multicolumn{5}{l}{\\emph{%s}} \\\\", HORIZ_LBL[[h]]),
    fila("\\quad Estimador", sprintf("%.3f", r$est)),
    fila("\\quad IC 95\\%", sprintf("[%.3f, %.3f]", r$lb, r$ub)),
    fila("\\quad $\\bar{M}$ de quiebre", fmt_q(r$quiebre)))
}))
r1 <- principal[horizonte == "impacto"][match(names(PRECIOS), y)]
# TODO nota de la tabla: sensibilidad a violaciones de tendencias paralelas de Rambachan
# y Roth (2023), restriccion de magnitudes relativas, sobre el estudio de eventos
# principal (log del precio x 100); estimador e IC 95% convencional del bin indicado;
# Mbar de quiebre = mayor Mbar (grilla 0,25 a 3) con el que el intervalo robusto al 95%
# excluye el cero, es decir, cuantas veces el mayor salto previo puede ser el salto
# posterior de la violacion; control a mas de 5 km; errores agrupados por comuna
writeLines(c("\\begin{tabular}{lcccc}", "\\toprule", fila("", unname(PRECIOS)), "\\midrule",
             cuerpo, "\\midrule",
             fila("Tratadas", r1$tratadas), fila("Controles", r1$controles),
             "\\bottomrule", "\\end{tabular}"),
           here("output", "tablas", "honest_principal.tex"))

# --- cuantiles del mercado ---
cols <- as.vector(outer(names(SERIES), names(VERSIONES), paste, sep = "_"))   # p90_con, ...
mercado <- rbindlist(lapply(names(PRECIOS), \(fv) {
  z <- mercado_local(panel, d, entrantes, fv, CUANTILES, MIN_COMP)
  rbindlist(lapply(cols, \(y) sensibilidad(z, y)))[, outcome := fv]
}))
print(mercado)

cuerpo <- unlist(lapply(names(PRECIOS), \(fv) {
  c(sprintf("\\multicolumn{7}{l}{\\emph{%s}} \\\\", PRECIOS[[fv]]),
    vapply(names(HORIZONTES), \(h) {
      r <- mercado[outcome == fv & horizonte == h][match(cols, y)]
      fila(sprintf("\\quad %s", HORIZ_LBL[[h]]), fmt_q(r$quiebre))
    }, character(1)))
}))
# TODO nota de la tabla: Mbar de quiebre de Rambachan y Roth (2023), restriccion de
# magnitudes relativas, sobre el estudio de eventos del log del percentil 90, la mediana
# y el percentil 10 de los precios del mercado (06_mercado); con entrante incluye a la
# estacion que abre, sin entrante la excluye; mayor Mbar (grilla 0,25 a 3) con el que el
# intervalo robusto al 95% excluye el cero; mercados de 2 km con al menos 1 competidora
# antes de la entrada; control a mas de 5 km; errores agrupados por comuna
writeLines(c("\\begin{tabular}{lcccccc}", "\\toprule",
             " & \\multicolumn{3}{c}{Con entrante} & \\multicolumn{3}{c}{Sin entrante} \\\\",
             "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
             fila("", rep(unname(SERIES), 2)), "\\midrule",
             cuerpo, "\\bottomrule", "\\end{tabular}"),
           here("output", "tablas", "honest_mercado.tex"))
