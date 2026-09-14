# 02_honest_did.R
#
# sensibilidad a tendencias paralelas (Rambachan y Roth 2023, magnitudes relativas) de
# los estudios de eventos de 03_estimacion/01 y 06. panel_mensual.csv + entradas.csv ->
# output/tablas/honest_{principal,mercado}.tex

library(fixest)
library(HonestDiD)
library(here)

source(here("code", "00_utilidades.R"))

MBAR    <- seq(0.25, 3, by = 0.25)
ALPHA   <- 0.05
GRID_SE <- 60   # semiancho de la busqueda del intervalo, en ee (con 20 se truncaba)
HORIZONTES <- c(impacto = 1L, meses_6_11 = 2L)   # posicion del bin entre los posteriores
HORIZ_LBL  <- c(impacto = "Impacto (meses 0 a 5)", meses_6_11 = "Meses 6 a 11")

p <- PANELES[["2012_2026"]]
panel <- leer_panel(p)
entrantes <- unique(fread(file.path(p$dir, "entradas.csv"))$station_key)
d <- muestra_estacion(panel, CTRL_ESTRICTO, p$focal)

# estudio de eventos sobre log(y) x 100 y Mbar de quiebre de cada horizonte
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
    # biseccion: el intervalo se ensancha con Mbar. 0 = "<0,25", Inf = ">3"
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

# ---- estimacion principal ----
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
writeLines(c("\\begin{tabular}{lcccc}", "\\toprule", fila("", unname(PRECIOS)), "\\midrule",
             cuerpo, "\\midrule",
             fila("Tratadas", r1$tratadas), fila("Controles", r1$controles),
             "\\bottomrule", "\\end{tabular}"),
           here("output", "tablas", "honest_principal.tex"))

# ---- cuantiles del mercado ----
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
writeLines(c("\\begin{tabular}{lcccccc}", "\\toprule",
             " & \\multicolumn{3}{c}{Con entrante} & \\multicolumn{3}{c}{Sin entrante} \\\\",
             "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
             fila("", rep(unname(SERIES), 2)), "\\midrule",
             cuerpo, "\\bottomrule", "\\end{tabular}"),
           here("output", "tablas", "honest_mercado.tex"))
