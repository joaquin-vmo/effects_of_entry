# 07_placebo_controles.R
#
# placebo con fechas de entrada falsas en el grupo de control. cada replica sortea la
# proporcion real de tratadas dentro del grupo, les asigna cohortes y desfases a la segunda
# entrada sacados de sus distribuciones reales, corta en la segunda entrada falsa y estima
# el ATT estatico de la especificacion principal. sobre todo el grupo y sobre su mitad mas
# densa (competidoras a <= RTREAT km).
# panel_mensual.csv -> output/tablas/placebo_controles.tex

library(fixest)
library(here)

source(here("code", "00_utilidades.R"))

B       <- 500L
SEMILLA <- 20260902L
POOLS   <- c(completo = "Todo el grupo de control", denso = "Mitad más densa del grupo de control")

set.seed(SEMILLA)
p <- PANELES[["2012_2026"]]
panel <- leer_panel(p)
real <- muestra_estacion(panel, CTRL_ESTRICTO, p$focal)
real[, post := as.integer(treated == 1L & ym >= g_entry)]

st <- unique(real[, .(station_key, treated, g_entry, g2_entry)])
prop_trat <- mean(st$treated)
coh_real <- mi(st[treated == 1L, g_entry])
d2_real  <- st[treated == 1L & !is.na(g2_entry), mi(g2_entry) - mi(g_entry)]

# densidad: competidoras con reporte a <= RTREAT km, promedio sobre los meses de la estacion
ncomp <- merge(vecinas(coords_estacion(panel), RTREAT), unique(panel[, .(vecina = station_key, miym)]), by = "vecina",
               allow.cartesian = TRUE)[, .(n = .N), by = .(station_key, miym)]
dens <- merge(unique(real[treated == 0L, .(station_key, miym)]), ncomp, all.x = TRUE)[
  , .(n = mean(fcoalesce(n, 0L))), by = station_key]
pools <- list(completo = dens$station_key, denso = dens[n >= median(n), station_key])

FML <- as.formula(sprintf("c(%s) ~ post | %s",
                          paste(sprintf("log(%s) * 100", names(PRECIOS)), collapse = ", "), FE_PRINCIPAL))
att <- \(m) rbindlist(lapply(seq_along(PRECIOS), \(j)
  data.table(outcome = names(PRECIOS)[j], t(coeftable(m[[j]])["post", c(1, 2, 4)]))))

reales <- att(feols(FML, data = real, cluster = ~comuna, notes = FALSE))
setnames(reales, c("outcome", "att", "se", "p"))

reps <- rbindlist(lapply(names(POOLS), \(pool) {
  sk <- pools[[pool]]
  base <- real[station_key %in% sk]
  rbindlist(lapply(seq_len(B), \(b) {
    k <- round(prop_trat * length(sk))
    g <- sample(coh_real, k, replace = TRUE)
    asg <- data.table(station_key = sample(sk, k), g = g, g2 = g + sample(d2_real, k, replace = TRUE))
    x <- merge(base, asg, by = "station_key", all.x = TRUE)[is.na(g2) | miym < g2]
    x[, post := as.integer(!is.na(g) & miym >= g)]
    r <- att(feols(FML, data = x, cluster = ~comuna, notes = FALSE))
    setnames(r, c("outcome", "att", "se", "p"))[, `:=`(pool = pool, rep = b)]
  }))
}))

res <- reps[, .(media = mean(att), de = sd(att), se_medio = mean(se),
                q025 = quantile(att, 0.025), q975 = quantile(att, 0.975),
                rechazo = mean(p < 0.05),
                p_ri = mean(abs(att) >= abs(reales[outcome == .BY$outcome, att]))),
            by = .(pool, outcome)]
print(res, digits = 3)

orden <- \(r) r[match(names(PRECIOS), outcome)]
num <- \(x, k = 3) sprintf("%.*f", k, x)
cuerpo <- unlist(lapply(names(POOLS), \(pl) {
  r <- orden(res[pool == pl])
  c(sprintf("\\multicolumn{5}{l}{\\emph{%s (%d estaciones)}} \\\\", POOLS[[pl]], length(pools[[pl]])),
    fila("\\quad ATT placebo medio", num(r$media)),
    fila("\\quad Desviación estándar de la distribución", num(r$de)),
    fila("\\quad Error estándar agrupado medio", num(r$se_medio)),
    fila("\\quad Cociente error estándar / desviación", num(r$se_medio / r$de, 2)),
    fila("\\quad Intervalo del 95\\% de la distribución", sprintf("[%.3f; %.3f]", r$q025, r$q975)),
    fila("\\quad Tasa de rechazo al 5\\%", num(r$rechazo)),
    fila("\\quad Valor $p$ por aleatorización del ATT real", num(r$p_ri)),
    "\\addlinespace")
}))
r <- orden(reales)
escribir_tabla("lcccc",
               c(fila("", unname(PRECIOS)), "\\midrule", cuerpo, "\\midrule",
                 filas_coef("ATT real", r$att, r$se, r$p)),
               "placebo_controles.tex")
