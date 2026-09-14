# 05_persistencia.R
#
# persistencia del tratamiento: estudio de eventos sobre el numero de estaciones a
# <= RTREAT km, con y sin la entrante, como la figura 2 de Fischer et al. (2025).
# con y sin el corte en la segunda entrada, que censura el conteo a horizontes largos.
# panel_mensual.csv -> output/graficos/persistencia.pdf

library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

p <- PANELES[["2012_2026"]]
panel <- leer_panel(p)

# vecinas a <= RTREAT km presentes en el panel cada mes
edges <- merge(vecinas(coords_estacion(panel), RTREAT), panel[, .(ini = min(miym)), by = .(vecina = station_key)], by = "vecina")
ev <- merge(edges, unique(panel[, .(vecina = station_key, miym)]), by = "vecina", allow.cartesian = TRUE)
ev <- merge(ev, unique(panel[role_entry == "treated", .(station_key, g = mi(g_entry))]),
            by = "station_key", all.x = TRUE)
conteo <- ev[, .(ncomp = .N, ninc = sum(is.na(g) | ini < g)), by = .(station_key, miym)]   # ninc: sin la entrante

CONTEOS  <- c(ncomp = "Incluye a la entrante", ninc = "Excluye a la entrante")
MUESTRAS <- c("Con corte en la segunda entrada", "Sin corte")

res <- rbindlist(lapply(MUESTRAS, \(mu) {
  x <- copy(panel)
  if (mu == MUESTRAS[2]) x[, g2_entry := NA]   # muestra_estacion() corta en g2_entry
  d <- muestra_estacion(x, CTRL_ESTRICTO, p$focal)
  d <- merge(d, conteo, by = c("station_key", "miym"), all.x = TRUE)
  d[is.na(ncomp), `:=`(ncomp = 0L, ninc = 0L)]
  rbindlist(lapply(names(CONTEOS), \(v)
    tidy_es(feols(as.formula(sprintf("%s ~ i(rel, treated, ref = -1) | %s", v, FE_PRINCIPAL)),
                  data = d, cluster = ~comuna))[, `:=`(serie = CONTEOS[[v]], muestra = mu)]))
}))
res[, `:=`(serie = factor(serie, levels = CONTEOS), muestra = factor(muestra, levels = MUESTRAS))]
print(dcast(res, serie + event_time ~ muestra, value.var = "estimate"), digits = 2)

fig <- grafico_es(res, "muestra", facetas = ~serie, escalas = "fixed",
                  y = sprintf("Cambio en el número de estaciones a %d km", RTREAT)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50")
guardar(fig, "persistencia.pdf", alto = 2.4)
