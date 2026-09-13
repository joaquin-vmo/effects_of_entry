# 05_heterogeneidad_autopista.R
#
# efecto de la entrada en estaciones a <= CERCA_M de la red primaria del MOP frente al
# resto, en una sola regresion. panel_mensual.csv + entradas.csv + red vial ->
# output/tablas/het_autopista.tex, output/graficos/het_autopista.pdf

library(fixest)
library(ggplot2)
library(sf)
library(here)

source(here("code", "00_utilidades.R"))

CERCA_M <- 100
LBL <- c(sprintf("Resto (más de %d m)", CERCA_M), sprintf("Autopista (hasta %d m)", CERCA_M))

p <- PANELES[["2012_2026"]]
panel <- leer_panel(p)

red <- st_union(vias_rm(1, solo_rm = FALSE))
a_red <- \(lat, lon) as.numeric(st_distance(
  st_transform(st_as_sf(data.frame(lon, lat), coords = c("lon", "lat"), crs = 4326), CRS_M), red))
sloc <- coords_estacion(panel)
sloc[, d_auto := a_red(lat, lon)]
entradas <- fread(file.path(p$dir, "entradas.csv"))
entradas[, `:=`(g = mi(as.IDate(g)), d_auto = a_red(elat, elon))]

d <- muestra_estacion(panel, CTRL_ESTRICTO, p$focal)
d <- merge(d, sloc[, .(station_key, d_auto)], by = "station_key")

# fuera las tratadas cuya entrante esta sobre la red. las entradas que tratan a cada una
# se reconstruyen con la regla de 05_armado_panel.R (mismo mes, <= RTREAT, otra marca)
trat <- unique(d[treated == 1L, .(station_key, g = mi(g_entry), distribuidor, dist_entry)])
trat <- merge(trat, sloc[, .(station_key, lat, lon)], by = "station_key")
trat[, c("d_min", "entrante_autopista") := {
  e <- entradas[g == .BY$g & (is.na(edist) | is.na(distribuidor) | edist != distribuidor)]
  de <- as.vector(dist_km(lat, lon, e$elat, e$elon))
  ok <- de <= RTREAT
  .(round(min(de[ok]), 3), any(e$d_auto[ok] <= CERCA_M))
}, by = .(station_key, g)]
stopifnot(all.equal(trat$d_min, trat$dist_entry))   # misma entrada que el panel
d <- d[!station_key %in% trat[entrante_autopista == TRUE, station_key]]
message(sprintf("tratadas fuera por entrante a %d m o menos de la red: %d de %d",
                CERCA_M, sum(trat$entrante_autopista), nrow(trat)))

d[, cerca := as.integer(d_auto <= CERCA_M)]
d[, post := as.integer(treated == 1L & ym >= g_entry)]
d[, `:=`(post_lejos = post * (1L - cerca), post_cerca = post * cerca,
         trat_lejos = treated * (1L - cerca), trat_cerca = treated * cerca)]

res <- lapply(names(PRECIOS), \(fv) {
  dd <- d[!is.na(get(fv))]
  lhs <- sprintf("log(%s) * 100", fv)
  f <- \(rhs) feols(as.formula(sprintf("%s ~ %s | %s", lhs, rhs, FE_PRINCIPAL)),
                    data = dd, cluster = ~comuna)

  m_es <- f("i(rel, trat_lejos, ref = -1) + i(rel, trat_cerca, ref = -1)")
  ct <- coeftable(m_es)
  es <- data.table(event_time = as.integer(sub("^rel::(-?\\d+):.*", "\\1", rownames(ct))),
                   grupo = LBL[grepl("trat_cerca$", rownames(ct)) + 1L],
                   estimate = ct[, 1], se = ct[, 2])
  es <- rbind(es, data.table(event_time = -1L, grupo = LBL, estimate = 0, se = 0))

  att <- coeftable(f("post_lejos + post_cerca"))
  m_dif <- f("post + post:cerca")
  dif <- coeftable(m_dif)["post:cerca", ]
  u <- dd[obs(m_es)]
  resumen <- data.table(
    outcome = fv,
    att_resto = att["post_lejos", 1], se_resto = att["post_lejos", 2],
    att_autopista = att["post_cerca", 1], se_autopista = att["post_cerca", 2],
    p_autopista = att["post_cerca", 4],
    dif = dif[1], se_dif = dif[2], p_dif = dif[4],
    trat_resto = uniqueN(u[trat_lejos == 1L, station_key]),
    trat_autopista = uniqueN(u[trat_cerca == 1L, station_key]),
    controles = uniqueN(u[treated == 0L, station_key]))
  list(es = es[, outcome := fv], resumen = resumen, m_dif = m_dif)
})
es <- rbindlist(lapply(res, `[[`, "es"))
resumen <- rbindlist(lapply(res, `[[`, "resumen"))
cat("\n=== ATT por grupo (% del precio) y diferencia autopista - resto ===\n")
print(resumen[, lapply(.SD, \(v) if (is.numeric(v)) round(v, 3) else v)])

# "Entrada" = ATT del resto; "Entrada x autopista" = diferencia; la suma va como fila
etable(lapply(res, `[[`, "m_dif"), tex = TRUE, headers = unname(PRECIOS), replace = TRUE,
       dict = c(post = "Entrada", `post:cerca` = "Entrada $\\times$ autopista"),
       extralines = list(
         `Efecto en autopista` = sprintf("%.3f%s (%.3f)", resumen$att_autopista, estrellas(resumen$p_autopista),
                                         resumen$se_autopista),
         `Tratadas resto` = resumen$trat_resto,
         `Tratadas autopista` = resumen$trat_autopista,
         Controles = resumen$controles),
       file = here("output", "tablas", "het_autopista.tex"))

es[, `:=`(combustible = factor(PRECIOS[outcome], levels = PRECIOS),
          grupo = factor(grupo, levels = LBL))]

fig <- ggplot(es, aes(event_time, estimate, colour = grupo)) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                  position = position_dodge(width = 0.4)) +
  facet_wrap(~combustible, scales = "free_y") +
  scale_x_continuous(breaks = -NBIN:NBIN) +
  labs(x = "Tiempo desde la entrada", y = "Efecto sobre el precio (%)", colour = NULL)
ggsave(here("output", "graficos", "het_autopista.pdf"), fig, width = 9, height = 6)
