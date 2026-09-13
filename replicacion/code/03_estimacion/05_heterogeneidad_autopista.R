# 05_heterogeneidad_autopista.R
#
# heterogeneidad del efecto de la entrada entre estaciones sobre una autopista (a
# CERCA_M metros o menos del eje de la red primaria del MOP) y las demas. porta
# 25_het_autopista.R del proyecto anterior (figura het_autopista_es.pdf), con el
# setting de la estimacion principal: control estricto (nunca una entrada a menos de
# RCTRL_B km), FE_PRINCIPAL, cluster por comuna, panel completo. el proyecto
# anterior usaba control amplio y 200 m.
#
# por que esperar una diferencia: el diseno define el mercado local como el disco de
# RTREAT km alrededor de la incumbente. para una estacion de autopista buena parte
# de la demanda va de paso y no compara con la estacion de la calle de atras; si su
# mercado relevante es el corredor y no el barrio, una entrada a 2 km no le cambia
# el entorno competitivo. no es un efecto causal de la autopista: esas estaciones
# cobran mas y pueden diferir en marca, mercado o margen previo.
#
# una sola regresion sobre una sola muestra: el tiempo-evento se interactua con el
# indicador de tratada lejos y de tratada cerca, de modo que la trayectoria comun de
# tiempo calendario la ancla el conjunto COMPLETO de controles. la diferencia de ATT
# (post:cerca) tiene su propio error estandar.
#
# entrante fuera de la autopista: se descartan las tratadas cuya entrante esta a
# CERCA_M o menos de la red, de modo que en ambos grupos el evento es la entrada de
# una estacion de barrio (con 100 m salen 77 de 506 tratadas).
#
# red primaria = clase 1 del inventario del MOP en todo el pais (la muestra es
# nacional): incluye las autopistas urbanas de la RM y tambien carreteras
# interurbanas (Ruta 5 Panamericana, rutas internacionales, Carretera Austral), con
# cobertura dispareja entre regiones. de las tratadas a 100 m o menos, ~1/3 esta en
# la RM. distancia al eje de la via, no a un enlace.
#
# no se portan: balance, robustez de umbral, de red y de control, la carrera contra
# margen previo y tamano de mercado y la version de margenes.
#
# toma data/procesado/panel_mensual.csv y data/input/mapas/red_vial, y produce
# output/graficos/het_autopista.pdf y output/tablas/het_autopista.tex

library(data.table)
library(fixest)
library(ggplot2)
library(sf)
library(here)

source(here("code", "00_utilidades.R"))

CERCA_M <- 100
LBL <- c(sprintf("Resto (más de %d m)", CERCA_M), sprintf("Autopista (hasta %d m)", CERCA_M))

p <- PANELES[["2012_2026"]]
panel <- fread(file.path(p$dir, "panel_mensual.csv"))
panel[, `:=`(ym = as.IDate(ym), g_entry = as.IDate(g_entry), g2_entry = as.IDate(g2_entry))]
panel[, year := year(ym)]

# distancia de cada estacion y de cada entrante a la red primaria
red <- st_union(vias_rm(1, solo_rm = FALSE))
a_red <- \(lat, lon) as.numeric(st_distance(
  st_transform(st_as_sf(data.frame(lon, lat), coords = c("lon", "lat"), crs = 4326), CRS_M), red))
sloc <- coords_estacion(panel)
sloc[, d_auto := a_red(lat, lon)]
entradas <- fread(file.path(p$dir, "entradas.csv"))
entradas[, `:=`(g = mi(as.IDate(g)), d_auto = a_red(elat, elon))]

d <- muestra_estacion(panel, CTRL_ESTRICTO, p$focal)
d <- merge(d, sloc[, .(station_key, d_auto)], by = "station_key")

# LA ENTRANTE NO PUEDE ESTAR SOBRE LA AUTOPISTA. se identifican las entradas que
# tratan a cada tratada con la misma regla de 05_armado_panel.R: mismo mes que
# g_entry, a RTREAT km o menos y de otra marca (SOLO_COMPETIDORAS). si alguna esta a
# CERCA_M o menos de la red, la tratada sale de la muestra: su entrante compite por
# el trafico de paso, que es justo lo que separa a los dos grupos
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

# tabla: una columna por combustible. "Entrada" es el ATT del resto y "Entrada x
# autopista" la diferencia; el efecto en autopista (la suma) va como fila aparte
estrellas <- \(p) fifelse(p < 0.01, "$^{***}$", fifelse(p < 0.05, "$^{**}$", fifelse(p < 0.10, "$^{*}$", "")))
# TODO nota de la tabla: ATT en % del precio; especificacion principal (efectos fijos
# de estacion, mes, region x ano y marca x ano); autopista = estacion a 100 m o menos
# del eje de la red primaria del MOP (clase 1); se excluyen las tratadas cuya
# entrante esta a 100 m o menos de esa red; control: estaciones sin entradas a menos
# de 5 km; errores estandar agrupados por comuna entre parentesis; * p<0,10
# ** p<0,05 *** p<0,01
etable(lapply(res, `[[`, "m_dif"), tex = TRUE, float = FALSE, depvar = FALSE,
       headers = unname(PRECIOS), digits = "r3", fitstat = ~n, replace = TRUE,
       dict = c(DICT, post = "Entrada", `post:cerca` = "Entrada $\\times$ autopista"),
       extralines = list(
         `Efecto en autopista` = sprintf("%.3f%s (%.3f)", resumen$att_autopista, estrellas(resumen$p_autopista),
                                         resumen$se_autopista),
         `Tratadas resto` = resumen$trat_resto,
         `Tratadas autopista` = resumen$trat_autopista,
         Controles = resumen$controles),
       style.tex = style.tex("aer", yesNo = c("Sí", "No"), fixef.suffix = "",
                             tablefoot = FALSE),
       file = here("output", "tablas", "het_autopista.tex"))

es[, `:=`(combustible = factor(PRECIOS[outcome], levels = PRECIOS),
          grupo = factor(grupo, levels = LBL))]

# TODO nota de la figura: estudio de eventos de la especificacion principal con el
# tiempo-evento interactuado por grupo en una sola regresion; autopista = estacion a
# 100 m o menos del eje de la red primaria del MOP (clase 1); se excluyen las
# tratadas cuya entrante esta a 100 m o menos de esa red; bins de seis meses
# desde la entrada, extremos agrupados; referencia en -1; intervalos al 95% con
# errores agrupados por comuna; control a mas de 5 km; n de tratadas por grupo
fig <- ggplot(es, aes(event_time, estimate, colour = grupo)) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                  position = position_dodge(width = 0.4)) +
  facet_wrap(~combustible, scales = "free_y") +
  scale_x_continuous(breaks = -NBIN:NBIN) +
  labs(x = "Tiempo desde la entrada", y = "Efecto sobre el precio (%)", colour = NULL)
ggsave(here("output", "graficos", "het_autopista.pdf"), fig, width = 9, height = 6)
