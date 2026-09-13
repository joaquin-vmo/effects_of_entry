# 06_mercado_min_media_max.R
#
# efecto de la entrada sobre el PERCENTIL 90, la MEDIANA y el PERCENTIL 10 de los
# precios del mercado local, por combustible. porta 11_mercado.R y la seccion 4 de
# 16_dispersion.R del proyecto anterior (figura mercado_max_media_min de Fischer,
# Martin y Schmidt-Dengler 2025), con cuantiles en vez de maximo, media y minimo.
#
# cuantiles en vez de maximo y minimo: el maximo es el precio de UNA estacion, y en
# ~3% de los mercado-mes la mas cara esta a mas de 5% sobre la mediana del mercado, en
# episodios que duran meses. eso triplicaba el error estandar de los bins y generaba
# bins previos negativos (-0,24 en la 93); subir el minimo de competidoras lo
# empeoraba (con mas estaciones es mas probable que una este muy arriba). p90 y p10
# miden la parte cara y la barata sin depender de una sola estacion, y la mediana
# reemplaza a la media por la misma razon. cuantil tipo 7: en un mercado de dos
# estaciones el p90 (p10) queda a 90% (10%) del camino del precio mas bajo al mas alto,
# y la mediana coincide con la media.
#
# el cuerpo estima cuanto baja el precio de cada incumbente; esto responde DONDE del
# mercado se produce la baja. si cae tanto la parte cara como la barata, la ganancia
# se reparte entre todos; si cae sobre todo la barata, se concentra en quienes
# comparan precios.
#
# setting de la estimacion principal: la muestra es la de muestra_estacion() con
# control estricto (nunca una entrada a menos de RCTRL_B km), FE_PRINCIPAL, cluster
# por comuna, panel completo. cada estacion focal de esa muestra define un mercado:
# ella mas todas las estaciones con precio a RTREAT km o menos en el mes (el mismo
# radio que define el tratamiento). resultado: log del agregado x 100, de modo que el
# coeficiente es un cambio porcentual del propio agregado.
#
# decisiones:
#   - MINIMO DE COMPETIDORAS. un mercado entra solo si en promedio tuvo al menos
#     MIN_COMP competidoras con precio ademas de la focal, medido ANTES de la entrada
#     (todos sus meses si es control). medirlo con el conteo contemporaneo
#     seleccionaria sobre la propia entrada, que es lo que cambia ese conteo. ademas
#     cada mercado-mes exige que la focal tenga precio.
#   - CON Y SIN ENTRANTE. con entrante: toda estacion del radio, la entrante incluida
#     desde que abre; es el mercado que enfrenta el consumidor. sin entrante: excluye
#     del agregado a toda estacion que sea un evento de entrada (entradas.csv). con
#     entrante la parte barata baja y la cara sube por pura composicion (un sorteo
#     mas); sin entrante solo quedan incumbentes, de modo que el movimiento es
#     conducta. la version sin entrante se calcula sobre los mismos mercado-mes aunque
#     quede con una sola estacion (p10 = p90, su valor correcto).
#   - se mantiene FE_PRINCIPAL, con marca x anio de la focal. el proyecto anterior lo
#     quitaba porque el resultado es del mercado y no de la focal.
#
# toma data/procesado/panel_mensual.csv y entradas.csv, y produce
# output/tablas/mercado_<combustible>.tex y output/graficos/mercado_<combustible>.pdf

library(data.table)
library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

MIN_COMP <- 1L   # competidoras minimas (ademas de la focal), en promedio antes de la entrada.
                 # 1 = al menos dos estaciones, el criterio de Fischer et al. con 2 los controles
                 # caen de 191 a 91 (son mercados ralos: nunca una entrada a menos de 5 km)
CUANTILES <- c(p90 = 0.90, p50 = 0.50, p10 = 0.10)   # los agregados del mercado
SERIES <- c(p90 = "Percentil 90", p50 = "Mediana", p10 = "Percentil 10")
VERSIONES <- c(con = "Con entrante", sin = "Sin entrante")

p <- PANELES[["2012_2026"]]
panel <- fread(file.path(p$dir, "panel_mensual.csv"))
panel[, `:=`(ym = as.IDate(ym), g_entry = as.IDate(g_entry), g2_entry = as.IDate(g2_entry))]
panel[, year := year(ym)]
entrantes <- unique(fread(file.path(p$dir, "entradas.csv"))$station_key)

d <- muestra_estacion(panel, CTRL_ESTRICTO, p$focal)
d[, post := as.integer(treated == 1L & ym >= g_entry)]

estimar <- function(z, y, rhs) feols(as.formula(sprintf("log(%s) * 100 ~ %s | %s", y, rhs, FE_PRINCIPAL)),
                                     data = z, cluster = ~comuna)

for (fv in names(PRECIOS)) {
  z <- mercado_local(panel, d, entrantes, fv, CUANTILES, MIN_COMP)   # 00_utilidades.R
  cols <- as.vector(outer(names(SERIES), names(VERSIONES), paste, sep = "_"))   # p90_con, ...

  # tabla: ATT estatico, una columna por agregado y version
  modelos <- lapply(cols, estimar, z = z, rhs = "post")
  n <- sapply(modelos, n_estaciones, d = z)
  # TODO nota de la tabla: <combustible>; ATT en % del cuantil del mercado (log x 100
  # del percentil 90, la mediana y el percentil 10 de los precios); mercado = estacion
  # focal y estaciones con precio a 2 km o menos; con entrante incluye a la estacion
  # que abre, sin entrante excluye a toda entrante; mercados con al menos 1 competidora
  # en promedio antes de la entrada; especificacion principal (efectos fijos de
  # estacion, mes, region x ano y marca x ano); control: estaciones sin entradas a
  # menos de 5 km; errores estandar agrupados por comuna entre parentesis; * p<0,10
  # ** p<0,05 *** p<0,01
  etable(modelos, tex = TRUE, float = FALSE, depvar = FALSE, digits = "r3", fitstat = ~n,
         headers = list("^:_:" = list(`Con entrante` = 3, `Sin entrante` = 3),
                        rep(unname(SERIES), 2)),
         dict = c(DICT, post = "Entrada"), replace = TRUE,
         extralines = list(Tratadas = n[1, ], Controles = n[2, ]),
         style.tex = style.tex("aer", yesNo = c("Sí", "No"), fixef.suffix = "",
                               tablefoot = FALSE),
         file = here("output", "tablas", sprintf("mercado_%s.tex", fv)))

  # grafico: estudio de eventos de los tres agregados, con y sin entrante
  es <- rbindlist(lapply(cols, \(y) {
    tidy_es(estimar(z, y, "i(rel, treated, ref = -1)"))[
      , `:=`(serie = SERIES[[sub("_.*", "", y)]], version = VERSIONES[[sub(".*_", "", y)]])]
  }))
  es[, `:=`(serie = factor(serie, levels = SERIES), version = factor(version, levels = VERSIONES))]
  # TODO nota de la figura: <combustible>; estudio de eventos sobre el log del percentil
  # 90, la mediana y el percentil 10 de los precios del mercado (radio de 2 km); bins de
  # seis meses desde la entrada, extremos agrupados; referencia en -1; intervalos al 95%
  # con errores agrupados por comuna; resto como la nota de la tabla
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
