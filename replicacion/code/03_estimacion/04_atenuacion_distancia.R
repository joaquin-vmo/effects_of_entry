# 04_atenuacion_distancia.R
#
# atenuacion del efecto de la entrada con la distancia. misma estimacion que la
# principal (panel mensual, FE_PRINCIPAL, cluster por comuna, control estricto:
# nunca una entrada a menos de RCTRL_B km); solo cambia la definicion de
# tratamiento: cada estacion con una entrada a <= RCTRL km se asigna al anillo de su
# PRIMERA entrada dentro de ese radio (ring_entry: 0-1 ... 4-5 km), y se estima un
# ATT estatico por anillo. el perfil entre anillos es el decaimiento espacial.
#
# un perfil que cae desde 0-1 km es consistente con competencia local; un perfil
# plano apuntaria a composicion (tratadas vs controles distintos) y no a la entrada.
#
# diferencias con 04_atenuacion.R del proyecto anterior, que estimaba en panel
# trimestral, ventana de +-4 trimestres y efectos fijos region x trimestre: aqui se
# usa la especificacion principal, con ventana de +-6 meses para las tratadas.
# tampoco se replica la version de bandas de 500 m.
#
# toma data/procesado/panel_mensual.csv (panel completo) y produce
# output/tablas/atenuacion_distancia.tex y output/graficos/atenuacion_distancia.pdf

library(data.table)
library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

ANILLOS <- c("0-1 km", "1-2 km", "2-3 km", "3-4 km", "4-5 km")

p <- PANELES[["2012_2026"]]
panel <- fread(file.path(p$dir, "panel_mensual.csv"))
panel[, `:=`(ym = as.IDate(ym), g5_entry = as.IDate(g5_entry), g2_5_entry = as.IDate(g2_5_entry))]
panel[, year := year(ym)]

# las mismas cuatro restricciones de muestra_estacion(), con el tratamiento a RCTRL km
d <- panel[role5_entry %in% c("treated", CTRL_ESTRICTO)]
d <- d[is.na(g2_5_entry) | ym < g2_5_entry]
d <- d[!(role5_entry == "treated" & year(g5_entry) < p$focal)]
d <- d[base == TRUE]
d[, treated := as.integer(role5_entry == "treated")]
# ventana: las tratadas solo aportan BIN_M meses a cada lado de la entrada (-6 a +5),
# es decir el bin de referencia y el primero post del event study principal. el ATT
# es el efecto de los primeros seis meses. sin ventana el post mezcla horizontes muy
# distintos entre cohortes tempranas y tardias. los controles aportan todos sus meses
d <- d[treated == 0L | (miym - mi(g5_entry)) %between% c(-BIN_M, BIN_M - 1L)]
# una sola variable categorica: "none" para todo lo no tratado y el anillo en el
# periodo post, de modo que i() entrega un att por anillo
d[, tpr := factor(fifelse(treated == 1L & ym >= g5_entry, ring_entry, "none"),
                  levels = c("none", ANILLOS))]

modelos <- lapply(names(PRECIOS), \(fv) {
  dd <- d[!is.na(get(fv))]
  m <- feols(as.formula(sprintf("log(%s) * 100 ~ i(tpr, ref = 'none') | %s", fv, FE_PRINCIPAL)),
             data = dd, cluster = ~comuna)
  u <- dd[obs(m)]
  list(m = m, n_anillo = u[treated == 1L, uniqueN(station_key), keyby = ring_entry][ANILLOS, V1],
       n = n_estaciones(m, dd))
})
ms <- lapply(modelos, `[[`, "m")

# tabla: filas = anillos, columnas = combustibles
n_anillo <- sapply(modelos, `[[`, "n_anillo")
n <- sapply(modelos, `[[`, "n")
extra <- c(setNames(lapply(seq_along(ANILLOS), \(i) n_anillo[i, ]),
                    sprintf("Tratadas %s", ANILLOS)),
           list(Controles = n[2, ]))
# TODO nota de la tabla: ATT estatico por anillo de distancia de la incumbente a su
# primera entrada a <= 5 km, en % del precio; ventana de 6 meses antes y despues de
# la entrada para las tratadas; errores estandar agrupados por comuna
# entre parentesis; * p<0,10 ** p<0,05 *** p<0,01; control: estaciones sin entradas
# a menos de 5 km
etable(ms, tex = TRUE, float = FALSE, depvar = FALSE, headers = unname(PRECIOS),
       dict = c(DICT, setNames(ANILLOS, paste0("tpr::", ANILLOS))),
       digits = "r3", fitstat = ~n, replace = TRUE, extralines = extra,
       style.tex = style.tex("aer", yesNo = c("Sí", "No"), fixef.suffix = "",
                             tablefoot = FALSE),
       file = here("output", "tablas", "atenuacion_distancia.tex"))

# grafico
res <- rbindlist(lapply(seq_along(ms), \(i) {
  ct <- coeftable(ms[[i]])
  data.table(anillo = factor(sub("^tpr::", "", rownames(ct)), levels = ANILLOS),
             estimate = ct[, 1], se = ct[, 2],
             combustible = factor(PRECIOS[[i]], levels = PRECIOS))
}))

# TODO nota de la figura: ATT estatico por anillo de distancia a la primera entrada a
# <= 5 km; ventana de 6 meses antes y despues de la entrada; intervalos al 95% con
# errores agrupados por comuna; control: estaciones sin entradas a menos de 5 km
fig <- ggplot(res, aes(anillo, estimate)) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se)) +
  facet_wrap(~combustible, scales = "free_y") +
  labs(x = "Distancia a la entrada", y = "Efecto sobre el precio (%)")
ggsave(here("output", "graficos", "atenuacion_distancia.pdf"), fig, width = 9, height = 6)
