# 04_atenuacion_distancia.R
#
# ATT estatico por anillo de distancia a la primera entrada a <= 5 km, panel completo.
# panel_mensual.csv -> output/tablas/atenuacion_distancia.tex, output/graficos/atenuacion_distancia.pdf

library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

ANILLOS <- c("0-1 km", "1-2 km", "2-3 km", "3-4 km", "4-5 km")

p <- PANELES[["2012_2026"]]
panel <- leer_panel(p)

# tratamiento a RCTRL_B km: las columnas *5_entry toman el lugar de las de RTREAT
panel[, c("g_entry", "g2_entry", "role_entry") := NULL]
setnames(panel, c("g5_entry", "g2_5_entry", "role5_entry"), c("g_entry", "g2_entry", "role_entry"))
panel[, `:=`(g_entry = as.IDate(g_entry), g2_entry = as.IDate(g2_entry))]
d <- muestra_estacion(panel, CTRL_ESTRICTO, p$focal)

# tratadas: BIN_M meses a cada lado de la entrada; controles: todos sus meses
d <- d[treated == 0L | (miym - mi(g_entry)) %between% c(-BIN_M, BIN_M - 1L)]
d[, tpr := factor(fifelse(treated == 1L & ym >= g_entry, ring_entry, "none"),
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

n_anillo <- sapply(modelos, `[[`, "n_anillo")
n <- sapply(modelos, `[[`, "n")
extra <- c(setNames(lapply(seq_along(ANILLOS), \(i) n_anillo[i, ]),
                    sprintf("Tratadas %s", ANILLOS)),
           list(Controles = n[2, ]))
etable(ms, tex = TRUE, headers = unname(PRECIOS), replace = TRUE, extralines = extra,
       dict = setNames(ANILLOS, paste0("tpr::", ANILLOS)),
       file = here("output", "tablas", "atenuacion_distancia.tex"))

res <- rbindlist(lapply(seq_along(ms), \(i) {
  ct <- coeftable(ms[[i]])
  data.table(anillo = factor(sub("^tpr::", "", rownames(ct)), levels = ANILLOS),
             estimate = ct[, 1], se = ct[, 2],
             combustible = factor(PRECIOS[[i]], levels = PRECIOS))
}))

fig <- ggplot(res, aes(anillo, estimate)) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se)) +
  facet_wrap(~combustible, scales = "free_y") +
  labs(x = "Distancia a la entrada", y = "Efecto sobre el precio (%)")
ggsave(here("output", "graficos", "atenuacion_distancia.pdf"), fig, width = 9, height = 6)
