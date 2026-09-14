# 03_fischer_semanal.R
#
# estudio de eventos principal con el diseno de Fischer et al. (2025): panel semanal,
# semana de la entrada, bins de 26 semanas y cohortes trimestrales en Sun y Abraham.
# panel_semanal.csv.gz + panel_mensual.csv + entradas.csv -> output/graficos/fischer_semanal.pdf

library(fixest)
library(ggplot2)
library(here)

source(here("code", "00_utilidades.R"))

SEM_BIN <- 26L                                                # semanas por bin
FUEL    <- c(p93 = "93", p95 = "95", p97 = "97", pdi = "di")  # codigos del panel semanal
FE_SEM  <- "station_key + wi + region^year + distribuidor^year"
SERIES  <- c("Mensual, TWFE", "Semanal, TWFE", "Semanal, Sun y Abraham")

p <- PANELES[["2012_2026"]]

w <- fread(file.path(p$dir, "panel_semanal.csv.gz"))
w[, `:=`(ym = as.IDate(ym), g_entry = as.IDate(g_entry), g2_entry = as.IDate(g2_entry))]
w[, year := year(ym)]

# misma muestra que muestra_estacion(), con filas semanales
d <- w[role_entry %in% c("treated", CTRL_ESTRICTO) & base == TRUE & (is.na(g2_entry) | ym < g2_entry)]
d <- d[!(role_entry == "treated" & year(g_entry) < p$focal)]
d[, treated := as.integer(role_entry == "treated")]

# semana de la entrada: primera semana con precio de la entrante competidora a <= RTREAT
# km que abrio en el mes g_entry (la regla de 05_armado_panel.R, a nivel de semana)
e <- fread(file.path(p$dir, "entradas.csv"))[, g := as.IDate(g)]
e <- merge(e, w[!is.na(price), .(wi_e = min(wi)), by = station_key], by = "station_key")
tr <- unique(d[treated == 1L, .(station_key, g_entry, lat, lon, distribuidor)])
m <- merge(tr, e, by.x = "g_entry", by.y = "g", allow.cartesian = TRUE, suffixes = c("", ".e"))
m[, km := diag(dist_km(lat, lon, elat, elon))]
m <- m[km <= RTREAT & station_key != station_key.e & (!SOLO_COMPETIDORAS | edist != distribuidor)]
ent <- m[, .(wi_entry = min(wi_e)), by = station_key]
stopifnot(nrow(ent) == nrow(tr))
d <- merge(d, ent, by = "station_key", all.x = TRUE)

d[, rel := fifelse(treated == 1L, pmax(-NBIN, pmin(NBIN, (wi - wi_entry) %/% SEM_BIN)), -1L)]
d[, wk_e := WEEK0 + wi_entry * 7L]
d[, cohorte := fifelse(treated == 1L, year(wk_e) * 4L + (month(wk_e) - 1L) %/% 3L, 1000L)]
d[, rel_sa := fifelse(treated == 1L, rel, -1000L)]

# ponytail: una cohorte con coeficientes no identificados por los efectos fijos (error
# estandar > 100 veces la mediana) sale de Sun y Abraham; en la 95 es una de dos estaciones
sunab_id <- function(x, fml) {
  m <- feols(fml("sunab(cohorte, rel_sa, no_agg = TRUE)"), data = x, cluster = ~comuna)
  se <- coeftable(m)[, 2]
  malas <- unique(as.integer(sub(".*cohort::", "", names(se)[se > 100 * median(se)])))
  if (length(malas)) message("  sin identificar, fuera de Sun y Abraham: cohorte ", paste(malas, collapse = ", "))
  feols(fml("sunab(cohorte, rel_sa)"), data = x[!cohorte %in% malas], cluster = ~comuna)
}

dm <- muestra_estacion(leer_panel(p), CTRL_ESTRICTO, p$focal)

res <- rbindlist(lapply(names(FUEL), \(fv) {
  message(fv)
  x <- d[fuel == FUEL[[fv]] & !is.na(price)]
  fml <- \(rhs) as.formula(sprintf("log(price) * 100 ~ %s | %s", rhs, FE_SEM))
  mens <- feols(as.formula(sprintf("log(%s) * 100 ~ i(rel, treated, ref = -1) | %s", fv, FE_PRINCIPAL)),
                data = dm[!is.na(get(fv))], cluster = ~comuna)
  rbind(tidy_es(mens)[, serie := SERIES[1]],
        tidy_es(feols(fml("i(rel, treated, ref = -1)"), data = x, cluster = ~comuna))[, serie := SERIES[2]],
        tidy_es(sunab_id(x, fml), "rel_sa")[, serie := SERIES[3]])[, combustible := PRECIOS[[fv]]]
}))
res[, `:=`(combustible = factor(combustible, levels = PRECIOS), serie = factor(serie, levels = SERIES))]

fig <- ggplot(res, aes(event_time, estimate, colour = serie)) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                  position = position_dodge(width = 0.6)) +
  facet_wrap(~combustible, scales = "free_y") +
  scale_x_continuous(breaks = -NBIN:NBIN) +
  labs(x = "Semestres desde la entrada", y = "Efecto sobre el precio (%)", colour = NULL) +
  theme(legend.position = "bottom")
ggsave(here("output", "graficos", "fischer_semanal.pdf"), fig, width = 9, height = 6)
