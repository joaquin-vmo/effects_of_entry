# 05_armado_panel.R
#
# identidad de estaciones, panel semanal y mensual con margen MEPCO, y asignacion de
# tratamiento por ventana de PANELES.
# data/procesado/{base,costo_mayorista_mepco}.csv -> identidad_estaciones.csv y, por
# ventana, panel_semanal.csv.gz, panel_mensual.csv y entradas.csv

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(here)

source(here("code", "00_utilidades.R"))

db <- read_csv(
  here("data", "procesado", "base.csv"),
  col_types = cols_only(
    id = col_character(), date = col_date(), fuel = col_character(),
    price = col_double(), distributor = col_character(),
    municipality = col_character(), region = col_character(),
    latitud = col_double(), longitud = col_double(), is_franchise = col_logical()
  ),
  locale = locale(encoding = "UTF-8")
) |>
  rename(comuna = municipality, distribuidor = distributor)

mepco <- read_csv(here("data", "procesado", "costo_mayorista_mepco.csv"), show_col_types = FALSE)

# ---- identidad: codigos distintos del mismo local ----

idinfo <- db |>
  summarise(first_date = min(date), last_date = max(date),
            lat = median(latitud, na.rm = TRUE), lon = median(longitud, na.rm = TRUE),
            .by = id) |>
  arrange(first_date, id)

# sucesion: el padre de i es el codigo ya apagado mas cercano que empezo antes
cand <- idinfo |> filter(!is.na(lat))
D_link <- dist_propia(cand$lat, cand$lon) * 1000
padre_sucesion <- rep(NA_character_, nrow(cand))
for (i in seq_len(nrow(cand))) {
  prev <- which(cand$last_date <= cand$first_date[i] &
                  cand$first_date < cand$first_date[i])
  if (!length(prev)) next
  j <- prev[which.min(D_link[i, prev])]
  if (D_link[i, j] < LINK_M) padre_sucesion[i] <- cand$id[j]
}

# duplicacion: codigo con letra final cuyo codigo base existe
duplicados <- idinfo |>
  filter(grepl("[0-9][a-z]$", id)) |>
  transmute(id, base_codigo = sub("[a-z]$", "", id), lat, lon) |>
  inner_join(idinfo |> select(base_codigo = id, b_lat = lat, b_lon = lon),
             by = "base_codigo") |>
  mutate(metros_a_base = round(1000 * diag(dist_km(lat, lon, b_lat, b_lon)), 1)) |>
  select(id, base_codigo, metros_a_base)

idinfo <- idinfo |>
  left_join(tibble(id = cand$id, padre_sucesion), by = "id") |>
  left_join(duplicados, by = "id") |>
  mutate(
    regla = case_when(!is.na(metros_a_base) & metros_a_base <= LINK_M ~ "duplicacion",
                      !is.na(padre_sucesion)                          ~ "sucesion"),
    padre = if_else(regla %in% "duplicacion", base_codigo, padre_sucesion)
  )

padres <- setNames(idinfo$padre, idinfo$id)
raiz <- function(x) {   # sigue la cadena de padres; el tope corta un ciclo
  for (k in 1:100) {
    if (is.na(padres[[x]])) return(x)
    x <- padres[[x]]
  }
  stop("cadena de identidad sin raiz (posible ciclo) en: ", x)
}
idinfo <- idinfo |> mutate(station_key = map_chr(id, raiz))

write_csv(idinfo |> select(id, station_key, regla, padre, padre_sucesion,
                           base_codigo, metros_a_base, first_date, last_date, lat, lon),
          here("data", "procesado", "identidad_estaciones.csv"), na = "")

db <- db |> left_join(idinfo |> select(id, station_key), by = "id")

# ---- panel semanal: ultimo precio de cada semana y margen MEPCO ----

semana <- function(d) as.integer(d - WEEK0) %/% 7L

obs_w <- db |>
  mutate(wi = semana(date)) |>
  filter(wi >= 0L) |>
  arrange(station_key, fuel, date) |>
  slice_tail(n = 1, by = c(station_key, fuel, wi)) |>
  select(station_key, fuel, wi, price)

mepco_w <- mepco |>
  select(date, `93` = `93_w/`, `97` = `97_w/`, di = `di_w/`) |>
  pivot_longer(-date, names_to = "fuel", values_to = "cost") |>
  mutate(wi = semana(date), .keep = "unused")

sattr <- db |>   # atributos de estacion: el valor mas frecuente
  group_by(station_key) |>
  summarise(id = modal(id), distribuidor = modal(distribuidor),
            comuna = modal(comuna), region = modal(region),
            lat = median(latitud, na.rm = TRUE), lon = median(longitud, na.rm = TRUE),
            is_franchise = as.logical(modal(as.character(is_franchise))))

ULT_DIA <- max(db$date)   # la ultima semana y el ultimo mes incompletos se recortan

panel_w <- obs_w |>
  mutate(obs_wi = wi) |>
  group_by(station_key, fuel) |>
  complete(wi = full_seq(wi, 1L)) |>
  ungroup() |>
  mutate(observed = !is.na(price)) |>
  arrange(station_key, fuel, wi) |>
  group_by(station_key, fuel) |>
  fill(price, obs_wi, .direction = "down") |>
  ungroup() |>
  filter(wi - obs_wi <= MAXGAP_W) |>
  select(-obs_wi) |>
  left_join(mepco_w, by = c("fuel", "wi")) |>
  mutate(margin = price - cost,
         wk = WEEK0 + wi * 7L,
         ym = mi2date(mi(wk))) |>
  left_join(sattr, by = "station_key") |>
  relocate(station_key, fuel, wi, wk, ym) |>
  arrange(station_key, fuel, wi) |>
  filter(wk + 6L <= ULT_DIA)

if (max(mepco_w$wi) < max(panel_w$wi))
  warning("la serie MEPCO termina antes que el panel: el margen de las ultimas semanas queda NA")

# ---- panel mensual: ultima semana de cada mes ----

last_m <- panel_w |>
  group_by(station_key, fuel, ym) |>
  slice_tail(n = 1) |>
  ungroup()

panel_m <- list(
  last_m |> select(station_key, ym, fuel, price) |>
    pivot_wider(names_from = fuel, values_from = price, names_prefix = "p"),
  last_m |> select(station_key, ym, fuel, margin) |>
    pivot_wider(names_from = fuel, values_from = margin, names_prefix = "m") |>
    select(-any_of("m95")),                  # sin referencia MEPCO
  last_m |> summarise(observed = any(observed), .by = c(station_key, ym))
) |>
  reduce(inner_join, by = c("station_key", "ym")) |>
  left_join(sattr, by = "station_key") |>
  mutate(miym = mi(ym)) |>
  arrange(station_key, ym) |>
  filter(mi2date(miym + 1L) - 1L <= ULT_DIA)  # mes completo

primera <- panel_m |> summarise(primera_ym = min(ym), .by = station_key)

# ---- tratamiento por ventana ----

asignar_tratamiento <- function(sloc, entradas) {
  D_ev <- dist_km(sloc$slat, sloc$slon, entradas$elat, entradas$elon)
  g_ev <- mi(entradas$g)

  # no cuenta: su propia entrada, una entrada fuera de la vida de la estacion o de su marca
  D_ev[cbind(match(entradas$station_key, sloc$station_key), seq_len(nrow(entradas)))] <- Inf
  D_ev[!(outer(mi(sloc$entry_ym), g_ev, "<") & outer(mi(sloc$last_ym), g_ev, ">="))] <- Inf
  if (SOLO_COMPETIDORAS) {
    misma_marca <- outer(sloc$distribuidor, entradas$edist, "==")
    D_ev[!is.na(misma_marca) & misma_marca] <- Inf
  }

  map(seq_len(nrow(sloc)), \(i) {
    d <- D_ev[i, ]
    meses <- sort(unique(g_ev[d <= RTREAT]))
    g <- if (length(meses)) meses[1] else NA_integer_
    # anillos: la entrada mas temprana y, en el mismo mes, la mas cercana
    w5 <- which(d <= RCTRL_B)
    w5 <- w5[order(g_ev[w5], d[w5])]
    meses5 <- unique(g_ev[w5])
    mindist <- round(min(d), 3)
    tibble(
      station_key = sloc$station_key[i],
      mindist     = mindist,
      g_entry     = mi2date(g),
      g2_entry    = mi2date(if (length(meses) >= 2) meses[2] else NA_integer_),
      role_entry  = case_when(!is.na(g)          ~ "treated",
                              mindist <= RCTRL_A ~ "buffer",
                              mindist <= RCTRL_B ~ "ctrl_a",
                              TRUE               ~ "ctrl_b"),
      dist_entry  = if (is.na(g)) NA_real_ else round(min(d[d <= RTREAT & g_ev == g]), 3),
      g5_entry    = mi2date(if (length(w5)) meses5[1] else NA_integer_),
      g2_5_entry  = mi2date(if (length(meses5) >= 2) meses5[2] else NA_integer_),
      dist_entry5 = if (length(w5)) round(d[w5[1]], 3) else NA_real_,
      ring_entry  = cut(if (length(w5)) d[w5[1]] else NA_real_,
                        breaks = 0:5, include.lowest = TRUE,
                        labels = c("0-1 km", "1-2 km", "2-3 km", "3-4 km", "4-5 km")),
      role5_entry = if (length(w5)) "treated" else "ctrl_b"
    )
  }) |>
    list_rbind()
}

construir_panel <- function(nombre, p) {
  inicio <- as.Date(sprintf("%d-01-01", p$desde))
  fin    <- as.Date(p$hasta)

  pm <- panel_m |> filter(ym >= inicio, ym <= fin)
  pw <- panel_w |> filter(wk >= inicio, wk <= fin)

  # base: su primera aparicion en toda la historia es a mas tardar el anio `desde`
  st_span <- pm |>
    summarise(entry_ym = min(ym), last_ym = max(ym), .by = station_key) |>
    inner_join(sattr, by = "station_key") |>
    left_join(primera, by = "station_key") |>
    mutate(base = as.integer(format(primera_ym, "%Y")) <= p$desde)

  entradas <- st_span |>
    filter(!base, !is.na(lat)) |>
    transmute(station_key, g = entry_ym, elat = lat, elon = lon,
              eregion = region, edist = distribuidor, efranchise = is_franchise) |>
    arrange(g, station_key) |>
    mutate(event_id = paste0("EN", row_number()))

  sloc <- st_span |>
    filter(!is.na(lat)) |>
    select(station_key, slat = lat, slon = lon, distribuidor, entry_ym, last_ym)

  asg <- asignar_tratamiento(sloc, entradas)
  agregar_diseno <- function(d) {
    d |>
      left_join(asg, by = "station_key") |>
      left_join(st_span |> select(station_key, base), by = "station_key")
  }

  dir.create(p$dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(agregar_diseno(pw), file.path(p$dir, "panel_semanal.csv.gz"), na = "")
  write_csv(agregar_diseno(pm), file.path(p$dir, "panel_mensual.csv"), na = "")
  write_csv(entradas, file.path(p$dir, "entradas.csv"), na = "")

  message(sprintf(
    "  panel %s: %d estaciones (%d de base), %d entradas (%d focales desde %d) -> %s",
    nombre, nrow(st_span), sum(st_span$base), nrow(entradas),
    sum(as.integer(format(entradas$g, "%Y")) >= p$focal), p$focal, p$dir))
}

message("05_armado_panel.R")
iwalk(PANELES, \(p, nombre) construir_panel(nombre, p))
