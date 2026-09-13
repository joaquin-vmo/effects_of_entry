# información respecto a las decisiones tomadas en 01_limpiar_precios.R

library(dplyr)
library(here)

CORTE_REGIMEN <- as.Date("2023-01-01")  # igual que en 01_limpiar_precios.R

r <- readRDS(here("data", "procesado", "precios_regimenes.rds"))

# autoservicio

por_estacion <- r$modalidad |>
  summarise(solo_autoservicio = !any(asistido), .by = id) |>
  filter(solo_autoservicio) |>
  mutate(en_antiguo = id %in% r$legacy$id)

cat("\n=== 1. AUTOSERVICIO, desde", format(CORTE_REGIMEN), "===\n")
cat(sprintf("lecturas de autoservicio descartadas: %.1f%%\n",
            100 * with(r$modalidad, sum(n[!asistido]) / sum(n))))
cat("estaciones sin precio asistido, con historia en el regimen antiguo:",
    sort(por_estacion$id[por_estacion$en_antiguo]), "\n")
cat("estaciones sin precio asistido, sin historia previa:",
    sort(por_estacion$id[!por_estacion$en_antiguo]), "\n")

# ubicacion
n_distintas <- r$legacy |>
  distinct(id, latitud, longitud, municipality, region) |>
  inner_join(r$ubicacion, by = "id", suffix = c("", "_actual")) |>
  filter(latitud != latitud_actual | longitud != longitud_actual |
           municipality != municipality_actual | region != region_actual) |>
  distinct(id) |>
  nrow()

cat("\n=== 2. UBICACION ===\n")
cat("estaciones del regimen antiguo cuya ubicacion se reemplaza por la del actual:",
    n_distintas, "\n")


# el archivo 2023 contiene información de años pasados por lo que podemos comparar si hay cambios entre ambos regímenes con respecto al período anterior

solape <- full_join(
  r$legacy  |> filter(date < CORTE_REGIMEN) |> distinct(id, date, fuel, .keep_all = TRUE) |>
    select(id, date, fuel, p_ant = price),
  r$current |> distinct(id, date, fuel, .keep_all = TRUE) |>
    select(id, date, fuel, p_act = price),
  by = c("id", "date", "fuel")
)

cat("\n=== 3. EMPALME: solape anterior a", format(CORTE_REGIMEN), "===\n")
solape |>
  summarise(
    en_ambas = sum(!is.na(p_ant) & !is.na(p_act)),
    pct_del_antiguo = 100 * mean(!is.na(p_act[!is.na(p_ant)])),
    pct_precio_igual = 100 * mean((p_ant == p_act)[!is.na(p_ant) & !is.na(p_act)]),
    solo_antiguo = sum(is.na(p_act)),
    est_solo_antiguo = n_distinct(id[is.na(p_act)]),
    solo_actual = sum(is.na(p_ant)),
    est_solo_actual = n_distinct(id[is.na(p_ant)])
  ) |>
  tidyr::pivot_longer(everything()) |>
  print()

# lo que se encuentra es que las estaciones que dejarond e reportar entre 2012 a 2022 no aparecen en 2023 en sus filas del pasado, por lo que es importante usar ambos regímenes

cat("ultima aparicion en el regimen antiguo, segun figuren o no en el actual:\n")
r$legacy |>
  summarise(ultima = max(date), .by = id) |>
  mutate(en_actual = id %in% r$current$id) |>
  summarise(estaciones = n(), ultima_mediana = median(ultima), .by = en_actual) |>
  print()
