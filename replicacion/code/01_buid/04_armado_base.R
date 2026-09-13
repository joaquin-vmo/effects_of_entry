# 04_armado_base.R
#
# una fila por estacion-fecha-combustible (ultima lectura del dia) y marca de cadena.
# data/procesado/precios.csv -> data/procesado/base.csv

library(dplyr)
library(readr)
library(here)

precios <- read_csv(
  here("data", "procesado", "precios.csv"),
  col_types = cols_only(
    id = col_character(), date = col_date(), fuel = col_character(),
    price = col_double(), distributor = col_character(),
    municipality = col_character(), region = col_character(),
    latitud = col_double(), longitud = col_double(), time = col_time()
  ),
  locale = locale(encoding = "UTF-8")
)

base <- precios |>
  # cadena: con bandera y mas de una estacion
  mutate(is_franchise = distributor != "sin bandera" & n_distinct(id) > 1,
         .by = distributor) |>
  arrange(id, date, fuel, time) |>
  slice_tail(n = 1, by = c(id, date, fuel)) |>
  select(-time)

write_csv(base,
          here("data", "procesado", "base.csv"),
          na = "")

message(sprintf(
  "04_armado_base.R: %s lecturas -> %s estacion-fecha-combustible; %d de %d estaciones de cadena -> %s",
  nrow(precios), nrow(base),
  n_distinct(base$id[base$is_franchise]), n_distinct(base$id),
  here("data", "procesado", "base.csv")))
