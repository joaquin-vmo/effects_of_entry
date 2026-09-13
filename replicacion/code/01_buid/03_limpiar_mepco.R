# 03_limpiar_mepco.R
#
# serie semanal de precios mayoristas con y sin MEPCO.
# data/input/mepco.xlsx -> data/procesado/costo_mayorista_mepco.csv

library(readxl)
library(dplyr)
library(readr)
library(here)

mepco <- read_excel(
  here("data", "input", "mepco.xlsx"),
  col_types = c("date", rep("text", 6), "skip", rep("text", 3)),   # col 8: separador vacio
  col_names = c("date", "93_w/o", "93_w/", "97_w/o", "97_w/", "di_w/o", "di_w/",
                "93_variable_specific_tax_utm_m3", "97_variable_specific_tax_utm_m3",
                "di_variable_specific_tax_utm_m3"),
  skip = 2
) |>
  mutate(date = as.Date(date), across(-date, as.numeric)) |>
  arrange(date)

# debe calzar con la grilla semanal de jueves del panel
if (any(diff(mepco$date) != 7) || any(format(mepco$date, "%u") != "4"))
  warning("la serie MEPCO no es de jueves consecutivos")

write_csv(mepco,
          here("data", "procesado", "costo_mayorista_mepco.csv"),
          na = "")

message(sprintf("03_limpiar_mepco.R: %d semanas, %s a %s -> %s", nrow(mepco),
                min(mepco$date), max(mepco$date),
                here("data", "procesado", "costo_mayorista_mepco.csv")))
