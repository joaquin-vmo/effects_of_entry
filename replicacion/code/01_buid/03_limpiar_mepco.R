# importamos y preparamos la serie del mepco

library(readxl)
library(dplyr)
library(readr)
library(here)

mepco <- read_excel(
  here("data", "input", "mepco.xlsx"),
  # la columna 8 es un separador vacio de la planilla
  col_types = c("date", rep("text", 6), "skip", rep("text", 3)),
  col_names = c("date", "93_w/o", "93_w/", "97_w/o", "97_w/", "di_w/o", "di_w/",
                "93_variable_specific_tax_utm_m3", "97_variable_specific_tax_utm_m3",
                "di_variable_specific_tax_utm_m3"),
  skip = 2  # encabezado y sub-encabezado ("Precio sin/con Mepco")
) |>
  mutate(date = as.Date(date), across(-date, as.numeric)) |>
  arrange(date)


# la serie debe ser de jueves consecutivos: un hueco o una fila espuria desalinearia el costo respecto de la grilla semanal del panel
if (any(diff(mepco$date) != 7) || any(format(mepco$date, "%u") != "4"))  # 4 = jueves
  warning("la serie MEPCO no es de jueves consecutivos")

write_csv(mepco,
          here("data", "procesado", "costo_mayorista_mepco.csv"), 
          na = "")

message(sprintf("03_limpiar_mepco.R: %d semanas, %s a %s -> %s", nrow(mepco),
                min(mepco$date), max(mepco$date),
                here("data", "procesado", "costo_mayorista_mepco.csv")))

