# 01_limpiar_precios.R
#
# une y limpia los precios de bencina en linea de ambos regimenes de reporte.
# data/input/bencina_en_linea/*.csv -> data/procesado/precios.csv, precios_regimenes.rds

library(dplyr)
library(here)
library(readr)
library(purrr)
library(stringr)

ANIOS <- 2012:2026
CORTE_REGIMEN <- as.Date("2023-01-01")   # inicio del regimen actual

SENTINELAS <- c(0, 99999999)
PRECIO_MIN <- 200
PRECIO_MAX <- 5000
TOL_MEDIANA <- 0.35   # tolerancia al corregir precios con decimales corridos

COMBUSTIBLE <- c("Gasolina 93" = "93", "Gasolina 95" = "95",
                 "Gasolina 97" = "97", "Petroleo Diesel" = "di",
                 "93" = "93", "95" = "95", "97" = "97", "DI" = "di")

DISTRIBUIDOR <- c("vigu ltda." = "vigu", "ruta 45" = "ruta v45",
                  "go abastible" = "abastible",
                  "gasco autogas" = "autogasco",
                  "hola" = "hola!", "esa" = "sesa",
                  "petrobras" = "aramco_petrobras",
                  "aramco" = "aramco_petrobras")

COMUNA <- c("aisén" = "aysen",
            "santiago centro" = "santiago")

REGION <- c(
  # antiguo
  "bío bío" = "biobio", "biobío" = "biobio",
  "gral. bernardo o'higgins" = "ohiggins",
  "los ríos" = "los_rios", "los lagos" = "los_lagos",
  "araucanía" = "araucania",
  "magallanes y la antártida chilena" = "magallanes_antartida",
  "aysén gral. c. ibáñez del campo" = "aysen",
  # actual
  "metropolitana de santiago" = "metropolitana",
  "del biobío" = "biobio",
  "del maule" = "maule", "de la araucanía" = "araucania",
  "de los lagos" = "los_lagos", "de los ríos" = "los_rios",
  "magallanes y de la antártica chilena" = "magallanes_antartida",
  "aysén del gral. carlos ibáñez del campo" = "aysen",
  # ambos
  "valparaíso" = "valparaiso", "ñuble" = "nuble",
  "tarapacá" = "tarapaca",
  "arica y parinacota" = "arica")

recodificar <- function(x, dic) replace_values(x, from = names(dic), to = unname(dic))

informar <- function(d, paso) {   # filas tras cada paso
  message(sprintf(" %-50s %s filas",
                  paso,
                  format(nrow(d), big.mark = ".", decimal.mark = ","))
          )
  d
}

leer_regimen <- function(anios, delim, columnas) {
  map(anios, \(a) {
    f <-  here("data", "input", "bencina_en_linea", paste0(a, ".csv"))
    d <- read_delim(f, delim = delim, col_types = cols(.default = col_character()),
                    locale = locale(encoding = "UTF-8"), progress = FALSE)
    if (ncol(d) != length(columnas))
      stop(basename(f), ": se esperaban ", length(columnas), " columnas")
    set_names(d, columnas)
  }) |>
    list_rbind()
}

normalizar_lugar <- function(x) {
  x |>
    chartr(old = "áéíóúüñ", new = "aeiouun") |>
    str_remove_all("['’]") |>
    str_replace_all("\\s+", "_")
}

limpiar_campos <- function(d) {
  d |>
    filter(!id %in% c("prueba", "CNE 01")) |>   # registros de prueba
    mutate(
      id = tolower(trimws(replace_values(id, "co730401 co730401" ~ "co730401"))),
      distributor = recodificar(tolower(trimws(distributor)), DISTRIBUIDOR),
      municipality = normalizar_lugar(recodificar(tolower(trimws(municipality)), COMUNA)),
      region = tolower(trimws(str_remove_all(region, "\\p{Cf}"))),   # caracter invisible
      region = if_else(str_detect(region, "libertador"), "ohiggins", region),
      region = recodificar(region, REGION),
      date = as.Date(date),
      # coma decimal en el antiguo; en Chile latitud y longitud son negativas
      across(c(latitud, longitud),
             \(x) suppressWarnings(as.numeric(str_replace(str_remove_all(x, '"'), ",", ".")))),
      latitud  = if_else(between(latitud,  -90,  0), latitud,  NA),
      longitud = if_else(between(longitud, -180, 0), longitud, NA)
    ) |>
    filter(!is.na(latitud), !is.na(longitud))
}

# precios fuera de rango que son x10 o x100 de la mediana del combustible-anio se corrigen
limpiar_precios <- function(d) {
  d |>
    filter(fuel %in% names(COMBUSTIBLE)) |>
    mutate(fuel = unname(COMBUSTIBLE[fuel]),
           price = suppressWarnings(as.numeric(price)),
           price = if_else(price %in% SENTINELAS, NA, price),
           anio = format(date, "%Y")) |>
    group_by(fuel, anio) |>
    mutate(
      med = median(price[between(price, PRECIO_MIN, PRECIO_MAX)], na.rm = TRUE),
      price = case_when(
        is.na(med) | price <= 2 * med ~ price,
        abs(price / 10 - med) / med <= TOL_MEDIANA ~ price / 10,
        abs(price / 100 - med) / med <= TOL_MEDIANA ~ price / 100
      )
    ) |>
    ungroup() |>
    filter(between(price, PRECIO_MIN, PRECIO_MAX)) |>
    select(-anio, -med)
}

anio_corte <- as.integer(format(CORTE_REGIMEN, "%Y"))

message(" regimen antiguo")
legacy <- leer_regimen(ANIOS[ANIOS < anio_corte], delim = ";", columnas = c(
  "id", "legal_name", "distributor", "address_street", "address_number",
  "municipality", "region", "price", "date", "fuel", "latitud", "longitud")) |>
  select(-legal_name, -address_street, -address_number) |>
  informar("leidas") |>
  limpiar_campos() |>
  informar("sin registros de prueba y con coordenadas validas") |>
  limpiar_precios() |>
  informar("combustibles del analisis con precio valido")

message("regimen actual")
current <- leer_regimen(ANIOS[ANIOS >= anio_corte], delim = ",", columnas = c(
  "id", "legal_name", "distributor", "address", "latitud", "longitud",
  "municipality", "region", "fuel", "price", "pricing_unit", "service_type",
  "date", "time", "ev_station", "gas_station")) |>
  select(-legal_name, -address, -pricing_unit, -service_type) |>
  informar("leidas") |>
  limpiar_campos() |>
  informar("sin registros de prueba y con coordenadas validas") |>
  mutate(time = hms::as_hms(time)) |>
  # una coordenada por estacion: la mas frecuente
  mutate(n = n(), .by = c(id, latitud, longitud)) |>
  mutate(latitud  = latitud[which.max(n)],
         longitud = longitud[which.max(n)], .by = id) |>
  select(-n)

# lecturas por modalidad (asistido / autoservicio), para 02_diagnostico_empalme.R
modalidad <- current |>
  filter(date >= CORTE_REGIMEN,
         fuel %in% c("93", "95", "97", "DI", "A93", "A95", "A97", "ADI")) |>
  count(id, asistido = fuel %in% names(COMBUSTIBLE))

current <- current |>
  limpiar_precios() |>
  informar("combustibles asistidos con precio valido")

# la ubicacion del regimen actual reemplaza a la del antiguo
ubicacion <- current |>
  count(id, latitud, longitud, municipality, region) |>
  slice_max(n, n = 1, with_ties = FALSE, by = id) |>
  select(-n)

saveRDS(list(legacy = legacy, current = filter(current, date < CORTE_REGIMEN),
             ubicacion = ubicacion, modalidad = modalidad),
        here("data", "procesado", "precios_regimenes.rds"))

legacy <- legacy |> rows_update(ubicacion, by = "id", unmatched = "ignore")

precios <- bind_rows(
  filter(legacy,  date <  CORTE_REGIMEN),
  filter(current, date >= CORTE_REGIMEN)
) |>
  informar("empalme") |>
  distinct() |>
  informar("sin filas duplicadas") |>
  arrange(id, date, fuel, time)

write_csv(precios, here("data", "procesado", "precios.csv"), na = "")

message(sprintf("01_limpiar_precios.R: %s estaciones, %s a %s -> %s",
                n_distinct(precios$id), min(precios$date), max(precios$date),
                here("data", "procesado", "precios.csv")))
