# 01_limpiar_precios

# objetivo: unir todos los años de los precios históricos de bencina en linea. se arma una base con una fila por precio, el último de cada día. 

# toma los data/input/bencina_en_linea y produce data/procesado/precios

# librerias

library(dplyr)
library(here)
library(readr)
library(purrr)
library(stringr)

# parámetros

ANIOS <- 2012:2026
CORTE_REGIMEN <- as.Date("2023-01-01")

SENTINELAS <- c(0, 99999999)
PRECIO_MIN <- 200
PRECIO_MAX <- 5000

TOL_MEDIANA <- 0.35

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

# funciones 

# renombrar
recodificar <- function(x, dic) replace_values(x, from = names(dic), to = unname(dic))

# imprime las filas que quedan tras cada paso para hacer seguimiento de com va quedando la base
informar <- function(d, paso) {
  message(sprintf(" %-50s %s filas",
                  paso,
                  format(nrow(d), big.mark = ".", decimal.mark = ","))
          )
  d
}

# ahora hay que leer las bases considerandoe el cambio de régimen
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
    filter(!id %in% c("prueba", "CNE 01")) |> # hay registros de prueba en la base
    mutate(
      id = tolower(trimws(replace_values(id, "co730401 co730401" ~ "co730401"))),
      distributor = recodificar(tolower(trimws(distributor)), DISTRIBUIDOR),
      municipality = normalizar_lugar(recodificar(tolower(trimws(municipality)), COMUNA)),
      # caracter de formato invisible
      region = tolower(trimws(str_remove_all(region, "\\p{Cf}"))), 
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
        abs(price / 10 - med) / med <=TOL_MEDIANA ~ price / 10,
        abs(price / 100 - med) / med <= TOL_MEDIANA ~ price / 100
      )
    ) |>
    ungroup() |>
    filter(between(price, PRECIO_MIN, PRECIO_MAX)) |>
    select(-anio, -med)
}

# ahora aplicamos todas estas funciones

message(" regimen antiguo (2012-2022)")
legacy <- leer_regimen(ANIOS[ANIOS < 2023], delim = ";", columnas = c(
  "id", "legal_name", "distributor", "address_street", "address_number",
  "municipality", "region", "price", "date", "fuel", "latitud", "longitud")) |>
  select(-legal_name, -address_street, -address_number) |>
  informar("leidas") |>
  limpiar_campos() |>
  informar("sin registros de prueba y con coordenadas validas") |>
  limpiar_precios() |>
  informar("combustibles del analisis con precio valido")

message("regimen actual (2023-2026)")
current <- leer_regimen(ANIOS[ANIOS >= 2023], delim = ",", columnas = c(
  "id", "legal_name", "distributor", "address", "latitud", "longitud",
  "municipality", "region", "fuel", "price", "pricing_unit", "service_type",
  "date", "time", "ev_station", "gas_station")) |>
  select(-legal_name, -address, -pricing_unit, -service_type) |>
  informar("leidas") |>
  limpiar_campos() |>
  informar("sin registros de prueba y con coordenadas validas") |>
  mutate(time = hms::as_hms(time)) |>
  # una coordenada por estacion: la mas frecuente (README 3.5)
  mutate(n = n(), .by = c(id, latitud, longitud)) |>
  mutate(latitud  = latitud[which.max(n)],
         longitud = longitud[which.max(n)], .by = id) |>
  select(-n)

# lecturas por modalidad, para 02_diagnostico_empalme.R
modalidad <- current |>
  filter(date >= CORTE_REGIMEN,
         fuel %in% c("93", "95", "97", "DI", "A93", "A95", "A97", "ADI")) |>
  count(id, asistido = fuel %in% names(COMBUSTIBLE))

current <- current |>
  limpiar_precios() |>
  informar("combustibles asistidos con precio valido")

# hay un tema importante con las ubicaciones
# estaciones que se teletransportan al cambio de régimen
# se hace el supuesto que las ubicaciones del nuevo régmine son más exactas

ubicacion <- current |>
  count(id, latitud, longitud, municipality, region) |>
  slice_max(n, n = 1, with_ties = FALSE, by = id) |>
  select(-n)

saveRDS(list(legacy = legacy, current = filter(current, date < CORTE_REGIMEN),
             ubicacion = ubicacion, modalidad = modalidad),
        here("data", "procesado", "precios_regimenes.rds"))

legacy <- legacy |> rows_update(ubicacion, by = "id", unmatched = "ignore")

# empalme para la base final

precios <- bind_rows(
  filter(legacy,  date <  CORTE_REGIMEN),
  filter(current, date >= CORTE_REGIMEN)
) |>
  informar("empalme") |>
  distinct() |>  # el mismo registro leido dos veces
  informar("sin filas duplicadas") |>
  arrange(id, date, fuel, time)

write_csv(precios, here("data", "procesado", "precios.csv"), na = "")

message(sprintf("01_clean_prices.R: %s estaciones, %s a %s -> %s",
                n_distinct(precios$id), min(precios$date), max(precios$date),
                here("data", "procesado", "precios.csv")))
