# 02_estructura_precios.R
#
# composicion del precio a publico por combustible: precio en refineria, margen
# bruto de comercializacion, impuesto especifico, IVA y FEPP. fuente: desglose
# porcentual mensual de la CNE para la region metropolitana, una hoja por
# combustible. se promedian los meses del anio mas reciente disponible, que con el
# archivo actual es 2021 (enero a diciembre; la serie termina en 2021-12).
#
# solo gasolina 93 y diesel: el archivo no trae 95 ni 97, y kerosene y gas licuado
# no son parte del analisis (el gas licuado ademas termina en 2014).
#
# toma data/input/estructura_precios_combustibles.xlsx y produce
# output/graficos/estructura_precios.pdf

library(data.table)
library(ggplot2)
library(readxl)
library(here)

HOJAS <- c(GASOLINA_93 = "Gasolina 93", DIESEL = "Diésel")
# orden de la barra, de abajo hacia arriba: primero el precio en refineria, luego
# impuestos y FEPP, y al final el margen bruto de comercializacion
COMP <- c(refineria = "Precio en refinería", especifico = "Impuesto específico",
          fepp = "FEPP", iva = "IVA", margen = "Margen bruto de comercialización")

# una fila por mes desde la fila 7; la fecha viene como serial de excel. "NO APLICA"
# y las celdas vacias son cero: el componente no existe para ese combustible o mes
est <- rbindlist(lapply(names(HOJAS), \(h) {
  x <- setDT(suppressMessages(read_excel(here("data", "input", "estructura_precios_combustibles.xlsx"),
                                         sheet = h, skip = 5, col_names = FALSE)))
  setnames(x, 1:6, c("serial", "refineria", "margen", "iva", "especifico", "fepp"))  # orden del excel
  x[, fecha := as.Date(suppressWarnings(as.numeric(serial)), origin = "1899-12-30")]
  x <- x[!is.na(fecha)]
  x[, (names(COMP)) := lapply(.SD, \(v) nafill(suppressWarnings(as.numeric(v)), fill = 0)),
    .SDcols = names(COMP)]
  x[, c("fecha", names(COMP)), with = FALSE][, combustible := HOJAS[[h]]]
}))

ANIO <- min(est[, max(year(fecha)), by = combustible]$V1)   # 2021 con el archivo actual
prom <- est[year(fecha) == ANIO, lapply(.SD, \(v) 100 * mean(v)), by = combustible,
            .SDcols = names(COMP)]
message(sprintf("anio %d, meses: %s", ANIO,
                paste(est[year(fecha) == ANIO, .N, by = combustible]$N, collapse = "/")))
print(prom)

pl <- melt(prom, id.vars = "combustible", variable.name = "comp", value.name = "pct")
pl[, `:=`(comp = factor(COMP[as.character(comp)], levels = COMP),
          combustible = factor(combustible, levels = HOJAS))]
pl <- pl[comp %in% pl[pct != 0, comp]]   # fuera los componentes que no aplican (FEPP en 2021)

# TODO nota de la figura: promedio de los desgloses mensuales de 2021 para la region
# metropolitana; fuente: CNE. en 2021 el FEPP no aplica a la 93 ni al diesel (rige el
# MEPCO), y el impuesto especifico incluye su componente variable
# reverse: ggplot apila el primer nivel arriba; asi el refineria queda abajo y la
# leyenda se lee en el mismo orden que la barra
fig <- ggplot(pl, aes(combustible, pct, fill = comp)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = NULL, y = "Porcentaje del precio a público", fill = NULL)
ggsave(here("output", "graficos", "estructura_precios.pdf"), fig, width = 7, height = 5)
