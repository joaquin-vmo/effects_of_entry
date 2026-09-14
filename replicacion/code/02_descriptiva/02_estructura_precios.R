# 02_estructura_precios.R
#
# composicion del precio a publico de la 93 y el diesel, promedio del ultimo anio del
# archivo de la CNE. data/input/estructura_precios_combustibles.xlsx ->
# output/graficos/estructura_precios.pdf

library(data.table)
library(ggplot2)
library(readxl)
library(here)

HOJAS <- c(GASOLINA_93 = "Gasolina 93", DIESEL = "Diésel")
# orden de la barra, de abajo hacia arriba
COMP <- c(refineria = "Precio en refinería", especifico = "Impuesto específico",
          fepp = "FEPP", iva = "IVA", margen = "Margen bruto de comercialización")

# fecha como serial de excel; "NO APLICA" y vacio = 0
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

ANIO <- min(est[, max(year(fecha)), by = combustible]$V1)
prom <- est[year(fecha) == ANIO, lapply(.SD, \(v) 100 * mean(v)), by = combustible,
            .SDcols = names(COMP)]
message(sprintf("anio %d, meses: %s", ANIO,
                paste(est[year(fecha) == ANIO, .N, by = combustible]$N, collapse = "/")))
print(prom)

pl <- melt(prom, id.vars = "combustible", variable.name = "comp", value.name = "pct")
pl[, `:=`(comp = factor(COMP[as.character(comp)], levels = COMP),
          combustible = factor(combustible, levels = HOJAS))]
pl <- pl[comp %in% pl[pct != 0, comp]]   # fuera los componentes que no aplican

# reverse: refineria abajo en la barra y primera en la leyenda
fig <- ggplot(pl, aes(combustible, pct, fill = comp)) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.6) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = NULL, y = "Porcentaje del precio a público", fill = NULL) +
  theme(legend.position = "bottom")
ggsave(here("output", "graficos", "estructura_precios.pdf"), fig, width = 6, height = 4.5)
