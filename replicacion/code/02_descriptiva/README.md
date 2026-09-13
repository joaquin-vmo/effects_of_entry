# 02_descriptiva: estadísticas descriptivas

Figuras y tablas descriptivas. Todos los scripts leen el panel completo (`data/procesado/`) y son independientes entre sí. Requieren haber corrido `01_buid/`.

| Script | Insumos | Productos |
|---|---|---|
| `01_serie_precios.R` | `panel_semanal.csv.gz` | `output/graficos/serie_precios.pdf`, `serie_margen.pdf` |
| `02_estructura_precios.R` | `data/input/estructura_precios_combustibles.xlsx` | `output/graficos/estructura_precios.pdf` |
| `03_mapa_calor.R` | `panel_mensual.csv`, `data/input/mapas/{comunas,red_vial}` | `output/graficos/mapa_calor.pdf` |
| `04_autopistas.R` | `panel_mensual.csv`, `data/input/mapas/{comunas,red_vial}` | `output/graficos/autopistas.pdf` |
| `05_dispersion_precios.R` | `panel_mensual.csv` | `output/tablas/dispersion_{p93,p95,p97,pdi}.tex` |
| `06_entradas_anio.R` | `entradas.csv` (ventana `2012_2026`) | `output/graficos/entradas_anio.pdf` |
| `07_composicion_muestra.R` | `panel_mensual.csv`, `entradas.csv` (ventana `2012_2026`) | `output/tablas/composicion_muestra.tex` |

## 01_serie_precios

Promedio simple semanal entre estaciones del precio nominal ($/L) y del margen de venta, (precio − costo MEPCO) / precio en %. El margen existe desde el 2014-08-07. La 95 no tiene margen.

**Nota de las figuras.**
- *Precios*: promedio simple semanal entre estaciones; precios nominales en $/L; el precio de una semana sin reporte es el último informado (hasta 13 semanas).
- *Margen*: margen = (precio − precio mayorista con MEPCO) / precio; promedio simple semanal entre estaciones; la gasolina 95 no tiene referencia MEPCO.

## 02_estructura_precios

Desglose porcentual mensual de la CNE para la Región Metropolitana: refinería, margen bruto de comercialización, impuesto específico, IVA y FEPP. Se promedian los meses del año más reciente del archivo, que es 2021 (la serie termina en 2021-12).
Solo 93 y diésel: el archivo no trae 95 ni 97. Kerosene y gas licuado no son parte del análisis. Los componentes que valen cero todo el año (el FEPP en 2021) no se grafican.

**Nota.** Promedio de los desgloses mensuales de 2021 para la Región Metropolitana; fuente: CNE. En 2021 el FEPP no aplica a la 93 ni al diésel (rige el MEPCO), y el impuesto específico incluye su componente variable.

## 03_mapa_calor

Para cada estación del Gran Santiago (las 32 comunas de la provincia de Santiago más Puente Alto y San Bernardo) se promedia el precio de 2025. Solo cuentan las estaciones con al menos 9 meses con precio. A ese promedio se le resta el promedio de las estaciones, y la desviación se interpola sobre una grilla de 250 m.

- **Núcleo gaussiano (Nadaraya-Watson) y no IDW.** IDW converge al dato puntual cerca de cada estación y el mapa se ve como puntos. Con z(s) = Σ wᵢ(s) yᵢ / Σ wᵢ(s) y wᵢ(s) = exp(−d²/2h²), el mapa suaviza a escala h. Con h = 2 km cada celda promedia del orden de 15 a 30 estaciones. Con 1 km el mapa queda ruidoso y con 4 km se borra el contraste oriente-poniente.
- **Máscara.** Se pinta solo la grilla dentro de las 34 comunas y con al menos 2 estaciones a ≤ 2,5 km. Sin eso el suavizador extrapola sobre la precordillera y el secano. El mínimo de dos descarta islas de una sola estación (camino a Farellones, sur de San Bernardo). La máscara es la misma para los cuatro combustibles.
- **Escala.** Divergente, simétrica y recortada al percentil 99 de |z|, para que un par de celdas extremas no fijen el rango.
- **Recorte por geometría.** Las estaciones se ubican por geometría y no por el nombre de comuna del panel, que no calza con el shapefile.

**Nota.** Promedio 2025 del precio de cada estación menos el promedio de las estaciones del Gran Santiago, en $/L; suavizado con núcleo gaussiano de 2 km; solo zonas con al menos 2 estaciones a 2,5 km; líneas blancas: autopistas urbanas; puntos: estaciones; escala recortada en ± percentil 99.

## 04_autopistas

La pregunta es si las estaciones pegadas a una autopista cobran más (demanda de paso, costo de búsqueda alto). Las autopistas no se trazaron al azar: cruzan el sector oriente, que es caro por razones ajenas a la carretera.
Por eso el eje y es la desviación respecto del promedio de la propia comuna, lo que compara estaciones del mismo barrio. Sigue siendo descriptivo, no causal.

- **Autopista**: clase 1 del inventario del MOP (concesionadas urbanas y rutas nacionales).
- **Distancia al eje de la vía, no a un enlace.** Una estación a 80 m de una autopista sin acceso cercano entra como cercana, lo que atenúa cualquier patrón.
- **Fuera**: las estaciones solas en su comuna (desviación 0 por construcción) y las que están a más de 5 km, que son un puñado en el borde y estiran el eje y el loess.

**Nota.** Cada punto es una estación del Gran Santiago; precio promedio 2025 menos el promedio de las estaciones de su comuna, en $/L; distancia al eje de la autopista más cercana (clase 1 del MOP), escala logarítmica, hasta 5 km; línea punteada: 200 m; curva: loess con intervalo al 95 %.

## 05_dispersion_precios

Réplica de la Tabla 2 de Fischer, Martin y Schmidt-Dengler (2025).

**Mercado.** La estación focal más todas las estaciones activas a ≤ r km (r = 1 y 2), en cada mes. Solo entran mercado-mes con al menos dos estaciones con precio y en que la focal tiene precio.

**Medidas por mercado-mes:**
- precio medio, mínimo y máximo;
- desviación estándar;
- rango = máximo − mínimo, la ganancia máxima posible de buscar;
- ahorro esperado de buscar = medio − mínimo, cuánto paga de más quien elige una estación al azar en vez de la más barata.

La tabla reporta la distribución de cada medida entre mercado-mes.

- **Un solo año (2025).** El panel cubre un ciclo completo del petróleo: agrupar años daría percentiles de calendario (la 93 va de ~690 a más de 1.400 $/L).
- **Precios con arrastre.** Se usan tal cual: una estación que no informa no cambió su precio.
- **Diferencia con Fischer et al.** No se excluyen los mercados cuya focal es entrante: esa exclusión importa para el estudio de eventos, no para describir un año. Con la exclusión quedarían 916 mercados de 1 km en la 93 en vez de 1.127, y las medias casi no cambian.

**Nota.** ⟨combustible⟩, 2025, $/L; mercado = estación focal y todas las estaciones con precio a 1 (2) km o menos en el mes; solo mercado-mes con al menos dos estaciones; ahorro esperado de buscar = precio medio − precio mínimo; percentiles, media y desviación estándar de cada medida entre mercado-mes; fuente: CNE. Replica la Tabla 2 de Fischer, Martin y Schmidt-Dengler (2025).

## 06_entradas_anio

Cantidad de estaciones entrantes por año de primera aparición, 2013-2026 (ventana `2012_2026`: base = presentes en 2012, entradas = todo lo posterior). Ver nota sobre 2013 en `01_buid/README.md` §9: 79 entradas ese año frente a 20-50 en un año normal, 43 de bandera blanca, es incorporación tardía al reporte y no apertura real.

**Nota.** Estaciones con primera aparición en el registro cada año, 2013-2026; fuente: bencinaenlinea.cl; 2026 incompleto (el registro corta en julio).

## 07_composicion_muestra

Estaciones por rol en la ventana `2012_2026` (tabla 7, "Composición de la muestra", del proyecto anterior, sin las filas de salidas). Los roles se cuentan sobre todas las estaciones del panel; la muestra de estimación son las estaciones que usa la especificación principal de la 93 (`muestra_estacion()` + `FE_PRINCIPAL`, sin singletons), de modo que coincide con las tablas de `03_estimacion/`.

De 849 tratadas a 501 en la muestra: 117 no son de base, 226 tienen su primera entrada en 2013 y 5 no tienen precio de 93. Control estricto, de 662 a 373: 276 no son de base, 9 no tienen precio de 93 y 4 son singletons.

**Nota.** Conteos sobre el panel mensual y las entradas de la ventana 2012–2026, después de unificar los códigos de un mismo local. Tratadas: estaciones con una entrada de otra marca a 2 km o menos mientras operaban; excluidas: entrada competidora más cercana entre 2 y 3 km; control amplio y estricto: nunca una entrada competidora a 3 y 5 km o menos. Muestra de estimación: estaciones presentes en 2012, cohortes desde 2014, cortada en la segunda entrada, gasolina 93. Tabla descriptiva, sin estimación.
