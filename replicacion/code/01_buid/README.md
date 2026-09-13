# 01_buid: construcción de la base

Desde los precios de Bencina en Línea y la serie MEPCO hasta los paneles que usa el análisis.
Rutas relativas a `replicacion/` (abrir `code.Rproj`). Parámetros compartidos en `code/00_utilidades.R`.

## Orden de ejecución

| Script | Insumos | Productos |
|---|---|---|
| `01_limpiar_precios.R` | `data/input/bencina_en_linea/2012.csv` … `2026.csv` | `data/procesado/precios.csv`, `precios_regimenes.rds` |
| `02_diagnostico_empalme.R` | `precios_regimenes.rds` | solo consola: cifras de §1–§3 |
| `03_limpiar_mepco.R` | `data/input/mepco.xlsx` | `data/procesado/costo_mayorista_mepco.csv` |
| `04_armado_base.R` | `precios.csv` | `data/procesado/base.csv` |
| `05_armado_panel.R` | `base.csv`, `costo_mayorista_mepco.csv` | `identidad_estaciones.csv` y, por ventana, `panel_semanal.csv.gz`, `panel_mensual.csv`, `entradas.csv` |

`02` es opcional para el resto del pipeline.

## Paquetes

R 4.x con `data.table` ≥ 1.15, `dplyr` ≥ 1.2.0 (`replace_values()`), `readr`, `purrr`, `tidyr`, `stringr`, `readxl`, `hms`, `here`.
El análisis agrega `fixest` ≥ 0.11, `ggplot2`, `sf` y `HonestDiD` ≥ 0.2.8.
Versiones con que se generaron los resultados: data.table 1.18.4, dplyr 1.2.1, fixest 0.14.1, ggplot2 4.0.3, sf 1.1.1, HonestDiD 0.2.8, here 1.0.2.

## Decisiones

### 1. Dos regímenes de reporte (`01`)

La CNE cambió el formato de los archivos en 2023: el antiguo (2012–2022) usa `;` y coma decimal en las coordenadas. El actual (2023–2026) usa `,`, separa modalidades de atención y trae hora.
Se usa el antiguo antes del 2023-01-01 y el actual desde esa fecha.

El archivo 2023 trae también fechas anteriores al corte (`02`, §3):

| | |
|---|---|
| lecturas estación-fecha-combustible en ambos | 3.073.006 |
| % de las lecturas del antiguo presentes en el actual | 89,0 % |
| % con precio idéntico cuando están en ambos | 99,7 % |
| lecturas solo en el antiguo | 379.956 (339 estaciones) |
| lecturas solo en el actual | 205 (49 estaciones) |

Las 339 estaciones que no figuran en el actual dejaron de reportar antes (última aparición mediana 2021-10-28, frente a 2022-12-29 de las que sí figuran). El régimen actual borra su historia, por lo que hay que empalmar ambos.

### 2. Solo precio asistido (`01`)

Desde 2023 cada combustible viene en modalidad asistida (`93`) y autoservicio (`A93`). Se conserva la asistida, que es la única comparable con el régimen antiguo.
Se descarta el 11,7 % de las lecturas (`02`, §1). Estaciones sin ningún precio asistido: con historia previa co631001, pb1360503, pe410101, sh1312304, sh1320104; sin historia pp1310101, sh1230102.

### 3. Ubicación (`01`)

Hay estaciones que "se teletransportan" al cambiar de régimen. Se asume que las coordenadas del régimen actual son más exactas.
Cada estación toma su coordenada, comuna y región más frecuentes del actual, y esa ubicación reemplaza a la del antiguo: 110 estaciones cambian (`02`, §2).

### 4. Limpieza de precios (`01`)

- Fuera: registros de prueba (`prueba`, `CNE 01`) y lecturas sin coordenadas válidas (en Chile latitud y longitud son negativas).
- Sentinelas `0` y `99999999` pasan a NA.
- Un precio por encima del doble de la mediana de su combustible-año se divide por 10 o por 100 si así queda a menos de 35 % de la mediana (decimales corridos). Lo que siga fuera de 200–5.000 $/L se descarta.
- Recodificación de distribuidor, comuna y región a un vocabulario único entre regímenes.

Filas tras cada paso (log de `01`):

| Paso | Antiguo | Actual |
|---|---|---|
| leídas | 4.159.260 | 4.800.767 |
| sin prueba, coordenadas válidas | 4.158.590 | 4.800.767 |
| combustibles del análisis con precio válido | 3.675.266 | 4.095.401 |
| empalme / sin duplicados | 4.495.195 / 4.473.981 | |

Resultado: 2.084 códigos de estación, 2012-01-01 a 2026-07-12.

### 5. Base diaria (`04`)

Una fila por estación-fecha-combustible: la última lectura del día. `is_franchise`: la estación tiene bandera distinta de "sin bandera" y esa bandera tiene más de una estación.

### 6. MEPCO (`03`)

Precio mayorista semanal con y sin MEPCO para 93, 97 y diésel; la 95 no tiene referencia, por lo que no tiene margen. La serie debe ser de jueves consecutivos.
La grilla semanal del panel parte el jueves 2012-01-05 (`WEEK0`). El MEPCO arranca el 2014-08-07, exactamente 135 semanas después, y cae sobre la grilla.

### 7. Identidad de estaciones (`05`)

Un mismo local puede cambiar de código. Si no se corrige, la recodificación aparece como una entrada y "trata" a sus vecinas.

- **Sucesión**: un código hereda el `station_key` del código ya apagado más cercano que empezó antes, si está a menos de `LINK_M` = 50 m. 25 casos.
- **Duplicación**: un código con letra final (`co1234a`) cuyo código base existe a ≤ 50 m. 37 casos.

El detalle queda en `identidad_estaciones.csv`: 2.084 códigos → 2.022 estaciones.

### 8. Paneles (`05`)

- **Semanal**: último precio de la semana. Una semana sin reporte arrastra el último precio hasta `MAXGAP_W` = 13 semanas (~3 meses; la ley obliga a informar cada cambio). Un vacío mayor saca a la estación en vez de propagar su precio. Margen = precio − costo MEPCO.
- **Mensual**: la última semana de cada mes, con `p93 p95 p97 pdi` y `m93 m97 mdi`.
- La última semana y el último mes incompletos del registro se recortan.
- Atributos de estación (marca, comuna, región): valor modal entre sus códigos. La marca queda constante por estación.

### 9. Ventanas (`PANELES`)

| Ventana | Base | Cohortes focales | Fin |
|---|---|---|---|
| `2012_2026` (principal, en `data/procesado/`) | presentes en 2012 | desde 2014 (`FOCAL_FROM`) | fin del registro |
| `2014_2019` | presentes en 2014 | desde 2015 | 2019-12-31 |
| `2021_2026` | presentes en 2021 | desde 2022 | 2026-02-28 |

- **Base**: la primera aparición de la estación en toda la historia es a más tardar el año de inicio. Las que aparecen después son entradas.
- **Entradas de 2013**: 79, frente a 20–50 en un año normal, y 43 son de bandera blanca. Son incorporación tardía al reporte, no aperturas: contaminan y cortan ventanas, pero nunca definen cohorte. En las ventanas cortas la cohorte focal parte al año siguiente de la base.
- **Fin de la ventana reciente**: febrero de 2026. El 26 de marzo el MEPCO salta de 1.083 a 1.455 $/L en la 93 y desde ahí el modo de competir es otro.

### 10. Asignación de tratamiento (`05`, `asignar_tratamiento`)

Para cada estación y cada entrada de la ventana:

- **No cuentan**: la propia entrada de la estación, entradas anteriores a su primer mes o posteriores a su último, y (con `SOLO_COMPETIDORAS = TRUE`) entradas de su misma marca.
- **`g_entry`**: mes de la primera entrada competidora a ≤ `RTREAT` = 2 km. **`g2_entry`**: mes de la segunda; la muestra se corta ahí.
- **`role_entry`**: se define por la distancia mínima a la que alguna vez hubo una entrada:
  - `treated`: hubo entrada a ≤ 2 km
  - `buffer`: la entrada más cercana quedó entre 2 y 3 km
  - `ctrl_a`: la más cercana está entre 3 y 5 km
  - `ctrl_b`: nunca hubo una entrada a menos de 5 km

  Control amplio = `ctrl_a` + `ctrl_b`; control estricto = `ctrl_b`.
- **Buffer**: no es tratado ni control. La figura de atenuación muestra efecto significativo en los cuatro combustibles en la banda de 2 a 3 km, de modo que usarlas como comparación metería estaciones parcialmente tratadas en el contrafactual.
- **Anillos**: `g5_entry`, `g2_5_entry`, `dist_entry5`, `ring_entry` y `role5_entry` repiten la asignación con radio 5 km. Toman la primera entrada y, en el mismo mes, la más cercana. Los usa `03_estimacion/04_atenuacion_distancia.R`.

**Solo las competidoras tratan.** Una entrada con la misma marca que la incumbente es expansión de la red de la propia cadena: bajo consignación el precio de ambas lo fija la misma mayorista y no hay contra quién competir. El marco teórico lo dice también: la n de la Proposición 1 cuenta competidoras, no estaciones.
La marca de la entrante se compara con la de cada incumbente, así que un mismo evento trata a unas y no a otras. El costo es de comparabilidad: Fischer, Martin y Schmidt-Dengler tratan cualquier entrada dentro del radio. `SOLO_COMPETIDORAS = FALSE` recupera su criterio. Con `TRUE` los efectos crecen entre 10 y 42 % según el resultado, porque el brazo tratado deja de incluir eventos de efecto cero.
