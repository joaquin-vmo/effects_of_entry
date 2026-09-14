# 03_estimacion: efecto de la entrada

Estudios de eventos y extensiones. Requieren haber corrido `01_buid/`. Los scripts son independientes entre sí.

| Script | Ventanas | Productos |
|---|---|---|
| `01_event_study.R` | las tres | `output/tablas/es_<ventana>_<comb>.tex`, `output/graficos/es_<ventana>.pdf` |
| `02_event_study_controles.R` | 2012_2026 | `output/tablas/es_controles_<comb>.tex` |
| `03_event_study_sunab.R` | 2012_2026 | `output/graficos/sunab_2012_2026.pdf` |
| `04_atenuacion_distancia.R` | 2012_2026 | `output/tablas/atenuacion_distancia.tex`, `output/graficos/atenuacion_distancia.pdf` |
| `05_heterogeneidad_autopista.R` | 2012_2026 (+ red vial MOP) | `output/tablas/het_autopista.tex`, `output/graficos/het_autopista.pdf` |
| `06_mercado_min_media_max.R` | 2012_2026 (+ `entradas.csv`) | `output/tablas/mercado.tex`, `output/graficos/mercado.pdf` |
| `07_distribucion_acumulada.R` | 2012_2026 | `output/tablas/distribucion_acumulada.tex`, `output/graficos/distribucion_acumulada.pdf`, `distribucion_beta.pdf` |

`<comb>` ∈ {p93, p95, p97, pdi}. Las tablas se escriben como `tabular` sin nota; las notas para la tesis están abajo.

## Especificación común

- **Resultado**: log(precio) × 100, de modo que el coeficiente es un cambio porcentual. Panel mensual.
- **Muestra** (`muestra_estacion()` en `00_utilidades.R`):
  1. tratadas más el grupo de control; ninguna estación del buffer (2–3 km) entra en ningún brazo;
  2. se corta en la segunda entrada: el estimando es el de una primera entrada;
  3. fuera las tratadas de cohortes anteriores a la focal (2013 es incorporación tardía al reporte);
  4. solo estaciones de base en ambos brazos (regla de Fischer et al.): una entrante reciente está en su propio transitorio de apertura.
- **Tiempo-evento**: bins de 6 meses desde la entrada, extremos agrupados en ≤ −4 y ≥ 4, referencia −1. Las no tratadas van al bin de referencia.
- **Control**: estricto por defecto, es decir, estaciones que nunca tuvieron una entrada a menos de 5 km (ver `01_buid/README.md` §10).
- **Efectos fijos** (`FE_PRINCIPAL`): estación, mes, región × año y marca × año.
  El de marca × año no está en Fischer et al. y responde a un rasgo del caso chileno: bajo consignación el precio lo fija la mayorista. Con tres cadenas que concentran ~80 % de las estaciones, un movimiento nacional de cualquiera de ellas es un confusor que ni el efecto fijo de estación ni el de región × año absorben. La marca es constante a nivel de estación por construcción del panel, así que no es un mal control.
- **Errores estándar** agrupados por comuna.
- **Tratadas y controles** de cada tabla: estaciones que efectivamente usa el modelo, sin los singletons que descarta fixest.

## 01_event_study

Escalera de efectos fijos sobre la misma muestra y el mismo regresor: sin efectos fijos, + mes, + estación, + región × año, + marca × año. Sin efecto fijo de estación se controla por el indicador de tratada. El gráfico es la última columna.

**Nota de la tabla.** Errores estándar agrupados por comuna entre paréntesis; * p<0,10 ** p<0,05 *** p<0,01; bins de seis meses desde la entrada.
**Nota de la figura.** Bins de seis meses desde la entrada, extremos agrupados (≤ −4 y ≥ 4); referencia en −1; intervalos al 95 % con errores agrupados por comuna.

## 02_event_study_controles

Especificación principal con control amplio (> 3 km) y estricto (> 5 km).

**Nota.** Errores estándar agrupados por comuna entre paréntesis; * p<0,10 ** p<0,05 *** p<0,01; bins de seis meses desde la entrada; efectos fijos de estación, mes, región × año y marca × año.

## 03_event_study_sunab

TWFE con cohortes escalonadas mezcla comparaciones entre cohortes y puede ponderar negativamente si el efecto varía entre ellas. Sun y Abraham (2021) estima un efecto por cohorte × periodo relativo contra las nunca tratadas y los promedia con el peso de cada cohorte.

- **Qué cambia respecto de `01`**: solo el estimador. Muestra, efectos fijos y bins son los mismos.
- **Cohortes**: mensuales (mes de la entrada).
- **Truco con `sunab()`**: la función no acepta bins desde una variable, así que se le pasa como periodo `cohorte + rel`. Así periodo − cohorte es exactamente el bin. El efecto fijo de mes sigue en `FE_PRINCIPAL`.
- **Nunca tratadas**: llevan una cohorte fuera de rango (99999).
- **Verificación**: contra el estimador armado a mano (interacciones cohorte × bin ponderadas por participación), coeficientes y errores estándar idénticos.

**Nota.** Panel mensual; Sun y Abraham (2021) con cohortes mensuales (mes de la entrada) y nunca tratadas como referencia; bins de seis meses desde la entrada, extremos agrupados; referencia en −1; misma muestra y efectos fijos que la estimación principal; intervalos al 95 % con errores agrupados por comuna; control a más de 5 km.

## 04_atenuacion_distancia

Solo cambia la definición de tratamiento: cada estación con una entrada a ≤ 5 km se asigna al anillo de su primera entrada dentro de ese radio (0–1 … 4–5 km). Se estima un ATT estático por anillo.

- **Lectura**: un perfil que cae desde 0–1 km es consistente con competencia local; uno plano apuntaría a composición y no a la entrada.
- **Ventana**: las tratadas aportan solo 6 meses a cada lado de la entrada (−6 a +5), así que el ATT es el efecto de los primeros seis meses. Sin ventana, el periodo post mezcla horizontes muy distintos entre cohortes. Los controles aportan todos sus meses.

**Nota.** ATT estático por anillo de distancia de la incumbente a su primera entrada a ≤ 5 km, en % del precio; ventana de 6 meses antes y después de la entrada para las tratadas; errores estándar agrupados por comuna entre paréntesis; * p<0,10 ** p<0,05 *** p<0,01; control: estaciones sin entradas a menos de 5 km.

## 05_heterogeneidad_autopista

**Por qué esperar una diferencia.** El diseño define el mercado como el disco de 2 km alrededor de la incumbente. Para una estación de autopista buena parte de la demanda va de paso: si su mercado relevante es el corredor y no el barrio, una entrada a 2 km no cambia su entorno competitivo. No es un efecto causal de la autopista, porque esas estaciones pueden diferir en marca, mercado o margen previo.

- **Autopista**: estación a ≤ 100 m del eje de la red primaria del MOP (clase 1, todo el país). Incluye autopistas urbanas y carreteras interurbanas (Ruta 5, rutas internacionales, Carretera Austral), con cobertura dispareja entre regiones. De las tratadas a ≤ 100 m, ~1/3 está en la RM.
- **Una sola regresión**: el tiempo-evento se interactúa con tratada-lejos y tratada-cerca, de modo que el conjunto completo de controles ancla la trayectoria calendario. La diferencia de ATT (`post:cerca`) tiene su propio error estándar.
- **Entrante fuera de la autopista**: se descartan las tratadas cuya entrante está a ≤ 100 m de la red, para que en ambos grupos el evento sea la entrada de una estación de barrio. Salen 77 de 506 tratadas.
  Las entradas se reconstruyen con la regla de `05_armado_panel.R`, y un `stopifnot` verifica que sean las mismas del panel.

**Nota de la tabla.** ATT en % del precio; especificación principal; autopista = estación a 100 m o menos del eje de la red primaria del MOP (clase 1); se excluyen las tratadas cuya entrante está a 100 m o menos de esa red; control: estaciones sin entradas a menos de 5 km; errores estándar agrupados por comuna entre paréntesis; * p<0,10 ** p<0,05 *** p<0,01.
**Nota de la figura.** Estudio de eventos de la especificación principal con el tiempo-evento interactuado por grupo en una sola regresión; mismas definiciones; bins de seis meses, extremos agrupados; referencia en −1; intervalos al 95 %.

## 06_mercado_min_media_max

Efecto de la entrada sobre el percentil 90, la mediana y el percentil 10 de los precios del mercado local. Responde **dónde** del mercado se produce la baja: si cae tanto la parte cara como la barata, la ganancia se reparte; si cae sobre todo la barata, se concentra en quienes comparan precios.

- **Mercado**: cada estación focal de `muestra_estacion()` más todas las estaciones con precio a ≤ 2 km en el mes. La focal debe tener precio. Resultado: log del cuantil × 100.
- **Cuantiles en vez de máximo, media y mínimo** (la figura de Fischer et al.): el máximo es el precio de una sola estación. En ~3 % de los mercado-mes la más cara está más de 5 % sobre la mediana, en episodios que duran meses. Eso triplicaba el error estándar y generaba bins previos negativos (−0,24 en la 93); subir el mínimo de competidoras lo empeoraba. Cuantil tipo 7: en un mercado de dos estaciones el p90 queda a 90 % del camino del más bajo al más alto y la mediana coincide con la media.
- **Mínimo de competidoras** (`MIN_COMP` = 1): el mercado debe tener en promedio al menos una competidora con precio **antes** de la entrada; en los controles, en todos sus meses. Medirlo con el conteo contemporáneo seleccionaría sobre la propia entrada. Con 2, los controles caen de 191 a 91 (son mercados ralos).
- **Entrante incluida**: toda estación del radio, la entrante incluida desde que abre. Es el mercado que enfrenta el consumidor, pero ahí la parte barata baja y la cara sube por pura composición. La versión sin entrante se dejó de estimar (`mercado_local()` aún la calcula, y `04_robustez/02_honest_did.R` la usa).
- **Efectos fijos**: se mantiene marca × año de la focal.
- **Estimador**: Sun y Abraham (2021), con cohortes mensuales y el mismo truco de bins que `03_event_study_sunab.R`. La tabla es el ATT agregado (`sunab(..., att = TRUE)`), la figura el estudio de eventos. Una tabla con los cuatro combustibles y una figura de cuatro paneles. Frente a TWFE los cuantiles bajos caen más en 93, 95 y diésel, y en la 97 el ATT pasa a cero: su efecto dura dos semestres y los bins largos, que pesan más en el ATT, están en cero. Tarda ~10 minutos. `04_robustez/02_honest_did.R` sigue usando TWFE para estas series.

**Nota de la tabla.** ATT de Sun y Abraham (2021) en % del cuantil del mercado (log × 100 del percentil 90, la mediana y el percentil 10 de los precios); mercado = estación focal y estaciones con precio a 2 km o menos, entrante incluida; mercados con al menos 1 competidora en promedio antes de la entrada; especificación principal; control: estaciones sin entradas a menos de 5 km; errores estándar agrupados por comuna entre paréntesis; * p<0,10 ** p<0,05 *** p<0,01.
**Nota de la figura.** Estudio de eventos de Sun y Abraham (2021) sobre el log del percentil 90, la mediana y el percentil 10 de los precios del mercado (radio de 2 km); bins de seis meses, extremos agrupados; referencia en −1; intervalos al 95 %; resto como la nota de la tabla.

## 07_distribucion_acumulada

Regresión de distribución de Chernozhukov, Fernández-Val y Melly (2013), como en la sección 4.2 de Fischer et al.

- **Estimación**: para cada umbral c (41 umbrales, cuantiles 2 a 98 del resultado) se estima 1[y ≤ c] = β(c)·post + efectos fijos. β(c) > 0 significa más masa bajo c, es decir, precios más bajos. La acumulada observada de las tratadas post-entrada menos β(c) es la contrafactual.
- **Dominancia de primer orden**: si β(c) ≥ 0 en todo c, la distribución con entrada domina a la contrafactual. El estadístico max_c β(c) se compara con el valor crítico unilateral de Gail y Green (1976), √(−log α / n), con α = 0,01.
- **Resultado desviado de la media nacional del mes**: en nivel, el cuantil 10 sería "un mes barato" y no "una estación barata", porque el mes explica ~99 % de la varianza del precio. La media se calcula sobre todo el panel, no sobre la muestra.

**Nota de la tabla.** β(c) = efecto de la entrada sobre 1[precio ≤ c], con c el cuantil indicado del precio menos su media nacional del mes; regresión de distribución de Chernozhukov, Fernández-Val y Melly (2013); β(c) > 0 = más masa bajo c (precios más bajos); dominancia estocástica de primer orden si max β(c) supera el valor crítico de Gail y Green (1976) y min β(c) no es negativo; especificación principal; control: estaciones sin entradas a menos de 5 km; errores estándar agrupados por comuna entre paréntesis; * p<0,10 ** p<0,05 *** p<0,01.
**Nota de la figura.** Acumulada del precio menos su media nacional del mes para las tratadas después de la entrada (observada) y la que habrían tenido sin ella (observada menos β(c)); regresión de distribución sobre 41 umbrales; especificación principal; control a más de 5 km.
**Nota de la figura (β).** Desplazamiento de la distribución de precios por efecto de la entrada: β(c) sobre los 41 umbrales, en el eje x el cuantil del precio menos su media nacional del mes al que corresponde cada umbral; β(c) > 0 = más masa bajo c (precios más bajos); bandas al 95 % con errores agrupados por comuna; especificación principal; control a más de 5 km.
