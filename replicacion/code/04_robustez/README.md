# 04_robustez: chequeos de robustez

Requieren haber corrido `01_buid/`; `02` repite las estimaciones de `03_estimacion/01` y `06`. Especificación común: la de `03_estimacion/README.md`.

| Script | Ventanas | Productos |
|---|---|---|
| `01_conley.R` | las tres | `output/graficos/conley_<ventana>.pdf` |
| `02_honest_did.R` | 2012_2026 (+ `entradas.csv`) | `output/tablas/honest_principal.tex`, `honest_mercado.tex` |
| `03_fischer_semanal.R` | 2012_2026 (panel semanal + `entradas.csv`) | `output/graficos/fischer_semanal.pdf` |
| `04_validacion_margen.R` | 2012_2026 (ago 2018 a jul 2019) | `output/tablas/validacion_margen.tex` |
| `05_persistencia.R` | 2012_2026 | `output/graficos/persistencia.pdf` |
| `06_placebo_adelanto.R` | 2012_2026 | `output/graficos/placebo_adelanto.pdf` |
| `07_placebo_controles.R` | 2012_2026 | `output/tablas/placebo_controles.tex` |
| `08_ventanas_cortas.R` | 2014_2019, 2021_2026 | `output/graficos/ventana_<ventana>.pdf`, `output/tablas/ventana_<ventana>.tex` |

## 01_conley

La comuna es una frontera administrativa: un disco de 2 km la cruza, y dos estaciones a 500 m en comunas distintas quedan como independientes. Conley (1999) estima la matriz de covarianza cuando la dependencia entre observaciones decae con su distancia: promedia las covarianzas entre pares con pesos que se anulan desde un corte, el análogo espacial de Newey y West (1987).

- **Cortes**: 2 km (radio de tratamiento) y 5 km (radio del control estricto).
- **Kernel uniforme, no Bartlett**: `conley()` de fixest da peso 1 a todo par a menos del corte. Conley propone pesos Bartlett (ec. 3.14), que garantizan una matriz semidefinida positiva; con pesos constantes eso no está garantizado (nota 11). fixest la corrige y avisa si no lo es; en estas estimaciones no avisó.
- **Sin dimensión temporal**: `conley()` de fixest correlaciona pares a menos del corte en cualquier mes, así que también cubre la autocorrelación de cada estación (con corte ~0 reproduce el cluster por estación).
- **Lectura**: coeficientes idénticos entre las tres series; solo cambian los intervalos. Figura en vez de tabla: con los intervalos lado a lado se ve de una vez que coinciden.

**Nota.** Especificación principal (log del precio × 100); intervalos al 95 % con errores agrupados por comuna y de Conley (1999) con kernel uniforme (implementación de fixest) y corte de 2 y 5 km; bins de seis meses desde la entrada, extremos agrupados, referencia en −1; efectos fijos de estación, mes, región × año y marca × año; control: estaciones sin entradas a menos de 5 km.

## 02_honest_did

No rechazar el test de leads no prueba tendencias paralelas: esos tests tienen poca potencia contra una deriva suave (Roth 2022). Rambachan y Roth (2023) acotan cuánto puede diferir la violación posterior a la entrada de la previa, que sí se observa, y entregan un intervalo robusto.

- **Restricción de magnitudes relativas**, Δ^RM(M̄): el salto de la violación entre periodos posteriores no supera M̄ veces el mayor salto observado entre periodos previos. M̄ = 1 significa que la deriva después de la entrada no es mayor que la de antes. Es adimensional, así que se puede comparar entre combustibles y series.
- **M̄ de quiebre**: el mayor M̄ de la grilla (0,25 a 3, paso 0,25) con el que el intervalo robusto al 95 % todavía excluye el cero.
  - "< 0,25": ni la relajación mínima lo sostiene.
  - "> 3": resiste una violación posterior del triple de la previa.

  Como el intervalo se ensancha con M̄, el quiebre se busca por bisección. La búsqueda del intervalo cubre ± 60 errores estándar; el default de HonestDiD, 20, truncaba intervalos a M̄ alto.
- **Horizontes**: impacto = bin 0 (meses 0 a 5) y bin 1 (meses 6 a 11). La violación se acumula bin a bin, así que el horizonte más lejano siempre resiste menos.
- **Verificación**: con control amplio y la grilla del proyecto anterior, los quiebres del bin de impacto coinciden (1, 2, 0,5 y 2).

**Nota (principal).** Sensibilidad a violaciones de tendencias paralelas de Rambachan y Roth (2023), restricción de magnitudes relativas, sobre el estudio de eventos principal (log del precio × 100); estimador e IC 95 % convencional del bin indicado; M̄ de quiebre = mayor M̄ (grilla 0,25 a 3) con el que el intervalo robusto al 95 % excluye el cero, es decir, cuántas veces el mayor salto previo puede ser el salto posterior de la violación; control a más de 5 km; errores agrupados por comuna.
**Nota (mercado).** M̄ de quiebre de Rambachan y Roth (2023), restricción de magnitudes relativas, sobre el estudio de eventos del log del percentil 90, la mediana y el percentil 10 de los precios del mercado; con entrante incluye a la estación que abre, sin entrante la excluye; mayor M̄ (grilla 0,25 a 3) con el que el intervalo robusto al 95 % excluye el cero; mercados de 2 km con al menos 1 competidora antes de la entrada; control a más de 5 km; errores agrupados por comuna.

## 03_fischer_semanal

Replica el diseño temporal de Fischer, Martin y Schmidt-Dengler (2025) para ver si la frecuencia mensual cambia algo.

- **Diario no**: `base.csv` tiene ~52 lecturas por estación-año (mediana entre lecturas: 7 días). Un panel diario repetiría el semanal.
- **Semana de la entrada**: primera semana con precio de la entrante competidora a ≤ 2 km que abrió en el mes `g_entry`. Un `stopifnot` verifica que todas las tratadas tengan semana.
- **Bins de 26 semanas** (−4 = ≤ −79 semanas, 4 = ≥ 104), efecto fijo de semana en vez de mes, cohortes trimestrales en Sun y Abraham, como en `sunabraham_baseline.R` de su paquete de replicación.
- **Cohortes sin identificar**: si un coeficiente cohorte × bin tiene error estándar > 100 veces la mediana, esa cohorte sale de Sun y Abraham. En la 95 es la cohorte 2024-T2 (dos estaciones); con ella la varianza agregada no es definida positiva y los errores de los bins 1 a 4 llegan a cientos.
- **Resultado**: coeficientes a ≤ 0,04 puntos de los mensuales; el bin de impacto es algo menor en las gasolinas. Tarda ~2,5 minutos.

**Nota.** Estudio de eventos principal en panel mensual (TWFE) y en panel semanal con el diseño de Fischer et al. (2025): semana de la entrada, bins de 26 semanas, efecto fijo de semana y cohortes trimestrales en Sun y Abraham; log del precio × 100; extremos agrupados, referencia en −1; efectos fijos de estación, región × año y marca × año; intervalos al 95 % con errores agrupados por comuna; control a más de 5 km.

## 04_validacion_margen

Compara el margen del panel (precio − costo mayorista con MEPCO) con la Tabla N.º 4 del informe de archivo Rol 2615-20 de la FNE: margen contable de Copec, Petrobras y Shell por anillos de 1 km en torno a la ex estación Sesa (Av. Tobalaba 1895), agosto 2018 a julio 2019.

- **Agregado**: margen ponderado sobre precio ponderado de 93, 97 y diésel con las participaciones nacionales en volumen (16,0; 3,3; 52,5), renormalizado a los combustibles que vende cada estación-mes. La FNE usa litros vendidos y los cuatro combustibles.
- **Resultado**: el panel da 62 a 83 $/L en las tres marcas; la FNE, 40–60 en Copec y 20–40 en Shell y Petrobras. El precio es casi igual entre marcas, así que la brecha contable viene del costo (flete, almacenamiento, adquisición), que el MEPCO no recoge.
- Conteo de estaciones a ≤ 3 km: 15 Copec, 12 Shell, 3 Petrobras (la FNE cuenta 11 Shell).

**Nota.** Margen porcentual y en $/L; este trabajo: precio menos costo mayorista con MEPCO, promedio simple de estación-mes de 93, 97 y diésel ponderados por participación nacional en volumen; FNE: ingresos menos costo de adquisición (ENAP, impuesto específico, almacenamiento y transporte) sobre ingresos minoristas de los cuatro combustibles, en rangos.

## 05_persistencia

Estudio de eventos sobre el número de estaciones con registro a ≤ 2 km, con y sin la entrante (figura 2 de Fischer et al.). Se corre con y sin el corte en la segunda entrada: el corte censura el conteo a horizontes largos, porque solo sobreviven mercados sin segunda entrada.

## 06_placebo_adelanto

Estudio de eventos con la entrada adelantada un período (6 meses), a resolución mensual en los cuatro bins a cada lado de la entrada registrada (−24 a 24 meses, extremos agrupados) y muestra completa. Referencia en el mes previo a la entrada adelantada (−7 respecto de la registrada); la figura marca ambas entradas. Con bins de seis meses el adelanto movería la figura exactamente un bin.

## 07_placebo_controles

Placebo con fechas de entrada falsas en el grupo de control (500 réplicas, semilla 20260902): se sortea la proporción real de tratadas, con cohortes y desfases a la segunda entrada de sus distribuciones reales, se corta en la segunda entrada falsa y se estima el ATT estático principal. Sobre todo el grupo y sobre su mitad más densa (competidoras a ≤ 2 km). Reporta centrado, desviación de la distribución frente al error agrupado medio, tasa de rechazo al 5 % y valor p por aleatorización del ATT real. Unidades: log del precio × 100 (el proyecto anterior usaba $/L). Tarda ~15 minutos.

## 08_ventanas_cortas

Estudio de eventos principal en las ventanas antes (2014_2019, cohortes 2015–2019) y después (2021_2026, cohortes 2022 a febrero de 2026) de la pandemia, con TWFE y Sun y Abraham (2021) sobre la misma muestra, efectos fijos y bins (mismo truco `per = cohorte + rel` que `03_estimacion/03_event_study_sunab.R`). Una figura y una tabla por ventana; la tabla tiene TWFE y SA por combustible.

- **Antes de la pandemia**: efectos mayores que en la ventana completa (0,4 a 0,85 %), con bins previos positivos que en Sun y Abraham son significativos en 95 y 97.
- **Después de la pandemia**: efectos de 0,1 a 0,25 % solo en los tres primeros semestres en 93 y 95, uno en diésel, ninguno negativo en 97; ~195 tratadas.
- Los intervalos de Sun y Abraham son más angostos que los de TWFE en ambas ventanas, como en la principal.

**Nota de la figura.** ⟨ventana⟩; log del precio × 100; TWFE y Sun y Abraham (2021) con cohortes mensuales y nunca tratadas como referencia; bins de seis meses desde la entrada, extremos agrupados, referencia en −1; efectos fijos de estación, mes, región × año y marca × año; intervalos al 95 % con errores agrupados por comuna; control a más de 5 km.
