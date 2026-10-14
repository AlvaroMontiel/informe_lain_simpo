# Informe LAIN

Abrir `informe_lain_simpo.Rproj` y renderizar `main.Rmd` con Knit en RStudio.
Desde R: `rmarkdown::render("main.Rmd")`.

- `main.Rmd`: informe completo, con resultados visibles y otros apartados pendientes.
- `R/funciones.R`: grupos etarios, normalización territorial, curvas semanales y tablas anuales.
- `R/preparar_datos.R`: carga, tipos, selección de columnas y filtro de intención suicida.
- `R/analisis_descriptivo.R`: análisis descriptivo para ejecución desde el script principal.
- `R/modelos_poisson.R`: denominadores, comprobaciones, Poisson, quasi-Poisson e interacciones.
- `exploratory_analisis.R`: ejecuta el flujo modular desde la raíz del proyecto.
- `exploratory_analisis_original.R`: copia del script anterior a la reorganización.

El R Markdown contiene los bloques descriptivos para poder ocultar cada resultado
con `include=FALSE`, o evitar su ejecución con `eval=FALSE`. Si se cambia un análisis,
actualizar también su bloque en `R/analisis_descriptivo.R` cuando se quiera mantener
el flujo de consola equivalente. La preparación, funciones y modelos se comparten.
No usar `eval=FALSE` en bloques que preparan objetos utilizados más adelante.

Los mensajes y advertencias están visibles para revisar los modelos. Todos los
bloques se ejecutaron al generar `main.html` con los archivos de entrada actuales.
Los resultados no representan automáticamente personas únicas; se conservan las
filas del original. No se aplicó un filtro adicional de duplicados.

Pendiente: confirmar si el modelo debe usar `comuna` o `comuna_paciente`.
La opción está explícita en `R/modelos_poisson.R`. Los modelos con interacciones
pueden tener combinaciones sin eventos; revisar estabilidad antes de interpretarlos.
Se conservan Student, Welch y Wilcoxon como exploración, sin seleccionar según p.

Las curvas mensuales ahora parten el día 1 e incluyen 2024–2026 completos en el eje.
Las semanales incluyen el último intervalo de cada año y se rotulan como intervalos
de siete días desde el 1 de enero, no como semanas epidemiológicas oficiales.
Se corrigió `density(..., na.rm=TRUE)` para que omita faltantes antes del cálculo.
Las tablas mantienen denominadores y períodos del original. La sección temporal
incluye 2026; revisar su fecha de corte antes de compararla con años completos.

Las propuestas boxplot y violín están visibles para compararlas. Se ocultan con
`mostrar_propuestas: false`.
Paquetes: tidyverse, readxl, janitor, skimr, rstatix, gtsummary, lubridate,
stringi, knitr y rmarkdown. RStudio proporciona Pandoc para producir HTML.
