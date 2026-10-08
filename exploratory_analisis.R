# Paquetes
library(here)
library(readxl)
library(tidyverse)
library(janitor)
library(skimr)
library(naniar)
library(rstatix)

# Carga de datos
ruta <- here("data", "input", "set_datos_lain_para_analisis.xlsx")

datos <- readxl::read_excel(ruta)
View(datos)

### Estructura y revisión de los datos 

dim(datos)  # Filas y columnas
names(datos)  # Nombre de las columnas
head(datos)
summary(datos)  # Resumen estadístico


### Limpiar tipos

variables_date <- c(
  "Fecha Atencion Urgencia", 
  "Fecha Nacimiento Paciente", 
  "Fecha del evento"
  )

variables_categoricas <- c(
  "Clasificacion",
  "Subclasificacion", 
  "Region", 
  "Comuna", 
  "Establecimiento Salud", 
  "Sexo Paciente",
  "Region Paciente", 
  "Comuna Paciente", 
  "Se considera pueblo originario", 
  "Pueblo originario", 
  "Se considera afrodescendiente", 
  "Identidad de Genero",
  "Orientacion Sexual", "Nacionalidad Paciente",
  "Persona Bajo Cuidado",
  "Lesion fue Autoinfligida", 
  "Lesion fue Intencional", 
  "Tuvo intencion de Morir",
  "Tiene Antecedentes salud mental",
  "Tiene tratamiento salud mental", 
  "Paciente estudia actualmente", 
  "Region de estudios", 
  "Comuna de estudios",             
  "Paciente trabaja actualmente", 
  "Region de trabajo", 
  "Comuna de trabajo",
  "Tipo de Evento", 
  "Metodo de Lesion", 
  "Lugar del evento", "compare_key",
  "es_duplicado"
)

variables_texto <- c(
  "Pueblo originario", 
  "Antecedentes salud mental", 
  "Lugar tratamiento salud mental"
)

variables_enteros <- c("Semana Epidemiologica", "Edad Calculada")

datos <- datos %>% mutate(
  across(all_of(variables_categoricas), as.factor))

datos <- datos %>% mutate(
  across(all_of(variables_date), function(x) as.Date(x, format = "%Y-%m-%d"))
)

datos <- datos %>% mutate(across(all_of(variables_enteros), as.integer))

# estandarizar nombre de las columnas

names(datos) <- janitor::make_clean_names(names(datos))

# Seleccionar columnas 
datos <- datos %>% 
  select(x1,
         compare_key,
         fecha_atencion_urgencia, 
         fecha_nacimiento_paciente,
         edad_calculada,
         fecha_del_evento,
         semana_epidemiologica,
         clasificacion,
         subclasificacion,
         region,
         comuna,
         establecimiento_salud,
         sexo_paciente,
         region_paciente,
         comuna_paciente,
         se_considera_pueblo_originario,
         pueblo_originario,
         se_considera_afrodescendiente,
         identidad_de_genero,
         orientacion_sexual,
         nacionalidad_paciente,
         persona_bajo_cuidado,
         lesion_fue_autoinfligida,
         lesion_fue_intencional,
         tuvo_intencion_de_morir,
         tiene_antecedentes_salud_mental,
         tiene_tratamiento_salud_mental,
         lugar_tratamiento_salud_mental,
         antecedentes_salud_mental,
         paciente_estudia_actualmente,
         region_de_estudios,
         comuna_de_estudios,
         paciente_trabaja_actualmente,
         region_de_trabajo,
         comuna_de_trabajo,
         tipo_de_evento,
         metodo_de_lesion,
         lugar_del_evento,
         es_duplicado
  )
   
# grupos de edad
datos <- datos %>%
  mutate(
    edad_categoria = cut(
    edad_calculada,
    breaks = c(0, 5, 11, 19, 24, 59, Inf),
    labels = c("Infancia 0-5", "Niñez 6-11", "Adolescencia 12-19",
               "Juventud 20-24", "Adultez 25-59", "Ancianidad 60 y más"),
    include.lowest = TRUE,  # En el primer grupó incluye el primer valor 
    right = TRUE  # TRUE por defecto, abierto por la derecha
   )
  ) %>% filter(
    subclasificacion == "Con intención suicida"
  )
  

# Visión general del conjunto de datos
skimr::skim(datos)

##########################
### Análisis exploratorio

## Temporal (todos los eventos, incluye duplicados)
## Fecha del evento, semana epidemiologica
## Número de eventos por mes; distribución semanal 

## TODO hacer las curvas epidemicas para la semana epidemiológica
## TODO eventos menusales
## TODO Curvas por establecimiento y comuna, 


## Demográfico (Personas únicas, evitar distorcionar la distribución)
#### Edad ####
### Estadísticos por edad
datos %>% 
  filter(year(fecha_del_evento) %in% c(2024, 2025)) %>%
  group_by(anio = year(fecha_del_evento)) %>%
  summarise(
    min = min(edad_calculada, na.rm = TRUE), 
    max = max(edad_calculada, na.rm = TRUE),
    media = mean(edad_calculada, na.rm = TRUE),
    mediana = median(edad_calculada, na.rm = TRUE),
    DE = sd(edad_calculada, na.rm = TRUE),
    Q1 = quantile(edad_calculada, prob = c(0.25), na.rm = TRUE),
    Q3 = quantile(edad_calculada, prob = c(0.75), na.rm = TRUE),
    .groups = "drop"
  )  # TODO separar por personas únicas

### Histograma
datos %>% 
  filter(year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = factor(year(fecha_del_evento))) %>%
  ggplot(aes(x = edad_calculada)) +
  geom_histogram(
    bins = 15,
    fill = "steelblue",
    color = "white") +
  facet_wrap(~ anio, ncol = 2, scales = "free_y") +
  labs(
    title = "Distribución de edad por año, 2024-2025",
    x = "Edad",
    y = "Frecuencia"
  )+
  theme_minimal()

### Test de normalidad
datos %>%
  filter(year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = factor(year(fecha_del_evento))) %>%
  group_by(anio) %>%
  shapiro_test(edad_calculada)

### QQ-plot: el más informativo
qqnorm(datos$edad_calculada)
qqline(datos$edad_calculada, col = "red")

### Densidad de distribución 
datos_2024 <- 
  datos %>% filter(
    year(fecha_del_evento) %in% c(2024))

datos_2025 <- 
  datos %>% filter(
    year(fecha_del_evento) %in% c(2025))

plot(density(datos_2024$edad_calculada), na.rm = TRUE)

plot(density(datos_2025$edad_calculada), na.rm = TRUE)


### histograma con la curva de distribucion

datos_filt <- datos %>%
  filter(year(fecha_del_evento) %in% c(2024, 2025),
         !is.na(edad_calculada)) %>%
  mutate(anio = factor(year(fecha_del_evento)))

### Parámetros por año + grilla para dibujar cada curva
curvas <- datos_filt %>%
  group_by(anio) %>%
  summarise(
    media = mean(edad_calculada),
    sd    = sd(edad_calculada),
    min_x = min(edad_calculada),
    max_x = max(edad_calculada),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(x = list(seq(min_x, max_x, length.out = 200))) %>%
  unnest(x) %>%
  mutate(y = dnorm(x, mean = media, sd = sd))

datos_filt %>%
  ggplot(aes(x = edad_calculada)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 15,
    fill = "steelblue",
    color = "white"
  ) +
  geom_line(
    data = curvas,
    aes(x = x, y = y),
    color = "red",
    linewidth = 1
  ) +
  facet_wrap(~ anio, ncol = 2, scales = "free_y") +
  labs(
    title = "Distribución de edad por año (2024-2025)",
    subtitle = "Curva roja = normal teórica con media y sd de cada año",
    x = "Edad",
    y = "Densidad"
  ) +
  theme_minimal()

# Edad Agrupada
datos %>% filter(year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  group_by(anio) %>%
  count(edad_categoria) %>%
  mutate(porcentaje = (n/sum(n))*100)

### Grafico de barras apiladas
datos %>%
  mutate(anio = year(fecha_del_evento)) %>%
  filter(anio %in% c(2024, 2025)) %>% 
  count(anio, edad_categoria) %>%
  ggplot() + 
  geom_col(
    mapping = aes(
      x = factor(anio),
      fill = edad_categoria,
      y = n
    ))

#### Edad x sexo ####
datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025),
    sexo_paciente %in% c("Hombre", "Mujer")) %>%
  group_by(anio = year(fecha_del_evento), sexo_paciente) %>%
  summarise(
    min = min(edad_calculada, na.rm = TRUE), 
    max = max(edad_calculada, na.rm = TRUE),
    media = mean(edad_calculada, na.rm = TRUE),
    mediana = median(edad_calculada, na.rm = TRUE),
    DE = sd(edad_calculada, na.rm = TRUE),
    Q1 = quantile(edad_calculada, prob = c(0.25), na.rm = TRUE),
    Q3 = quantile(edad_calculada, prob = c(0.75), na.rm = TRUE),
    .groups = "drop"
  )  # TODO separar por personas únicas

### tabla edad por sexo
datos %>% filter(
  year(fecha_del_evento) %in% c(2024, 2025),
  sexo_paciente %in% c("Hombre", "Mujer")) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  group_by(anio) %>%
  count(sexo_paciente) %>%
  mutate(porcentaje = (n/sum(n))*100)

### Histograma edad x sexo
datos %>% 
  filter(year(fecha_del_evento) %in% c(2024, 2025) & sexo_paciente %in% c("Hombre", "Mujer")) %>%
  mutate(anio = factor(year(fecha_del_evento))) %>%
  ggplot(aes(x = edad_calculada)) +
  geom_histogram(
    bins = 15,
    fill = "steelblue",
    color = "white") +
  facet_wrap(anio ~ sexo_paciente, ncol = 2, scales = "free_y") +
  labs(
    title = "Distribución de edad por año, 2024-2025",
    x = "Edad",
    y = "Frecuencia"
  )+
  theme_minimal()

### Test de normalidad edad x sexo
resultado <- datos %>%
  filter(year(fecha_del_evento) %in% c(2024, 2025) & sexo_paciente %in% c("Hombre", "Mujer")) %>%
  mutate(anio = factor(year(fecha_del_evento))) %>%
  group_by(anio, sexo_paciente) %>%
  shapiro_test(edad_calculada)

resultado

### QQ-plot: el más informativo
datos %>% 
  filter(year(fecha_del_evento) %in% c(2024, 2025) & sexo_paciente %in% c("Hombre", "Mujer")) %>%
  mutate(anio = factor(year(fecha_del_evento))) %>%
  group_by(anio, sexo_paciente) %>%
  ggplot(
    aes(sample = edad_calculada, fill = sexo_paciente)) +
  stat_qq(size = 1.2, alpha = 0.4)+
  stat_qq_line(linewidth = 0.8)+
  facet_grid(anio ~ sexo_paciente, scales = "free")+ 
  labs(
    title = "QQ-plots de edad por año y sexo (2024-2025)",
    subtitle = "Puntos alineados con la línea = distribución aproximadamente normal",
    x = "Cuantiles teóricos (normal)",
    y = "Cuantiles observados (edad)",
    color = "Sexo"
  )+
  theme_minimal()+
  theme(legend.position = "bottom")
  
  
### Densidad de distribución 
datos %>%
  filter(
    year(fecha_del_evento) %in% c(2024, 2025),
    sexo_paciente %in% c("Hombre", "Mujer")
  ) %>%
  mutate(anio = factor(year(fecha_del_evento))) %>%
  ggplot(aes(x = edad_calculada, fill = anio, color = anio)) +
  geom_density(alpha = 0.4, na.rm = TRUE) +
  facet_wrap(~ sexo_paciente, ncol = 2) +
  labs(
    title = "Densidad de edad por sexo y año, 2024-2025",
    x = "Edad",
    y = "Densidad",
    fill = "Año",
    color = "Año"
  ) +
  theme_minimal()

### Histograma con la curva normal teorica con md y sd para cada año
datos_filt <- datos %>%
  filter(
    year(fecha_del_evento) %in% c(2024, 2025),
    !is.na(edad_calculada),
    sexo_paciente %in% c("Hombre", "Mujer")
  ) %>%
  mutate(
    anio         = factor(year(fecha_del_evento)),
    sexo_paciente = factor(sexo_paciente)   # <- forzar factor en ambos
  )

# Curvas teóricas: una por combinación año × sexo
curvas <- datos_filt %>%
  group_by(anio, sexo_paciente) %>%
  summarise(
    media   = mean(edad_calculada),
    sd      = sd(edad_calculada),
    min_x   = min(edad_calculada),
    max_x   = max(edad_calculada),
    n       = n(),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(x = list(seq(min_x, max_x, length.out = 200))) %>%
  unnest(x) %>%
  ungroup() %>%
  mutate(y = dnorm(x, mean = media, sd = sd))

# Verificación: debe haber 4 combinaciones con media/sd distintas
curvas %>% distinct(anio, sexo_paciente, media, sd, n)

ggplot(datos_filt, aes(x = edad_calculada)) +
  geom_histogram(
    aes(y = after_stat(density), fill = sexo_paciente),
    bins = 15,
    color = "white",
    alpha = 0.7
  ) +
  geom_line(
    data = curvas,
    aes(x = x, y = y),
    color = "red",
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  facet_grid(anio ~ sexo_paciente) +
  labs(
    title    = "Distribución de edad por año y sexo (2024-2025)",
    subtitle = "Curva roja = normal teórica con media y sd de cada grupo",
    x = "Edad", y = "Densidad", fill = "Sexo"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

### t-test media de edad por sexo 
t_student <- datos %>%
  filter(
    year(fecha_del_evento) %in% c(2024, 2025),
    sexo_paciente %in% c("Hombre", "Mujer"),
    !is.na(edad_calculada)
  ) %>%
  mutate(anio = factor(year(fecha_del_evento))) %>%
  group_by(anio) %>%
  t_test(
    edad_calculada ~ sexo_paciente,
    var.equal = TRUE,  # Prueba t de Student (supone varianzas iguales)
    detailed = TRUE
    )

t_student

### t de Welch media de edad por sexo
t_welch <- datos %>%
  filter(
    year(fecha_del_evento)  %in% c(2024, 2025),
    sexo_paciente %in% c("Hombre", "Mujer"),
    !is.na(edad_calculada)
  ) %>%
  mutate(anio = factor(year(fecha_del_evento))) %>%
  group_by(anio) %>%
  t_test(
    edad_calculada ~ sexo_paciente,
    var.equal = FALSE,  # Prueba t de Welch (no exige varianzas iguales)
    detailed = TRUE
  )

t_welch

#### Nota metodológica:
#### Welch no exige igualdad de varianzas entre grupos
#### Funciona bien con grupos de diferentes tamaños, a pesar de la DS similar
#### Welch no corrige la asimetría, Student y Welch pueden verse afectados por la asimentría
#### Lo ppal para elegirlo esla diferencia de los grupos

### test U de Mann-Whitney o suma de rangos de Wilcoxon

u_wil <- datos %>%
  filter(
    year(fecha_del_evento)  %in% c(2024, 2025),
    sexo_paciente %in% c("Hombre", "Mujer"),
    !is.na(edad_calculada)
  ) %>%
  mutate(anio = factor(year(fecha_del_evento))) %>%
  group_by(anio) %>%
  wilcox_test(edad_calculada ~ sexo_paciente) %>%
  mutate(
    A_grupo1 = statistic / (n1 * n2),
    porcentaje_grupo1 = A_grupo1 * 100,
    porcentaje_grupo2 = 100 - porcentaje_grupo1
  )

u_wil
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTERPRETACIÓN: SUMA DE RANGOS DE WILCOXON / U DE MANN–WHITNEY
#
# Análisis real: comparación de edad entre mujeres y hombres,
# por separado para 2024 y 2025.
#
# 1. ¿QUÉ COMPARA?
# Compara los rangos de las edades individuales entre dos grupos
# independientes. Evalúa si un grupo tiende a presentar edades mayores
# que el otro. No compara directamente medias ni medianas.
#
# 2. SUPUESTOS Y ALCANCE
# - No exige normalidad.
# - Requiere observaciones independientes: los reintentos de una misma
#   persona no deben tratarse como observaciones independientes.
# - Puede interpretarse como una comparación de medianas si las
#   distribuciones tienen formas similares y solo difieren en ubicación.
# - No es una prueba que detecte cualquier diferencia de distribución.
#
# 3. VALOR p
# Un p < 0,05 aporta evidencia contra la hipótesis nula de distribuciones
# iguales, al nivel de significación elegido.
# El valor p NO indica la dirección ni la magnitud de la diferencia.
# Un p >= 0,05 NO demuestra que las distribuciones sean iguales.
#
# 4. DIRECCIÓN Y MAGNITUD: A
# En esta salida de R, statistic corresponde a U del primer grupo.
#
# A = statistic / (n1 * n2)
#
# n1 * n2 es el número total de parejas posibles entre ambos grupos.
#
# A = P(edad grupo1 > edad grupo2) + 0,5 * P(edades iguales)
#
# - A > 0,50: predominan edades mayores en grupo1.
# - A < 0,50: predominan edades mayores en grupo2.
# - A = 0,50: equilibrio en el predominio; no garantiza distribuciones iguales.
#
# El complemento:
# 1 - A = P(edad grupo1 < edad grupo2) + 0,5 * P(edades iguales)
#
# Estos porcentajes describen COMPARACIONES ENTRE PARES.
# NO representan porcentajes de personas pertenecientes a cada grupo.
# A puede calcularse aunque la prueba no sea significativa.
#
# 5. RESULTADOS REALES: group1 = Mujer; group2 = Hombre
# 2024: p = 0,0207; A = 0,429; complemento = 0,571.
# 2025: p = 0,00287; A = 0,431; complemento = 0,569.
#
# Interpretación:
# Las edades de los hombres tienden a ser mayores que las de las mujeres.
# Al comparar al azar una persona de cada sexo entre las notificadas,
# la comparación favorece una edad mayor en el hombre aproximadamente
# el 57 % de las veces, contabilizando los empates por mitad.
#
# El 57 % no es la probabilidad estricta de edad mayor: incluye medio empate.
# La magnitud de esta tendencia es muy similar en ambos años, aunque
# los valores p difieren. No implica separación completa entre grupos.
#
# 6. MEDIANAS COMO DESCRIPCIÓN
# 2024: hombres = 24 años; mujeres = 21 años.
# 2025: hombres = 25,5 años; mujeres = 22 años.
#
# Las medianas y los cuartiles describen las edades.
# Mann–Whitney no contrasta específicamente estas diferencias de medianas.
#
# 7. REDACCIÓN PARA EL INFORME
# "Se observaron diferencias estadísticamente significativas en la
# distribución de edad entre sexos en 2024 y 2025, con tendencia a
# edades mayores en hombres (p = 0,0207 y p = 0,00287, respectivamente)."
#
# 8. CAMBIAR EL ORDEN DE LOS GRUPOS
# Invertir group1 y group2 reemplaza A por 1 - A.
# El valor p bilateral y el hallazgo epidemiológico no cambian.
#
# 9. DIFERENCIA RESPECTO DE WELCH
# Welch compara medias; Mann–Whitney compara rangos.
# Pueden producir valores p distintos porque responden preguntas distintas.
# No elegir entre ellas según cuál resulte significativa.
#
# Los ejemplos de peso, semana epidemiológica y métodos de autolesión
# fueron únicamente ilustrativos; no son resultados de este análisis.
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::







# Sexo

datos %>%
  filter(year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  janitor::tabyl(anio, sexo_paciente, show_na = TRUE) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()


# Nacionalidad




## Identidad y pertenencia (Personas únicas, evitar distorcionar la distribución)
## identidad de género, orientación sexual, pertenencia y pueblo originario

## Residencia (Personas únicas, evitar distorcionar la distribución)
## Región y comuna de residencia del paciente

## Notificación (Total de eventos notificados)
## Comuna y establecimiento de notificación

## Características del evento (Total de eventos)
## Subclasificación, método de lesión, lugar del evento

## Salud mental (Personas únicas, evitar distorcionar la distribución)
## Antecedentes, descripción libre, tratamiento

## Contexto social (Total de eventos)
## Acompañante, estudia/trabaja

## Territorio de actividades(Total de eventos)
## Región y comuna de estudios y/o trabajo












