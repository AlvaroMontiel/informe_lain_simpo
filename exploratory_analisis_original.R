#### Paquetes ####
library(here)
library(readxl)
library(tidyverse)
library(janitor)
library(skimr)
library(naniar)
library(rstatix)
library(gtsummary)

#### Carga de datos ####
ruta <- here("data", "input", "set_datos_lain_para_analisis.xlsx")

datos <- readxl::read_excel(ruta)
View(datos)

#### Estructura y revisión de los datos ####

dim(datos)  # Filas y columnas
names(datos)  # Nombre de las columnas
head(datos)
summary(datos)  # Resumen estadístico


#### Limpiar tipos ####

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

#### estandarizar nombre de las columnas ####

names(datos) <- janitor::make_clean_names(names(datos))

#### Seleccionar columnas ####
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
   
#### grupos de edad ####
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
  

#### Visión general del conjunto de datos ####
skimr::skim(datos)


###############################################################################
### Análisis exploratorio
#### curva epidemicia x mes ####
breaks_mensuales = seq.Date(from = as.Date("2024-01-02"),
                            to = as.Date("2026-09-30"),
                            by = "months")

datos %>%
  filter(year(fecha_del_evento) %in% c(2024, 2025, 2026)) %>%
  ggplot(aes(x = fecha_del_evento)) +
  geom_histogram(
    breaks = breaks_mensuales,
    fill = "steelblue", color = "white"
  ) +
  scale_x_date(
    date_breaks = "3 months",          # cada 3 meses para no saturar
    date_labels = "%b\n%Y",            # mes arriba, año abajo
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  )+
  labs(title = "Nuevos eventos por mes 2024-2026", 
       x = "Mes del evento", 
       y = "Número de eventos mensuales") +
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0,
                                face = "italic"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5))


#### curva por semana epidemiológica ####
anios <- c(2024, 2025, 2026)
for (anio in anios) {
  inicio <- as.Date(paste0(anio, "-01-01"))
  fin    <- as.Date(paste0(anio, "-12-31"))
  
  grafico <- datos %>%
    filter(year(fecha_del_evento) == anio) %>%
    ggplot(aes(x = fecha_del_evento)) +
    geom_histogram(
      breaks = seq(inicio, fin + 1, by = "1 week"),
      fill = "steelblue", color = "white"
    ) +
    scale_x_date(
      date_breaks = "4 weeks", 
      date_labels = "%a\n%d %b\n%Y"
      ) +
    scale_y_continuous(
      expand =c(0, 0)
    )+
    labs(title = paste0("Nuevos eventos por semana epidemiolǵica ", anio),
         x = "Semana del evento", 
         y = "Número de eventos semanales")+
    theme_minimal()+
    theme(
      plot.caption = element_text(hjust = 0,
                                  face = "italic"),
      axis.title = element_text(face ="bold"))
  
  print(grafico)
}


#### curva por semana epidemiológica x comuna ####
anios <- c(2024, 2025, 2026)
for (anio in anios) {
  inicio <- as.Date(paste0(anio, "-01-01"))
  fin    <- as.Date(paste0(anio, "-12-31"))
  
  grafico <- datos %>%
    filter(year(fecha_del_evento) == anio) %>%
    ggplot() +
    geom_histogram(
      mapping = aes(
        x = fecha_del_evento,
        group = comuna,
        fill = comuna
        ),
      breaks = seq(inicio, fin + 1, by = "1 week"),
      color = "black"
    ) +
    scale_x_date(
      date_breaks = "4 weeks", 
      date_labels = "%a\n%d %b\n%Y"
    ) +
    scale_y_continuous(
      expand =c(0, 0)
    )+
    labs(title = paste0("Nuevos eventos por semana epidemiolǵica según comuna ", anio),
         x = "Semana del evento", 
         y = "Número de eventos semanales")+
    theme_minimal()+
    theme(
      plot.caption = element_text(hjust = 0,
                                  face = "italic"),
      axis.title = element_text(face ="bold"))
  
  print(grafico)
}

#### curva por semana epidemiológica x establecimiento ####
for (anio in anios) {
  inicio <- as.Date(paste0(anio, "-01-01"))
  fin    <- as.Date(paste0(anio, "-12-31"))
  
  grafico <- datos %>%
    filter(year(fecha_del_evento) == anio) %>%
    ggplot() +
    geom_histogram(
      mapping = aes(
        x = fecha_del_evento,
        group = establecimiento_salud,
        fill = establecimiento_salud
      ),
      breaks = seq(inicio, fin + 1, by = "1 week"),
      color = "black"
    ) +
    scale_x_date(
      date_breaks = "4 weeks", 
      date_labels = "%a\n%d %b\n%Y"
    ) +
    scale_y_continuous(
      expand =c(0, 0)
    )+
    labs(title = paste0("Nuevos eventos por semana epidemiolǵica según comuna ", anio),
         x = "Semana del evento", 
         y = "Número de eventos semanales")+
    theme_minimal()+
    theme(
      plot.caption = element_text(hjust = 0,
                                  face = "italic"),
      axis.title = element_text(face ="bold"))
  
  print(grafico)
}


#### Curva por sexo ####
for (anio in anios) {
  inicio <- as.Date(paste0(anio, "-01-01"))
  fin    <- as.Date(paste0(anio, "-12-31"))
  
  grafico <- datos %>%
    filter(year(fecha_del_evento) == anio) %>%
    ggplot() +
    geom_histogram(
      mapping = aes(
        x = fecha_del_evento,
        group = sexo_paciente,
        fill = sexo_paciente,
        position = "dodge"
      ),
      breaks = seq(inicio, fin + 1, by = "1 week"),
      color = "black"
    ) +
    scale_x_date(
      date_breaks = "4 weeks", 
      date_labels = "%a\n%d %b\n%Y"
    ) +
    scale_y_continuous(
      expand =c(0, 0)
    )+
    labs(title = paste0("Nuevos eventos por semana epidemiolǵica según comuna ", anio),
         x = "Semana del evento", 
         y = "Número de eventos semanales")+
    theme_minimal()+
    theme(
      plot.caption = element_text(hjust = 0,
                                  face = "italic"),
      axis.title = element_text(face ="bold"))
  
  print(grafico)
}

#### Curva por metodo de autolesion ####
for (anio in anios) {
  inicio <- as.Date(paste0(anio, "-01-01"))
  fin    <- as.Date(paste0(anio, "-12-31"))
  
  grafico <- datos %>%
    filter(year(fecha_del_evento) == anio) %>%
    ggplot() +
    geom_histogram(
      mapping = aes(
        x = fecha_del_evento,
        group = metodo_de_lesion,
        fill = metodo_de_lesion
      ),
      breaks = seq(inicio, fin + 1, by = "1 week"),
      color = "black"
    ) +
    scale_x_date(
      date_breaks = "4 weeks", 
      date_labels = "%a\n%d %b\n%Y"
    ) +
    scale_y_continuous(
      expand =c(0, 0)
    )+
    labs(title = paste0("Nuevos eventos por semana epidemiolǵica según comuna ", anio),
         x = "Semana del evento", 
         y = "Número de eventos semanales")+
    theme_minimal()+
    theme(
      plot.caption = element_text(hjust = 0,
                                  face = "italic"),
      axis.title = element_text(face ="bold"))
  
  print(grafico)
}

#### Curva desestacionalizada ####





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

#### Edad Agrupada ####
datos_agg <- datos %>% 
  filter(year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, edad_categoria)

datos_agg

datos_agg %>%
  group_by(edad_categoria) %>%
  summarise(
    Año_2024 = sum(n[anio == 2024]),
    Año_2025 = sum(n[anio == 2025]),
  ) %>%
  adorn_totals() %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")

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
####
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

### tabla edad por sexo y año
datos_agg <- datos %>%
  filter(
    year(fecha_del_evento) %in% c(2024, 2025),
    !is.na(edad_calculada),
    sexo_paciente %in% c("Hombre", "Mujer")) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, edad_categoria, sexo_paciente)
  
print(n = 40, datos_agg)

datos_agg %>% 
  group_by(anio, sexo_paciente) %>%
  summarise(
    Niñez = sum(n[edad_categoria == "Niñez 6-11"], na.rm = T),
    Adolescencia = sum(n[edad_categoria == "Adolescencia 12-19"], na.rm = T),
    Juventud = sum(n[edad_categoria == "Juventud 20-24"], na.rm = T),
    Adultez = sum(n[edad_categoria == "Adultez 25-59"], na.rm = T),
    Ancianidad = sum(n[edad_categoria == "Ancianidad 60 y más"], na.rm = T),
    total = sum(n, na.rm = T)
  ) %>%
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 


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

#### Test estadísticos edad x sexo ####
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

####
#### Edad x metodo de autolesion ####
datos %>%
  filter(
    year(fecha_del_evento) %in% c(2025),
    !is.na(edad_calculada)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  drop_na(metodo_de_lesion) %>%
  group_by(anio, metodo_de_lesion) %>%
  summarise(
    n = n(),
    min = min(edad_calculada, na.rm = TRUE), 
    max = max(edad_calculada, na.rm = TRUE),
    media = mean(edad_calculada, na.rm = TRUE),
    mediana = median(edad_calculada, na.rm = TRUE),
    DE = sd(edad_calculada, na.rm = TRUE),
    Q1 = quantile(edad_calculada, prob = c(0.25), na.rm = TRUE),
    Q3 = quantile(edad_calculada, prob = c(0.75), na.rm = TRUE),
    .groups = "drop"
  )

datos_agg <- datos %>%
  filter(
    year(fecha_del_evento) %in% c(2025),
    !is.na(edad_calculada)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  drop_na(metodo_de_lesion) %>%
  count(anio, edad_categoria, metodo_de_lesion)

print(n = 40, datos_agg)

datos_agg %>% 
  group_by(anio, metodo_de_lesion) %>%
  summarise(
    Niñez = sum(n[edad_categoria == "Niñez 6-11"], na.rm = T),
    Adolescencia = sum(n[edad_categoria == "Adolescencia 12-19"], na.rm = T),
    Juventud = sum(n[edad_categoria == "Juventud 20-24"], na.rm = T),
    Adultez = sum(n[edad_categoria == "Adultez 25-59"], na.rm = T),
    Ancianidad = sum(n[edad_categoria == "Ancianidad 60 y más"], na.rm = T),
    total = sum(n, na.rm = T)
  ) %>%
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 

#### Sexo x metodo de autolesion ####
datos_agg <- datos %>%
  filter(
    year(fecha_del_evento) %in% c(2025),
    !is.na(edad_calculada),
    sexo_paciente %in% c("Hombre", "Mujer")
  ) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  drop_na(metodo_de_lesion) %>%
  count(anio, sexo_paciente, metodo_de_lesion)
  
print(n = 40, datos_agg)

datos_agg %>% 
  group_by(anio, metodo_de_lesion) %>%
  summarise(
    hombres = sum(n[sexo_paciente == "Hombre"], na.rm = T),
    mujeres = sum(n[sexo_paciente == "Mujer"], na.rm = T),
    total = sum(n, na.rm = T)
  ) %>%
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 


#### Edad x sexo x metodo de lesion x año ####
tabla <- datos %>%
  filter(
    year(fecha_del_evento) == 2025,
    !is.na(edad_calculada),
    sexo_paciente %in% c("Hombre", "Mujer")
  ) %>%
  count(metodo_de_lesion, edad_categoria, sexo_paciente) %>%
  pivot_wider(
    names_from  = c(sexo_paciente, edad_categoria),
    values_from = n,
    values_fill = 0,
    names_sep   = "_"
  ) %>%
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 

tabla

#### Nacionalidad ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, nacionalidad_paciente)
  
datos_agg
  
datos_agg %>%
  group_by(nacionalidad_paciente) %>%
  summarise(
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025])
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 
  
#### Identidad de genero ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, identidad_de_genero)

datos_agg

datos_agg %>%
  group_by(identidad_de_genero) %>%
  summarise(
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025])
  # summarise(
  #   femenino = sum(n[identidad_de_genero == "Femenina"]),
  #   masculino = sum(n[identidad_de_genero == "Masculino"]),
  #   no_binarie = sum(n[identidad_de_genero == "No Binarie"]),
  #   sin_informacion = sum(n[identidad_de_genero == "No hay Informacion"]),
  #   otra = sum(n[identidad_de_genero == "Otra"]),
  #   trans_femenina = sum(n[identidad_de_genero == "Transgenero Femenina"]),
  #   trans_mnasculino = sum(n[identidad_de_genero == "Transgenero Masculino"]),
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 

#### Oriencacion sexual ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, orientacion_sexual)

datos_agg

datos_agg %>%
  group_by(orientacion_sexual) %>%
  summarise(
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025])
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 

#### Pertenencia a pueblo originario ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, pueblo_originario)

datos_agg

datos_agg %>%
  group_by(pueblo_originario) %>%
  summarise(
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025])
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 

#### Lugar del evento ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, lugar_del_evento)

datos_agg

datos_agg %>%
  group_by(lugar_del_evento) %>%
  summarise(
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025])
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 

#### Establecimiento de notificación ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, establecimiento_salud)

datos_agg

datos_agg %>%
  group_by(establecimiento_salud) %>%
  summarise(
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025])
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 

#### Comuna de notificación ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2023, 2024, 2025, 2026)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, comuna)

datos_agg

datos_agg %>%
  group_by(comuna) %>%
  summarise(
    año_2023 = sum(n[anio == 2023]),
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025]),
    año_2026 = sum(n[anio == 2026])
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 

#### Región de residencia del paciente ####

datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, comuna_paciente)

datos_agg

datos_agg %>%
  group_by(comuna_paciente) %>%
  summarise(
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025])
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 

#### Antecedentes salud mental ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, tiene_antecedentes_salud_mental)

datos_agg

datos_agg %>%
  group_by(tiene_antecedentes_salud_mental) %>%
  summarise(
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025])
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 

#### Antecedentes salud mental x sexo ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025),
    sexo_paciente %in% c("Hombre", "Mujer")) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, sexo_paciente, tiene_antecedentes_salud_mental)

datos_agg

datos_agg %>%
  group_by(sexo_paciente, tiene_antecedentes_salud_mental) %>%
  summarise(
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025])
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 

#### Antecedentes salud mental x edad agrupada ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, tiene_antecedentes_salud_mental, edad_categoria)

datos_agg

datos_agg %>%
  group_by(edad_categoria, tiene_antecedentes_salud_mental) %>%
  summarise(
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025])
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 


#### Antecedentes salud mental x método de lesión ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, tiene_antecedentes_salud_mental, metodo_de_lesion)

datos_agg

datos_agg %>%
  group_by(tiene_antecedentes_salud_mental, metodo_de_lesion) %>%
  summarise(
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025])
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 


#### Tratamiento salud mental ####

datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, tiene_tratamiento_salud_mental)

datos_agg

datos_agg %>%
  group_by(tiene_tratamiento_salud_mental) %>%
  summarise(
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025])
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 

#### Tratamiento salud mental x sexo ####

datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025),
    sexo_paciente %in% c("Hombre", "Mujer"),
    !is.na(tiene_tratamiento_salud_mental)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, tiene_tratamiento_salud_mental, sexo_paciente)

datos_agg

datos_agg %>%
  group_by(tiene_tratamiento_salud_mental, sexo_paciente) %>%
  summarise(
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025])
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 

#### Tratamiento salud mental x edad agrupada ####

datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025),
    !is.na(tiene_tratamiento_salud_mental)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, tiene_tratamiento_salud_mental, edad_categoria)

datos_agg

datos_agg %>%
  group_by(tiene_tratamiento_salud_mental, edad_categoria) %>%
  summarise(
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025])
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 

#### Tratamiento salud mental x metodo de autolesión ####

datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025),
    !is.na(tiene_tratamiento_salud_mental),
    !is.na(metodo_de_lesion)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, tiene_tratamiento_salud_mental, metodo_de_lesion)

datos_agg

datos_agg %>%
  group_by(tiene_tratamiento_salud_mental, metodo_de_lesion) %>%
  summarise(
    año_2024 = sum(n[anio == 2024]),
    año_2025 = sum(n[anio == 2025])
  ) %>% 
  adorn_totals() %>%
  adorn_percentages("col") %>%                      
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") 


####################################
### Poblaciones
ruta <- here("data", "input", "poblaciones.xlsx")

poblacion <- readxl::read_excel(ruta)

names(poblacion) <- janitor::make_clean_names(names(poblacion))
names(poblacion)

####################################
### Modelos de regresión: Poisson ###
####################################

library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(lubridate)

#### 1. Funciones comunes ####

normalizar_comuna <- function(x) {
  x %>%
    str_squish() %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_upper()
}

categorizar_edad <- function(x) {
  cut(
    x,
    breaks = c(0, 5, 11, 19, 24, 59, Inf),
    labels = c(
      "Infancia 0-5",
      "Niñez 6-11",
      "Adolescencia 12-19",
      "Juventud 20-24",
      "Adultez 25-59",
      "Ancianidad 60 y más"
    ),
    include.lowest = TRUE,
    right = TRUE  # Intervalos cerrados por la derecha
  )
}

#### 2. Población en formato largo ####
# Se conserva intacto el dataframe original: poblacion.

poblacion_long <- poblacion %>%
  select(
    nombre_region,
    nombre_comuna,
    sexo,
    edad,
    all_of(paste0("poblacion_", 2024:2025))
  ) %>%
  filter(normalizar_comuna(nombre_region) == "ANTOFAGASTA") %>%
  pivot_longer(
    cols = all_of(paste0("poblacion_", 2024:2025)),
    names_to = "anio",
    names_prefix = "poblacion_",
    values_to = "poblacion"
  ) %>%
  mutate(
    anio = as.integer(anio),
    nombre_comuna = normalizar_comuna(nombre_comuna),
    edad_categoria = categorizar_edad(edad),
    sexo = case_when(
      as.character(sexo) == "1" ~ "Hombre",
      as.character(sexo) == "2" ~ "Mujer",
      TRUE ~ NA_character_
    )
  )

# Verificar claves y cantidades antes de sumar.
stopifnot(
  !anyNA(poblacion_long$nombre_comuna),
  all(nzchar(poblacion_long$nombre_comuna)),
  !anyNA(poblacion_long$edad_categoria),
  !anyNA(poblacion_long$sexo),
  all(is.finite(poblacion_long$poblacion)),
  all(poblacion_long$poblacion >= 0)
)

# Cada comuna, edad simple, sexo y año debe aparecer una sola vez.
# Si falla, revisar duplicados antes de sumar poblaciones.
stopifnot(
  anyDuplicated(
    poblacion_long %>%
      select(nombre_comuna, edad, sexo, anio)
  ) == 0L
)

#### 3. Población agregada por grupo etario ####

poblacion_agrupada <- poblacion_long %>%
  group_by(nombre_comuna, edad_categoria, sexo, anio) %>%
  summarise(
    poblacion = sum(poblacion),
    .groups = "drop"
  )

# El offset requiere denominadores positivos.
stopifnot(
  all(is.finite(poblacion_agrupada$poblacion)),
  all(poblacion_agrupada$poblacion > 0)
)

#### 4. Preparar eventos del período ####
# comuna debe corresponder a la comuna de residencia.
# Este bloque cuenta filas: conserva los reintentos presentes en datos.
# Se asume que los duplicados de notificación ya fueron eliminados.

eventos_periodo <- datos %>%
  filter(year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(
    anio = as.integer(year(fecha_del_evento)),
    comuna = normalizar_comuna(comuna),
    sexo_paciente = str_squish(as.character(sexo_paciente)),
    edad_categoria = categorizar_edad(edad_calculada)
  )

# Registrar eventos excluidos por no tener sexo con denominador disponible.
eventos_sin_denominador_sexo <- eventos_periodo %>%
  filter(
    is.na(sexo_paciente) |
      !sexo_paciente %in% c("Hombre", "Mujer")
  )

print(
  eventos_sin_denominador_sexo %>%
    count(sexo_paciente, anio, name = "eventos_excluidos")
)

#### 5. Contar eventos por estrato ####

conteos_modelo <- eventos_periodo %>%
  filter(sexo_paciente %in% c("Hombre", "Mujer")) %>%
  count(
    comuna,
    edad_categoria,
    sexo_paciente,
    anio,
    name = "intentos"
  )

#### 6. Verificar correspondencia antes del cruce ####

conteos_sin_poblacion <- conteos_modelo %>%
  anti_join(
    poblacion_agrupada,
    by = c(
      "comuna" = "nombre_comuna",
      "edad_categoria",
      "sexo_paciente" = "sexo",
      "anio"
    )
  )

print(conteos_sin_poblacion, n = Inf)

# Si falla, corregir las claves o documentar exclusiones antes de continuar.
stopifnot(nrow(conteos_sin_poblacion) == 0L)

#### 7. Construir tabla final ####
# Partir desde población conserva los estratos sin eventos.

datos_modelo <- poblacion_agrupada %>%
  left_join(
    conteos_modelo,
    by = c(
      "nombre_comuna" = "comuna",
      "edad_categoria",
      "sexo" = "sexo_paciente",
      "anio"
    )
  ) %>%
  mutate(
    intentos = coalesce(intentos, 0L),
    tasa_100000 = intentos / poblacion * 100000
  )

# Verificar que ningún evento elegible se perdió o duplicó.
stopifnot(
  sum(datos_modelo$intentos) == sum(conteos_modelo$intentos)
)

print(
  tibble(
    eventos_periodo = nrow(eventos_periodo),
    excluidos_por_sexo = nrow(eventos_sin_denominador_sexo),
    eventos_elegibles = sum(conteos_modelo$intentos),
    eventos_en_tabla_final = sum(datos_modelo$intentos)
  )
)

#### 8. Revisar totales por categoría ####

totales_categoria <- bind_rows(
  lapply(
    c("nombre_comuna", "edad_categoria", "sexo", "anio"),
    function(variable) {
      datos_modelo %>%
        group_by(valor = .data[[variable]]) %>%
        summarise(
          intentos = sum(intentos),
          .groups = "drop"
        ) %>%
        mutate(
          valor = as.character(valor),
          variable = variable
        ) %>%
        select(variable, valor, intentos)
    }
  )
)

print(totales_categoria, n = Inf)

categorias_sin_eventos <- totales_categoria %>%
  filter(intentos == 0)

#### 9. Ajustar Poisson si no hay categorías completas sin eventos ####

modelo <- NULL

if (nrow(categorias_sin_eventos) > 0L) {
  
  print(categorias_sin_eventos, n = Inf)
  
  message(
    "No se ajustó el modelo: hay categorías completas sin eventos. ",
    "Definir su tratamiento y el alcance del análisis antes de continuar. ",
    "datos_modelo conserva todos los estratos para la descripción."
  )
  
} else {
  
  modelo <- glm(
    intentos ~ nombre_comuna + edad_categoria + sexo + factor(anio),
    offset = log(poblacion),
    family = poisson(link = "log"),
    data = datos_modelo
  )
  
  print(modelo$converged)
  print(summary(modelo))
  
  # Diagnóstico inicial de dispersión: no sustituye otros diagnósticos.
  dispersion_pearson <- sum(
    residuals(modelo, type = "pearson")^2
  ) / df.residual(modelo)
  
  print(dispersion_pearson)
}

# Conservar datos_modelo completo para las tablas descriptivas.
# Crear otra tabla exclusivamente para el modelo restringido.

datos_modelo_restringido <- datos_modelo %>%
  filter(
    !nombre_comuna %in% c(
      "MEJILLONES",
      "OLLAGUE",
      "SAN PEDRO DE ATACAMA"
    ),
    edad_categoria != "Infancia 0-5"
  ) %>%
  mutate(
    nombre_comuna = relevel(
      factor(nombre_comuna),
      ref = "ANTOFAGASTA"
    ),
    edad_categoria = relevel(
      droplevels(edad_categoria),
      ref = "Adolescencia 12-19"
    ),
    sexo = relevel(factor(sexo), ref = "Hombre"),
    anio = relevel(factor(anio), ref = "2024")
  )

modelo <- glm(
  intentos ~ nombre_comuna + edad_categoria + sexo + anio,
  offset = log(poblacion),
  family = poisson(link = "log"),
  data = datos_modelo_restringido
)

modelo$converged
summary(modelo)

dispersion_pearson <- sum(
  residuals(modelo, type = "pearson")^2
) / df.residual(modelo)

dispersion_pearson


###################################
modelo_quasi <- update(
  modelo,
  family = quasipoisson(link = "log")
)

summary(modelo_quasi)

# Razones de tasas e IC del 95 % de Wald,
# utilizando los errores estándar ajustados y distribución t.
tabla_coef <- coef(summary(modelo_quasi))
critico <- qt(0.975, df = df.residual(modelo_quasi))

resultados_quasi <- tibble(
  termino = rownames(tabla_coef),
  razon_tasas = exp(tabla_coef[, 1]),
  IC95_inf = exp(tabla_coef[, 1] - critico * tabla_coef[, 2]),
  IC95_sup = exp(tabla_coef[, 1] + critico * tabla_coef[, 2]),
  p = tabla_coef[, 4]
)

resultados_quasi


#### Modelos con interacción: comuna × edad × sexo ####

# 1. Revisar eventos por combinación, sumando ambos años
conteos_interaccion <- datos_modelo_restringido %>%
  group_by(nombre_comuna, edad_categoria, sexo) %>%
  summarise(
    intentos = sum(intentos),
    .groups = "drop"
  )

print(conteos_interaccion, n = Inf)

# Combinaciones sin eventos en ambos años
conteos_interaccion %>%
  filter(intentos == 0) %>%
  print(n = Inf)


# 2. Modelo sin interacciones: referencia para comparar
modelo_principal <- modelo_quasi


# 3. Modelo con todas las interacciones de dos variables
modelo_interaccion_doble <- update(
  modelo_principal,
  formula. = intentos ~
    (nombre_comuna + edad_categoria + sexo)^2 +
    anio
)


# 4. Modelo con interacción triple
modelo_interaccion_triple <- update(
  modelo_principal,
  formula. = intentos ~
    nombre_comuna * edad_categoria * sexo +
    anio
)

summary(modelo_interaccion_triple)


# 5. Diagnósticos iniciales
modelo_interaccion_triple$converged
df.residual(modelo_interaccion_triple)

dispersion_triple <- sum(
  residuals(modelo_interaccion_triple, type = "pearson")^2
) / df.residual(modelo_interaccion_triple)

dispersion_triple

# Coeficientes y errores estándar:
# buscar valores extremos o errores estándar muy grandes.
coef(summary(modelo_interaccion_triple))


# 6. Comparaciones entre modelos anidados
# Para quasi-Poisson se usa una prueba F aproximada.
# Interpretar solo si los ajustes son estables.

# ¿Aportan las interacciones dobles respecto del modelo principal?
anova(
  modelo_principal,
  modelo_interaccion_doble,
  test = "F"
)

# ¿Aporta la interacción triple más allá de las dobles?
anova(
  modelo_interaccion_doble,
  modelo_interaccion_triple,
  test = "F"
)

###################

modelo_comuna_edad <- update(
  modelo_principal,
  formula. = intentos ~ nombre_comuna * edad_categoria + sexo + anio
)

anova(
  modelo_principal,
  modelo_comuna_edad,
  test = "F"
)


