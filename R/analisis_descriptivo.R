#### Visión general del conjunto de datos ####
skimr::skim(datos)


###############################################################################
### Análisis exploratorio
#### curva epidemicia x mes ####
breaks_mensuales = seq.Date(from = as.Date("2024-01-01"),
                            to = as.Date("2027-01-01"),
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


#### Curvas semanales: Total ####
for (anio in c(2024, 2025, 2026)) print(curva_semanal(datos, anio, NULL))

#### Curvas semanales: Comuna de notificación ####
for (anio in c(2024, 2025, 2026)) print(curva_semanal(datos, anio, "comuna"))

#### Curvas semanales: Establecimiento ####
for (anio in c(2024, 2025, 2026)) print(curva_semanal(datos, anio, "establecimiento_salud"))

#### Curvas semanales: Sexo ####
for (anio in c(2024, 2025, 2026)) print(curva_semanal(datos, anio, "sexo_paciente"))

#### Curvas semanales: Método de lesión ####
for (anio in c(2024, 2025, 2026)) print(curva_semanal(datos, anio, "metodo_de_lesion"))

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

plot(density(datos_2024$edad_calculada, na.rm = TRUE))

plot(density(datos_2025$edad_calculada, na.rm = TRUE))


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

tabla_por_anio(datos_agg, c("edad_categoria"), c(2024, 2025))

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
  
tabla_por_anio(datos_agg, c("nacionalidad_paciente"), c(2024, 2025)) 
  
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

tabla_por_anio(datos_agg, c("orientacion_sexual"), c(2024, 2025)) 

#### Pertenencia a pueblo originario ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, pueblo_originario)

datos_agg

tabla_por_anio(datos_agg, c("pueblo_originario"), c(2024, 2025)) 

#### Lugar del evento ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, lugar_del_evento)

datos_agg

tabla_por_anio(datos_agg, c("lugar_del_evento"), c(2024, 2025)) 

#### Establecimiento de notificación ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, establecimiento_salud)

datos_agg

tabla_por_anio(datos_agg, c("establecimiento_salud"), c(2024, 2025)) 

#### Comuna de notificación ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2023, 2024, 2025, 2026)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, comuna)

datos_agg

tabla_por_anio(datos_agg, c("comuna"), c(2023, 2024, 2025, 2026)) 

#### Región de residencia del paciente ####

datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, comuna_paciente)

datos_agg

tabla_por_anio(datos_agg, c("comuna_paciente"), c(2024, 2025)) 

#### Antecedentes salud mental ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, tiene_antecedentes_salud_mental)

datos_agg

tabla_por_anio(datos_agg, c("tiene_antecedentes_salud_mental"), c(2024, 2025)) 

#### Antecedentes salud mental x sexo ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025),
    sexo_paciente %in% c("Hombre", "Mujer")) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, sexo_paciente, tiene_antecedentes_salud_mental)

datos_agg

tabla_por_anio(datos_agg, c("sexo_paciente", "tiene_antecedentes_salud_mental"), c(2024, 2025)) 

#### Antecedentes salud mental x edad agrupada ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, tiene_antecedentes_salud_mental, edad_categoria)

datos_agg

tabla_por_anio(datos_agg, c("edad_categoria", "tiene_antecedentes_salud_mental"), c(2024, 2025)) 


#### Antecedentes salud mental x método de lesión ####
datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, tiene_antecedentes_salud_mental, metodo_de_lesion)

datos_agg

tabla_por_anio(datos_agg, c("tiene_antecedentes_salud_mental", "metodo_de_lesion"), c(2024, 2025)) 


#### Tratamiento salud mental ####

datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, tiene_tratamiento_salud_mental)

datos_agg

tabla_por_anio(datos_agg, c("tiene_tratamiento_salud_mental"), c(2024, 2025)) 

#### Tratamiento salud mental x sexo ####

datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025),
    sexo_paciente %in% c("Hombre", "Mujer"),
    !is.na(tiene_tratamiento_salud_mental)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, tiene_tratamiento_salud_mental, sexo_paciente)

datos_agg

tabla_por_anio(datos_agg, c("tiene_tratamiento_salud_mental", "sexo_paciente"), c(2024, 2025)) 

#### Tratamiento salud mental x edad agrupada ####

datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025),
    !is.na(tiene_tratamiento_salud_mental)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, tiene_tratamiento_salud_mental, edad_categoria)

datos_agg

tabla_por_anio(datos_agg, c("tiene_tratamiento_salud_mental", "edad_categoria"), c(2024, 2025)) 

#### Tratamiento salud mental x metodo de autolesión ####

datos_agg <- datos %>% 
  filter(
    year(fecha_del_evento) %in% c(2024, 2025),
    !is.na(tiene_tratamiento_salud_mental),
    !is.na(metodo_de_lesion)) %>%
  mutate(anio = year(fecha_del_evento)) %>%
  count(anio, tiene_tratamiento_salud_mental, metodo_de_lesion)

datos_agg

tabla_por_anio(datos_agg, c("tiene_tratamiento_salud_mental", "metodo_de_lesion"), c(2024, 2025)) 


