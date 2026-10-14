# Requiere datos preparados y funciones.R.
poblacion <- readxl::read_excel(file.path("data", "input", "poblaciones.xlsx"))
names(poblacion) <- janitor::make_clean_names(names(poblacion))
# Pendiente de confirmar: el original utiliza comuna de notificación.
variable_comuna_modelo <- "comuna"
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
    comuna = normalizar_comuna(.data[[variable_comuna_modelo]]),
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


