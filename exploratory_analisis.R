# Paquetes
library(here)
library(readxl)
library(tidyverse)
library(janitor)
library(skimr)
library(naniar)

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

names(datos) <- make_clean_names(names(datos))

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
    labels = c("Infancia", "Niñez", "Adolescencia",
               "Juventud", "Adultez", "Ancianidad"),
    include.lowest = TRUE,  # En el primer grupó incluye el primer valor 
    right = TRUE  # TRUE por defecto, abierto por la derecha
   )
  )
  
# Visión general del conjunto de datos
skimr::skim(datos)


