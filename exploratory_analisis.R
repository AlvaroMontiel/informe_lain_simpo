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

### Estructura de los datos 

dim(datos)  # Filas y columnas
names(datos)  # Nombre de las columnas
head(datos)
summary(datos)  # Resumen estadístico


### Limpiar tipos

variables <- c(
  "Fecha Atencion Urgencia", "Semana Epidemiologica", "Clasificacion",
  "Subclasificacion", "Region", "Comuna", "Establecimiento Salud", 
  "Identificacion Paciente", "ID/RUT Paciente", "Sexo Paciente",
  "Fecha Nacimiento Paciente", "Edad Calculada", "Region Paciente", 
  "Comuna Paciente", "Direccion Paciente", "Se considera pueblo originario", 
  "Pueblo originario", "Se considera afrodescendiente", "Identidad de Genero",
  "Orientacion Sexual", "Nacionalidad Paciente", "Persona Bajo Cuidado",
  "Lesion fue Autoinfligida", "Lesion fue Intencional", "Tuvo intencion de Morir",
  "Tiene Antecedentes salud mental", "Antecedentes salud mental", 
  "Tiene tratamiento salud mental", "Lugar tratamiento salud mental",
  "Paciente estudia actualmente", "Region de estudios", "Comuna de estudios",             
  "Nombre establecimiento estudio", "Paciente trabaja actualmente", 
  "Region de trabajo", "Comuna de trabajo", "Fecha del evento",
  "Tipo de Evento", "Metodo de Lesion", "Lugar del evento", "compare_key",
  "es_duplicado", "id_rut_id_base"
  )




table(datos$`Factor Precipitante`)










