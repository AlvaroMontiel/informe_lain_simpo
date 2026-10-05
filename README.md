# Analisis

| Dimensión | Variables | Análisis inicial |
|---|---|---|
| Temporal | Fecha del evento, semana epidemiológica | Número de eventos por mes; distribución semanal si aporta información adicional. |
| Demográfica | Sexo, edad, nacionalidad | Personas únicas por categoría; mediana y rango intercuartílico de edad; grupos etarios. |
| Identidad y pertenencia | Identidad de género, orientación sexual, pertenencia y pueblo originario | Distribución de personas, mostrando información faltante o desconocida. |
| Residencia | Región y comuna del paciente | Distribución territorial de personas únicas. |
| Notificación | Comuna, establecimiento | Eventos notificados por establecimiento y comuna, una vez confirmado qué representa esta última. |
| Características del evento | Subclasificación, método de lesión, lugar del evento | Número y proporción de **eventos** por categoría. |
| Salud mental | Antecedentes, descripción libre, tratamiento | Proporción de personas con antecedentes y tratamiento registrados en el episodio seleccionado. |
| Contexto social | Acompañante, estudia, trabaja | Distribución de estas características en el episodio seleccionado. |
| Territorio de actividades | Región y comuna de estudios y trabajo | Descripción entre quienes estudian o trabajan, respectivamente. |


La principal precisión es que algunas variables conviene analizarlas por evento. 
El método, el lugar, la subclasificación y el acompañante pueden cambiar entre episodios. 
Si utilizas solo el primer registro de cada persona, describirás su primer evento 
observado y perderás parte de la información sobre los siguientes. 
Para edad, sexo y perfil demográfico, contar una vez a cada persona sigue siendo 
coherente con tu objetivo.


También hay tres decisiones que dejaría resueltas antes de explorar:
- Denominadores: presentar cada proporción con su número de registros y aclarar 
si corresponde a personas, eventos o un subconjunto. Por ejemplo, el pueblo 
específico entre quienes declaran pertenencia; la comuna de estudios entre 
quienes estudian.
- Información faltante: distinguir «no», «desconocido», «no responde» y dato 
vacío, según las categorías reales del sistema. Esto será especialmente 
relevante para interpretar antecedentes, tratamiento, identidad y orientación 
sexual.
- Texto libre de salud mental: primero revisar su contenido y establecer 
categorías reproducibles. Mantendría separado lo declarado en la 
notificación de un diagnóstico clínico confirmado.

La dirección del paciente puede ayudar a validar residencia o desarrollar 
después un análisis espacial, pero no la incluiría como dato individual 
en el informe. Para esta primera versión, región y comuna ofrecen una 
escala territorial manejable; las tablas con pocos casos también requieren 
cuidar la posibilidad de identificar personas.
Como núcleo inicial propondría: volumen temporal, perfil demográfico, 
residencia, características de los eventos, antecedentes y tratamiento de 
salud mental, y recurrencia. Estudios, trabajo y las demás características 
pueden incorporarse según su completitud y utilidad.
Para afinar esta estructura, falta precisar qué categorías contiene 
“Subclasificación” y qué representa “Comuna”: residencia, ocurrencia 
o ubicación del establecimiento notificador.

