
# INICIO FASE DE VISUALIZACIÓN

# Librerías Requeridas ----

library(tidyverse)   # version 1.2.1
library(readxl)      # version 1.0.0
library(DT)          # version 0.4
library(highcharter) # version 0.5.0.9999


# Importar Scripts ----

source("R/Importar Programas.R", encoding = 'UTF-8')

source("R/Funciones.R", encoding = 'UTF-8')

# Funciones ----

# Exportar archivos HTML


Salvar <- function(objeto, ruta, nombre){
  saveWidget(objeto,
             file = file.path(str_sub(getwd(), 1, str_length(getwd())-12),
                              ruta,
                              nombre),
             selfcontained = F, libdir = "libraryjs")
  
}


# Programas ---- 

# Gra1100 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "Gra1100") %>% select(-(Nivel))


# Evolución histórica total de Programas ---

col <-   c("#0071bc") # Azul vivo, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes Programas", eje = "Número de Programas (k: miles)");EVOLUCION_SERIE


# Modalidad de formación ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado

TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes Programas por modalidad de formación', mensaje = "Número de estudiantes Programas por modalidad de formación", titulo = "Programas por modalidad de formación");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes Programas por modalidad de formación", eje = "Número de Programas  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col, titulo = "Distribución de Programas por modalidad de formación", etiqueta = "Número de Programas", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#fbb03b", # amarillo, Especialidades médicas
            "#29abe2", # azul claro, Especialización
            "#c1272d",  # rojo, Maestría
            "#8cc63f")  # verde, Pregrado
            

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes Programas por nivel de formación', mensaje = "Número de estudiantes Programas por nivel de formación", titulo = "Programas por nivel de formación");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes Programas por nivel de formación", eje = "Número de Programas");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de Programas por modalidad de formación", eje = "Número de Programas"); NIVEL_ACTUAL


# Sedes  ---


col <-   c( "#29abe2", # azul claro, Amazonía
            "#8cc63f", # verde, Bogotá
            "#c1272d", # rojo, Caribe 
            "#9e9ac8",  # Morado claro, De la Paz
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellín
            "#fbb03b", # amarillo, Orinoquía 
            "#93278f", # Morado, Palmira
            "#6d6666"  # gris, Tumaco 
) 

SEDE_TABLA <- tabla(datos = Consolidado, categoria = "SEDE_NOMBRE_ADM", variable = 'Total estudiantes Programas por sede de graduación', mensaje = "Total de estudiantes por sede de graduación", titulo = "Sede estudiantes Programas");SEDE_TABLA
SEDE_SERIE <- series(datos = Consolidado, categoria = "SEDE_NOMBRE_ADM", colores = col, titulo = "Evolución del número de Programas por sede", eje = "Número de Programas (k: miles)");SEDE_SERIE
SEDE_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "SEDE_NOMBRE_ADM", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de Programas por sedes de la Universidad", eje = "Número de Programas"); SEDE_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes Programas según nacionalidad', mensaje = "Número de estudiantes Programas por nacionalidad", titulo = "Programas según nacionalidad");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de Programas según nacionalidad", eje = "Número de Programas (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de Programas según nacionalidad", etiqueta = "Número de Programas", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total Programas por áreas del conocimiento SNIES', mensaje = "Total de Programas por áreas del conocimiento SNIES", titulo = "Sede Programas por áreas del conocimiento SNIES");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de Programas por áreas del conocimiento SNIES", eje = "Número de Programas (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de Programas por áreas del conocimiento SNIES", eje = "Número de Programas"); AREAC_SNIES_ACTUAL

# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Programas/Nal/Programas", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Programas/Nal/Programas", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Programas/Nal/Programas", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Programas/Nal/Programas", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Programas/Nal/Programas", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Programas/Nal/Programas", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Programas/Nal/Programas", "A_nivel.html")
Salvar(SEDE_TABLA, "G_Programas/Nal/Programas", "T_sede.html")
Salvar(SEDE_SERIE, "G_Programas/Nal/Programas", "S_sede.html")
Salvar(SEDE_ACTUAL, "G_Programas/Nal/Programas", "A_sede.html")
Salvar(NACIONALIDAD_TABLA, "G_Programas/Nal/Programas", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Programas/Nal/Programas", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Programas/Nal/Programas", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Programas/Nal/Programas", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Programas/Nal/Programas", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Programas/Nal/Programas", "A_sexo.html")
Salvar(AREAC_SNIES_TABLA, "G_Programas/Nal/Programas", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Programas/Nal/Programas", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Programas/Nal/Programas", "A_snies.html")


# Gra1101 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "Gra1101") %>% select(-(Nivel))


# Evolución histórica total de Programas ---

col <-   c("#8cc63f") # verde, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes Programas - sede Bogotá", eje = "Número de Programas (k: miles)");EVOLUCION_SERIE


# Modalidad de formación ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado

TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes Programas por modalidad de formación - sede Bogotá', mensaje = "Número de estudiantes Programas por modalidad de formación - sede Bogotá", titulo = "Programas por modalidad de formación - sede Bogotá");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes Programas por modalidad de formación - sede Bogotá", eje = "Número de Programas  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col, titulo = "Distribución de Programas por modalidad de formación - sede Bogotá", etiqueta = "Número de Programas", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#fbb03b", # amarillo, Especialidades médicas
            "#29abe2", # azul claro, Especialización
            "#c1272d",  # rojo, Maestría
            "#8cc63f")  # verde, Pregrado


NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes Programas por nivel de formación - sede Bogotá', mensaje = "Número de estudiantes Programas por nivel de formación - sede Bogotá", titulo = "Programas por nivel de formación - sede Bogotá");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes Programas por nivel de formación - sede Bogotá", eje = "Número de Programas");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de Programas por modalidad de formación - sede Bogotá", eje = "Número de Programas"); NIVEL_ACTUAL


# Facultad  ---


col <-   c( "#9e9ac8",  # Morado claro, Enfermería
            "#0071bc", # azul vivo, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias económicas
            "#f15a24", # naranja, Ciencias humanas
            "#fbb03b", # amarillo, Derecho, ciencias políticas y sociales
            "#93278f", # Morado, Artes 
            "#8cc63f", # verde, Ingeniería
            "#bdbdbd",  # Gris claro, Medicina 
            "#c1272d", # rojo, Medicina veterianaria y zootecnia
            "#99d8c9")  # Agua Marina, Odontología 
            


FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes Programas por facultad - sede Bogotá', mensaje = "Total de estudiantes Programas por facultad - sede Bogotá", titulo = "Facultad estudiantes Programas - sede Bogotá");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de Programas por facultad - sede Bogotá", eje = "Número de Programas (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de Programas por facultad - sede Bogotá", eje = "Número de Programas"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes Programas según nacionalidad - sede Bogotá', mensaje = "Número de estudiantes Programas por nacionalidad - sede Bogotá", titulo = "Programas según nacionalidad - sede Bogotá");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de Programas según nacionalidad - sede Bogotá", eje = "Número de Programas (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de Programas según nacionalidad - sede Bogotá", etiqueta = "Número de Programas", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales   


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total Programas por áreas del conocimiento SNIES - sede Bogotá', mensaje = "Total de Programas por áreas del conocimiento SNIES - sede Bogotá", titulo = "Sede Programas por áreas del conocimiento SNIES - sede Bogotá");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de Programas por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de Programas (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de Programas por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de Programas"); AREAC_SNIES_ACTUAL

# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Programas/Bog/Programas", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Programas/Bog/Programas", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Programas/Bog/Programas", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Programas/Bog/Programas", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Programas/Bog/Programas", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Programas/Bog/Programas", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Programas/Bog/Programas", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Programas/Bog/Programas", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Programas/Bog/Programas", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Programas/Bog/Programas", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Programas/Bog/Programas", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Programas/Bog/Programas", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Programas/Bog/Programas", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Programas/Bog/Programas", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Programas/Bog/Programas", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Programas/Bog/Programas", "A_sexo.html")
Salvar(AREAC_SNIES_TABLA, "G_Programas/Bog/Programas", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Programas/Bog/Programas", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Programas/Bog/Programas", "A_snies.html")


# Gra1102 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "Gra1102") %>% select(-(Nivel))


# Evolución histórica total de Programas ---

col <-   c("#f15a24") # Naranja, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes Programas - sede Medellín", eje = "Número de Programas (k: miles)");EVOLUCION_SERIE


# Modalidad de formación ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado

TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes Programas por modalidad de formación - sede Medellín', mensaje = "Número de estudiantes Programas por modalidad de formación - sede Medellín", titulo = "Programas por modalidad de formación - sede Medellín");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes Programas por modalidad de formación - sede Medellín", eje = "Número de Programas  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col, titulo = "Distribución de Programas por modalidad de formación - sede Medellín", etiqueta = "Número de Programas", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d",  # rojo, Maestría
            "#8cc63f")  # verde, Pregrado


NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes Programas por nivel de formación - sede Medellín', mensaje = "Número de estudiantes Programas por nivel de formación - sede Medellín", titulo = "Programas por nivel de formación - sede Medellín");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes Programas por nivel de formación - sede Medellín", eje = "Número de Programas");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de Programas por modalidad de formación - sede Medellín", eje = "Número de Programas"); NIVEL_ACTUAL


# Facultad  ---


col <-   c( "#9e9ac8",  # Morado claro, Arquitectura
            "#f15a24", # naranja, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias humanas y económicas 
            "#8cc63f" # verde, Minas
)


FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes Programas por facultad - sede Medellín', mensaje = "Total de estudiantes Programas por facultad - sede Medellín", titulo = "Facultad estudiantes Programas - sede Medellín");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de Programas por facultad - sede Medellín", eje = "Número de Programas (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de Programas por facultad - sede Medellín", eje = "Número de Programas"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes Programas según nacionalidad - sede Medellín', mensaje = "Número de estudiantes Programas por nacionalidad - sede Medellín", titulo = "Programas según nacionalidad - sede Medellín");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de Programas según nacionalidad - sede Medellín", eje = "Número de Programas (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de Programas según nacionalidad - sede Medellín", etiqueta = "Número de Programas", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total Programas por áreas del conocimiento SNIES - sede Medellín', mensaje = "Total de Programas por áreas del conocimiento SNIES - sede Medellín", titulo = "Sede Programas por áreas del conocimiento SNIES - sede Medellín");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de Programas por áreas del conocimiento SNIES - sede Medellín", eje = "Número de Programas (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de Programas por áreas del conocimiento SNIES - sede Medellín", eje = "Número de Programas"); AREAC_SNIES_ACTUAL

# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Programas/Med/Programas", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Programas/Med/Programas", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Programas/Med/Programas", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Programas/Med/Programas", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Programas/Med/Programas", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Programas/Med/Programas", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Programas/Med/Programas", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Programas/Med/Programas", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Programas/Med/Programas", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Programas/Med/Programas", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Programas/Med/Programas", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Programas/Med/Programas", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Programas/Med/Programas", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Programas/Med/Programas", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Programas/Med/Programas", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Programas/Med/Programas", "A_sexo.html")
Salvar(AREAC_SNIES_TABLA, "G_Programas/Med/Programas", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Programas/Med/Programas", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Programas/Med/Programas", "A_snies.html")


# Gra1103 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "Gra1103") %>% select(-(Nivel))


# Evolución histórica total de Programas ---


col <-   c("#0071bc") # Azul vivo, Total 


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes Programas - sede Manizales", eje = "Número de Programas (k: miles)");EVOLUCION_SERIE


# Modalidad de formación ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado

TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes Programas por modalidad de formación - sede Manizales', mensaje = "Número de estudiantes Programas por modalidad de formación - sede Manizales", titulo = "Programas por modalidad de formación - sede Manizales");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes Programas por modalidad de formación - sede Manizales", eje = "Número de Programas  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col, titulo = "Distribución de Programas por modalidad de formación - sede Manizales", etiqueta = "Número de Programas", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d",  # rojo, Maestría
            "#8cc63f")  # verde, Pregrado


NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes Programas por nivel de formación - sede Manizales', mensaje = "Número de estudiantes Programas por nivel de formación - sede Manizales", titulo = "Programas por nivel de formación - sede Manizales");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes Programas por nivel de formación - sede Manizales", eje = "Número de Programas");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de Programas por modalidad de formación - sede Manizales", eje = "Número de Programas"); NIVEL_ACTUAL


# Facultad  ---


col <-   c( "#f15a24", # naranja, Administración
            "#0071bc", # azul vivo, Ciencias exactas y naturales
            "#8cc63f" # verde, Ingeniaría y arquitectura
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes Programas por facultad - sede Manizales', mensaje = "Total de estudiantes Programas por facultad - sede Manizales", titulo = "Facultad estudiantes Programas - sede Manizales");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de Programas por facultad - sede Manizales", eje = "Número de Programas (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de Programas por facultad - sede Manizales", eje = "Número de Programas"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes Programas según nacionalidad - sede Manizales', mensaje = "Número de estudiantes Programas por nacionalidad - sede Manizales", titulo = "Programas según nacionalidad - sede Manizales");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de Programas según nacionalidad - sede Manizales", eje = "Número de Programas (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de Programas según nacionalidad - sede Manizales", etiqueta = "Número de Programas", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total Programas por áreas del conocimiento SNIES - sede Manizales', mensaje = "Total de Programas por áreas del conocimiento SNIES - sede Manizales", titulo = "Sede Programas por áreas del conocimiento SNIES - sede Manizales");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de Programas por áreas del conocimiento SNIES - sede Manizales", eje = "Número de Programas (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de Programas por áreas del conocimiento SNIES - sede Manizales", eje = "Número de Programas"); AREAC_SNIES_ACTUAL

# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Programas/Man/Programas", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Programas/Man/Programas", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Programas/Man/Programas", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Programas/Man/Programas", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Programas/Man/Programas", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Programas/Man/Programas", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Programas/Man/Programas", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Programas/Man/Programas", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Programas/Man/Programas", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Programas/Man/Programas", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Programas/Man/Programas", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Programas/Man/Programas", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Programas/Man/Programas", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Programas/Man/Programas", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Programas/Man/Programas", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Programas/Man/Programas", "A_sexo.html")
Salvar(AREAC_SNIES_TABLA, "G_Programas/Man/Programas", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Programas/Man/Programas", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Programas/Man/Programas", "A_snies.html")


# Gra1104 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "Gra1104") %>% select(-(Nivel))


# Evolución histórica total de Programas ---


col <-   c("#93278f") # Morado, Total


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes Programas - sede Palmira", eje = "Número de Programas (k: miles)");EVOLUCION_SERIE


# Modalidad de formación ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado

TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes Programas por modalidad de formación - sede Palmira', mensaje = "Número de estudiantes Programas por modalidad de formación - sede Palmira", titulo = "Programas por modalidad de formación - sede Palmira");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes Programas por modalidad de formación - sede Palmira", eje = "Número de Programas  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col, titulo = "Distribución de Programas por modalidad de formación - sede Palmira", etiqueta = "Número de Programas", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d",  # rojo, Maestría
            "#8cc63f")  # verde, Pregrado


NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes Programas por nivel de formación - sede Palmira', mensaje = "Número de estudiantes Programas por nivel de formación - sede Palmira", titulo = "Programas por nivel de formación - sede Palmira");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes Programas por nivel de formación - sede Palmira", eje = "Número de Programas");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de Programas por modalidad de formación - sede Palmira", eje = "Número de Programas"); NIVEL_ACTUAL


# Facultad  ---


col <-   c( "#0071bc", # azul vivo, Ciencias agropecuarias 
            "#8cc63f" # verde, Ingeniería y Administración
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes Programas por facultad - sede Palmira', mensaje = "Total de estudiantes Programas por facultad - sede Palmira", titulo = "Facultad estudiantes Programas - sede Palmira");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de Programas por facultad - sede Palmira", eje = "Número de Programas (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de Programas por facultad - sede Palmira", eje = "Número de Programas"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes Programas según nacionalidad - sede Palmira', mensaje = "Número de estudiantes Programas por nacionalidad - sede Palmira", titulo = "Programas según nacionalidad - sede Palmira");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de Programas según nacionalidad - sede Palmira", eje = "Número de Programas (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de Programas según nacionalidad - sede Palmira", etiqueta = "Número de Programas", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total Programas por áreas del conocimiento SNIES - sede Palmira', mensaje = "Total de Programas por áreas del conocimiento SNIES - sede Palmira", titulo = "Sede Programas por áreas del conocimiento SNIES - sede Palmira");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de Programas por áreas del conocimiento SNIES - sede Palmira", eje = "Número de Programas (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de Programas por áreas del conocimiento SNIES - sede Palmira", eje = "Número de Programas"); AREAC_SNIES_ACTUAL

# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Programas/Pal/Programas", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Programas/Pal/Programas", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Programas/Pal/Programas", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Programas/Pal/Programas", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Programas/Pal/Programas", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Programas/Pal/Programas", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Programas/Pal/Programas", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Programas/Pal/Programas", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Programas/Pal/Programas", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Programas/Pal/Programas", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Programas/Pal/Programas", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Programas/Pal/Programas", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Programas/Pal/Programas", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Programas/Pal/Programas", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Programas/Pal/Programas", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Programas/Pal/Programas", "A_sexo.html")
Salvar(AREAC_SNIES_TABLA, "G_Programas/Pal/Programas", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Programas/Pal/Programas", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Programas/Pal/Programas", "A_snies.html")


