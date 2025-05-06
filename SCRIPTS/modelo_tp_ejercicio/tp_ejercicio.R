# tp_ejercicio.R
# Funciones y código auxiliar para el TP

# Cargar paquetes necesarios
library(tidyverse)

# Función de ejemplo
cargar_datos <- function(ruta) {
  read_csv(ruta)
}

# Otro ejemplo
resumen_variables <- function(df) {
  summary(df)
}
