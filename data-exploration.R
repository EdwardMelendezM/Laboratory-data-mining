#LIBRERIAS
library(readr)
library(tidyverse)
library(GGally)

# EXPLORACION DE DATOS

dataset <- read_csv2("dataset/rollingsales_bronx.csv")

dim(dataset)
summary(dataset)

#Exploracion de columnas
colnames(dataset)

# "BOROUGH": El código de borough de la propiedad.
# "NEIGHBORHOOD": El nombre del vecindario de la propiedad.
# "BUILDING CLASS CATEGORY": La categoría de clase de construcción de la propiedad.
# "TAX CLASS AT PRESENT": La clase de impuesto actual de la propiedad.
# "BLOCK": El número de bloque de la propiedad.
# "LOT": El número de lote de la propiedad.
# "BUILDING CLASS AT PRESENT": La clase de construcción actual de la propiedad.
# "ADDRESS": La dirección de la propiedad.
# "ZIP CODE": El código postal de la propiedad.
# "RESIDENTIAL UNITS": El número de unidades residenciales en la propiedad.
# "COMMERCIAL UNITS": El número de unidades comerciales en la propiedad.
# "TOTAL UNITS": El número total de unidades en la propiedad.
# "LAND SQUARE FEET": El tamaño del terreno en pies cuadrados.
# "GROSS SQUARE FEET": El tamaño bruto en pies cuadrados.
# "YEAR BUILT": El año de construcción de la propiedad.
# "TAX CLASS AT TIME OF SALE": La clase de impuesto al momento de la venta de la propiedad.
# "BUILDING CLASS AT TIME OF SALE": La clase de construcción al momento de la venta de la propiedad.
# "SALE PRICE": El precio de venta de la propiedad.
# "SALE DATE": La fecha de venta de la propiedad.

# Nuestra variable DEPENDIENTE sera SALE "SALE PRICE" por que queremos saber si la propiedad
# que tenemos valdra cierto promedio de venta

sale_price <- dataset$`SALE PRICE`
sale_price
class(sale_price)

# Remover comas y convertir a numérico
cleaned_sale_price <- as.numeric(gsub(",", ".", sale_price))
dataset$`SALE PRICE`<- cleaned_sale_price
dataset


# LIMPIEZA DE DATOS


# Calcular la cantidad de valores NA en cada columna
cantidad_nulos <- colSums(is.na(dataset))
cantidad_nulos

#Algunas columnas tienen casi en su totalidad valores nulos, por lo tanto las eliminamos
columnas_a_eliminar <- c("EASEMENT", "APARTMENT NUMBER")
dataset <- dataset[, !(names(dataset) %in% columnas_a_eliminar)]

#Verificamos nuestro dataset
cantidad_nulos <- colSums(is.na(dataset))
cantidad_nulos

#Ahora vemos algunas columnas con varios valores NA pero en menos cantidad.
#Para esto vamos a reemplazarlos con el promedio. Antes verifiquemos cual es el
#promedio antes de reemplazar

# Obtener los nombres de las columnas numéricas
columnas_numericas <- names(dataset)[sapply(dataset, is.numeric)]

# Recorrer las columnas numéricas y reemplazar los valores NA por el promedio
for (col in columnas_numericas) {
  promedio <- mean(dataset[[col]], na.rm = TRUE)
  dataset[[col]][is.na(dataset[[col]])] <- promedio
}
cantidad_nulos_despues <- colSums(is.na(dataset))
cantidad_nulos_despues


#Hallar la media y la desviacion estandar
isNumericDataset<-dataset %>% summarize_if(is.numeric, mean)
isNumericDataset

#Limpiar filas con datos NA y duplicados otra vez
clean.data <- dataset %>% drop_na() %>% unique()
clean.data
dim(clean.data)


# VERIFICAR LOS DATOS CATEGORICOS Y ELIMINA AQUELLOS QUE TIENEN DATOS UNICOS
# Crear una lista para almacenar los resultados
resultados <- list()

# Recorrer las columnas y calcular los resultados
for (col in names(clean.data)) {
  num_filas <- nrow(clean.data)  # número total de filas en el dataset
  num_valores_unicos <- length(unique(clean.data[[col]]))  # cantidad de valores únicos en la columna
  
  proporcion_valores_unicos <- num_valores_unicos / num_filas
  
  if (proporcion_valores_unicos > 0.9) {
    resultado <- "---DATOS UNICOS"
  } else {
    resultado <- "->Datos distintos"
  }
  
  resultados[[col]] <- resultado
}

# Mostrar el dataframe resultante
resultados


# ELIMINAR COLUMNA CON DATOS UNICOS
columnas_a_eliminar <- c("ADDRESS")
clean.data <- clean.data[, !(names(clean.data) %in% columnas_a_eliminar)]
dim(clean.data)


# VER LAS GRAFICAS PARA SELECCIONAR NUESTRAS COLUMNAS PARA EL ENTRENAMIENTO

