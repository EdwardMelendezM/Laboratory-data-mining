library(tidyverse)
library(GGally)
#-----------------------------------------------------------
# 1.1 CONJUNTO DE DATOS

# ANALISIS DE COLUMNAS

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


# EXPLORACION DE DATOS
dataset <- read_csv2("dataset/rollingsales_bronx.csv")

#Mostara 3 filas
print(dataset, n = 6, width = Inf)

#-----------------------------------------------------------
# 1.2 CALIDAD DE DATOS

# Resumen de datos
class(dataset)
dim(dataset)
summary(dataset)
colnames(dataset)

# Remover comas y convertir a numérico el tipo SALE PRICE
sale_price <- dataset$`SALE PRICE`
cleaned_sale_price <- as.numeric(gsub(",", ".", sale_price))
dataset$`SALE PRICE`<- cleaned_sale_price

# Calcular la cantidad de valores NA en cada columna
cantidad_nulos <- colSums(is.na(dataset))
cantidad_nulos

#Algunas columnas tienen casi en su totalidad valores nulos, por lo tanto las eliminamos
columnas_a_eliminar <- c("EASEMENT", "APARTMENT NUMBER","BOROUGH")
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
dataset <- dataset[, !(names(dataset) %in% columnas_a_eliminar)]
dim(dataset)

#Eliminamos los valores que son inusuales o absurdos en SALE PRICE
dataset <- dataset[dataset$`SALE PRICE` > 100, ]

#Actualizar valores numericos
columnas_numericas <- names(dataset)[sapply(dataset, is.numeric)]
columnas_NO_numericas <- names(dataset)[!sapply(dataset, is.numeric)]
#-----------------------------------------------------------
# 1.3 AGREGACION
# No es necesario hacer la agregacion



#-----------------------------------------------------------
# 1.4 MUESTREO - ALEATORIO
take <- sample(seq(nrow(dataset)), size = 100)
muestraDataset<-dataset[take, ]
muestraDataset


#-----------------------------------------------------------
# 1.5 ESCALADO MULTIDIMENSIONAL
numeric_columns <- sapply(muestraDataset, is.numeric)
selected_columns <- names(muestraDataset)[numeric_columns]
selected_columns <- selected_columns[!selected_columns %in% "SALE PRICE"]
d <- muestraDataset %>% select(all_of(selected_columns)) %>% dist()
fit <- cmdscale(d, k = 2)
colnames(fit) <- c("comp1", "comp2")
fit <- as_tibble(fit) %>% add_column("SALE PRICE" = muestraDataset$"SALE PRICE")
ggplot(fit, aes(x = comp1, y = comp2)) + geom_point()
#---------------------------------------------------------------------
# DISCRETIZACION DE CARACTERISTICAS
library(Matrix)
library(arules)
columnas_numericas
ggplot(muestraDataset, aes(x = `LAND SQUARE FEET` )) + geom_histogram(binwidth = .2)
ggplot(muestraDataset, aes(x = `GROSS SQUARE FEET` )) + geom_histogram(binwidth = .2)
ggplot(muestraDataset, aes(x = `YEAR BUILT` )) + geom_histogram(binwidth = .2)
#---------------------------------------------------------------------
# ESTANDARIZACION DE DATOS
scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))
muestraDataset <- muestraDataset %>% scale_numeric()
summary(muestraDataset)
#---------------------------------------------------------------------
# PROXIMIDADES Y DISTANCIAS
people <- muestraDataset %>% mutate_if(is.character, factor)
people
#---------------------------------------------------------------------
# VER MATRIZ DE CORRELACION
cc <- muestraDataset %>% select(-columnas_NO_numericas) %>% cor()

ggplot(muestraDataset, aes(`RESIDENTIAL UNITS`, `TOTAL UNITS`)) + 
  geom_point() +
  geom_smooth(method = "lm")
#---------------------------------------------------------------------
# ESTIMACION DE DENSIDAD
library(tidyverse)

#Dos dimensiones
ggplot(muestraDataset, aes(`RESIDENTIAL UNITS`, `TOTAL UNITS`)) +
  geom_density_2d_filled() +
  geom_jitter()
#---------------------------------------------------------------------
# MATRIZ DE CORRELACION
#Seleccionamos todos los datos menos species, lo hacemos matriz
# y por ultimo hallamos la correlacion
library(ggcorrplot)
library(seriation)
cm1 <- muestraDataset %>% select(-columnas_NO_numericas) %>% as.matrix %>% cor()
ggcorrplot(cm1)

#Vemos otro
gghmap(cm1, prop = TRUE)


#---------------------------------------------------------------------
# CONVERSION DE DATOS CATEGORICOS A NUMERICOS CON FACTORES
# Seleccionar las columnas categóricas que se convertirán en factores
muestraDataset[columnas_NO_numericas] <- lapply(muestraDataset[columnas_NO_numericas], factor)

# Convertir los factores a variables numericas
muestraDataset[columnas_NO_numericas] <- lapply(muestraDataset[columnas_NO_numericas], as.numeric)

# Imprimir el resultado
muestraDataset

