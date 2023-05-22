# Cargar la biblioteca de minería de datos (si no está cargada)
library(caret)
library(randomForest)
library(e1071)
library(gbm)

# SEPARACION DE DATOS DE ENTRENAMIENTO Y PRUEBA

# Definir una proporción para dividir los datos (por ejemplo, 70% para entrenamiento y 30% para prueba)
proporcion_entrenamiento <- 0.7

# Crear un índice aleatorio para la división de datos
set.seed(42)  # Establecer una semilla para reproducibilidad
indice_entrenamiento <- createDataPartition(datos$`SALE PRICE`, p = proporcion_entrenamiento, list = FALSE)
 
# Crear los conjuntos de datos de entrenamiento y prueba
datos_entrenamiento <- datos[indice_entrenamiento, ]
datos_prueba <- datos[-indice_entrenamiento, ]


# ENTRENAMIENTO DE MODELOS DE CLASIFICACION

# Modelos utilizados
modelos <- c("Bosques Aleatorios" = "rf",
             "Máquinas de Vectores de Soporte" = "svmRadial",
             "Gradient Boosting" = "gbm")

# Crear una lista para almacenar las predicciones y precisión de cada modelo
resultados <- list()

# Entrenar los modelos y realizar predicciones en el conjunto de prueba
for (modelo_nombre in names(modelos)) {
  modelo <- modelos[modelo_nombre]
  
  # Entrenar el modelo
  if (modelo == "rf") {
    modelo_entrenado <- randomForest(`SALE PRICE` ~ ., data = datos_entrenamiento)
  } else if (modelo == "svmRadial") {
    modelo_entrenado <- svm(`SALE PRICE` ~ ., data = datos_entrenamiento, kernel = "radial")
  } else if (modelo == "gbm") {
    modelo_entrenado <- gbm(`SALE PRICE` ~ ., data = datos_entrenamiento, n.trees = 100, shrinkage = 0.1)
  }
  
  # Realizar predicciones en el conjunto de prueba
  predicciones <- predict(modelo_entrenado, newdata = datos_prueba)
  
  # Calcular la matriz de confusión
  matriz_confusion <- table(Real = datos_prueba$`SALE PRICE`, Prediccion = predicciones)
  
  # Calcular la precisión del modelo
  precision <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
  
  # Almacenar los resultados en la lista
  resultados[[modelo_nombre]] <- list(modelo_entrenado = modelo_entrenado,
                                      predicciones = predicciones,
                                      matriz_confusion = matriz_confusion,
                                      precision = precision)
}

# Imprimir los resultados
for (modelo_nombre in names(resultados)) {
  modelo_resultado <- resultados[[modelo_nombre]]
  
  cat("Modelo:", modelo_nombre, "\n")
  cat("Matriz de Confusión:\n")
  print(modelo_resultado$matriz_confusion)
  cat("Precisión:", modelo_resultado$precision, "\n\n")
}

