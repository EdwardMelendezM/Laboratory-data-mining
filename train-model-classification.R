# Cargar la biblioteca de minería de datos (si no está cargada)
library(randomForest)
library(caret)
library(pROC)
# SEPARACION DE DATOS DE ENTRENAMIENTO Y PRUEBA
datos
summary(datos)
dim(datos)
class(datos)
set.seed(123)
colnames(datos)

colnames(datos) <- c("LAND_SQUARE_FEET", "TAX_CLASS_AT_TIME_OF_SALE", "COMMERCIAL_UNITS", "BLOCK",
                     "ZIP_CODE", "RESIDENTIAL_UNITS", "TOTAL_UNITS", "BUILDING_CLASS_CATEGORY",
                     "BUILDING_CLASS_AT_PRESENT", "NEIGHBORHOOD", "SALE_PRICE")

# Dividir los datos en características (X) y variable objetivo (y)
X <- datos[, -which(names(datos) == "SALE_PRICE")]
y <- datos$SALE_PRICE

# Dividir los datos en conjunto de entrenamiento y conjunto de prueba
set.seed(42)
train_indices <- sample(1:nrow(datos), nrow(datos) * 0.8)
X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

# Crear el modelo de regresión utilizando Random Forest
model <- randomForest(x = X_train, y = y_train)

# Realizar predicciones en el conjunto de prueba
predictions <- predict(model, X_test)

# Convertir las variables a tipo numérico
predictions <- as.numeric(predictions)
y_test <- as.numeric(y_test)

# Evaluar el rendimiento del modelo
rmse <- sqrt(mean((predictions - y_test)^2))
print(paste("RMSE:", rmse))

#1.12 unidades de los valores reales en el conjunto de prueba.

# VISUALIZAR LOS RESULTADOS CON UNA MATRIZ DE CONFUSION

# Crear la matriz de confusión
confusion_matrix <- confusionMatrix(data = factor(predictions, levels = c(0, 1,2)),
                                    reference = factor(y_test, levels = c(0, 1,2)))
# Visualizar la matriz de confusión como tabla
print(confusion_matrix$table)

# Visualizar la matriz de confusión como mapa de calor
heatmap(confusion_matrix$table, col = colorRampPalette(c("white", "blue","yellow"))(100))


# VISUALIZAR LOS RESULTADOS CON Gráfico de ROC

# Calcular la curva ROC
roc_curve <- roc(response = factor(y_test, levels = c(0, 1)),
                 predictor = predictions)

# Visualizar la curva ROC
plot(roc_curve, main = "Curva ROC", print.auc = TRUE)



# VISUALIZAR LOS RESULTADOS CON BARRAS DE PASTEL

# Crear un data frame con las etiquetas reales y predichas
results <- data.frame(Real = factor(y_test, levels = c(0, 1,2)),
                      Predicha = factor(predictions, levels = c(0, 1,2)))

# Contar las frecuencias de las etiquetas
freq <- table(results)

# Visualizar como gráfico de barras
barplot(freq, beside = TRUE, legend.text = rownames(freq))

# Visualizar como gráfico de pastel
pie(freq)
