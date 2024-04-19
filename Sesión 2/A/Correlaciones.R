install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

#N° filas
dim(mtcars)

#Características: tipo de datos
str(mtcars)

#Nombres de las variables
names(mtcars)

#Datos descriptivos 
summary (mtcars)

# Ejecutar matriz de correlaciones
M = cor(mtcars)

#Seleccionar solo las columnas numéricas
mtcars_numeric <- mtcars %>%
  select_if(is.numeric)

# Ejecutar matriz de correlaciones
M = cor(mtcars_numeric)
corrplot(M, method = "ellipse") 
corrplot(M, method = "circle")
corrplot(M, method = "square")
corrplot(M, method = "number")
corrplot(M, method = "shade")
corrplot(M, method = "color")
corrplot(M, method = "pie")