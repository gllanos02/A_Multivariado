install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

Peso=c(51,59,49,54,50,55,48,53,52,57)
largo =c(33.5,38,32,37.5,31.5,33,31,36.5,34,35)

#Matriz de gráficos de dispersión
pairs(largo~Peso)

df = data.frame(Peso,largo)

#Matriz de correlación
chart.Correlation(df)

#gráfico de dispersión
pairs(largo~Peso,col="yellow")

#test de Shapiro-Wilk
shapiro.test(Peso)
shapiro.test(largo)

#--------------------------------------------
edad=c(26,18,20,19,25,22,37,56,78)
talla=c(1.56, 1.72, 1.65, 1.44, 1.69, 1.66, 1.51, 1.62, 1.42)

pairs(talla~edad,col="blue")

df = data.frame(edad,talla)
chart.Correlation(df)

shapiro.test(edad)
shapiro.test(talla)
