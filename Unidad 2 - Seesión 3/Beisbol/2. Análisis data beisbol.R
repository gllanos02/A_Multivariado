#predecir el rendimiento general de un jugador por su puntaje
#Variable dependiente: "points"
#Variables independientes: "Age", "Games", "Fields Goal", "3-points Field Goal", "2-points Field Goal", "Free Throws" 
#"Total Rebounds", "Assists", "Blocks", "Turnovers" "Personal Fouls"

# Calcula la correlación
round(cor(x = beisbol, method = "pearson"), 3)

#colinealidad por gráfico
library(psych) 
multi.hist(x = beisbol, dcol = c("purple", "red"),dlty = c("dotted", "solid"), main = "")

#gráfico de dispersión
library(GGally) 
ggpairs(beisbol, lower = list(continuous = "smooth"), diag = list(continuous = "barDiag"), axisLabels = "none")

#cuál de todas influye más. Elegir mejor predictor 
modelox = lm(beisbol$Points ~ beisbol$Age+beisbol$Games+beisbol$`Fields Goal`+beisbol$`3-points Field Goal`+beisbol$`2-points Field Goal`+
               beisbol$`Free Throws`+beisbol$`Total Rebounds`+beisbol$Assists+beisbol$Blocks+beisbol$Turnovers+beisbol$`Personal Fouls`)

step(object = modelox, direction = "both", trace=1)

library(car)
vif(modelox)

# 3d
library(rgl)
plot3d(beisbol$`Fields Goal`, beisbol$`3-points Field Goal`, beisbol$Points, pch = ".", size = 0.5)