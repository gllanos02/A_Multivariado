install.packages("psych")
install.packages("GGally")
install.packages("car")
install.packages("rgl")
install.packages("ggplot2")
install.packages("tibble")
install.packages("cli")




round(cor(x = bookstore, method = "pearson"), 3)

library(psych) 
multi.hist(x = bookstore, dcol = c("red", "green"),
           dlty = c("dotted", "solid"), main = "")

library(GGally)
library(ggplot2)
ggpairs(bookstore, lower = list(continuous =
                                  "smooth"), diag = list(continuous = "barDiag"), 
        axisLabels = "none")

#price indep
modelox = lm(bookstore$price ~ bookstore$pages+bookstore$reviews+bookstore$n_reviews
             +bookstore$star5+bookstore$star4+bookstore$star3+bookstore$star2+bookstore$star1
             +bookstore$weight)

step(object = modelox, direction = "both", trace=1)

library(car)

vif(modelox)

#Gráfica en 3D
library(rgl)

plot3d(bookstore$price, bookstore$star5,bookstore$star1, pch = ".", size = 0.5)