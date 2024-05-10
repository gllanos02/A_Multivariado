  ### UNIVERSIDAD

round(cor(x = universidad, method = "pearson"), 3)
library(psych) 
multi.hist( x = universidad, dcol = c("purple", "red"),
                          dlty = c("dotted", "solid"), main = "")

library(GGally) 
ggpairs(universidad, lower = list(continuous =
         "smooth"), diag = list(continuous = "barDiag"), axisLabels ="none")

modelox = lm(universidad$Founded_year ~ universidad$UK_rank+universidad$World_rank+universidad$score+universidad$Minimum_IELTS_score
             +universidad$fees
             +universidad$Student_satisfaction+universidad$`Estimated_cost_of_living_per_year_(in_pounds)`+universidad$Latitude)

step(object = modelox, direction = "both", trace=1)

library(car)

vif(modelox)

library(rgl)

plot3d(universidad$Latitude, universidad$Minimum_IELTS_score, universidad$Founded_year, pch = ".", size = 0.5)

