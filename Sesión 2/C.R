weight=births$weight
births$fumanofuma <- ifelse(births$smoke == "smoker", 1, 0)

fnf=births$fumanofuma

mostrar=t.test(fnf,weight)
print(mostrar) #Hay diferencia



weight=births$weight
births$sex <- ifelse(births$sex_baby == "male", 1, 0)

sex0=births$sex

mostrar=t.test(sex0,weight)
print(mostrar)#Hay diferencia 
