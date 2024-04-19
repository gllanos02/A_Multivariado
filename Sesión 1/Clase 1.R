resultados2014 =read_delim("C:/Users/brisa/Downloads/resultados2014.csv", ",",escape_double = FALSE, trim_ws=TRUE)
head(resultados2014,5)
resultados2018 =read_delim("C:/Users/brisa/Downloads/resultados2018.csv", ",",escape_double = FALSE, trim_ws=TRUE)
head(resultados2018,5)

##n° de columnas
dim(resultados2014) 

##estructura/característica
str(resultados2014)

##datos descriptivos de las variables
summary(resultados2014)

##nombres de las variables
names(resultados2014)
names(resultados2018)

partidos_nombre = c('pase18', 'pac18', 'adc18', 'pt18', 'fa18', 'pin18',
                    'pln18', 'pml18', 'png18', 'prc18', 'prsc18', 'prn18', 'pusc18')

##Cambiar nombre
cam_nombre = function(dataframe)
{
  for (i in 1:length(partidos_nombre))
  {
    names(dataframe)[names(dataframe) ==
    paste0('votos', i)] = partidos_nombre[i]
  }
  return(dataframe)
}
resultados2018=cam_nombre(resultados2018)

##Calcular porcentaje
votos_porcentaje= function(dataframe){
  x=dataframe%>%
    group_by(codigo)%>%
  mutate_all(funs((. / votos_validos)*100))%>%
  select(-votos_validos)
  return(x)
}

por_resultados2014 = votos_porcentaje(resultados2014)
por_resultados2018 = votos_porcentaje(resultados2018)

##resultado ganador
winner = function(dataframe, periodo){
  x = dataframe%>%
    gather(partido, votos, -codigo) %>%
  group_by(codigo)%>%
  filter(votos==max(votos))%>%
  separate(partido, c(paste0("partido", periodo)),
sep="1")%>%
  select(-votos)
  return(x)
}

winner2014=winner(por_resultados2014, 14)
winner2018 =winner(por_resultados2018, 18)

##cambio en la distribución
cambio = winner2018%>%
  left_join(winner2014, by="codigo")%>%
  mutate(cambio=ifelse(partido18==partido14,"sin cambio", "cambio"),
         robo=ifelse(cambio=="cambio", paste(partido18, partido14, sep=" al "), "sin cambio"))

table(cambio$cambio)
table(cambio$robo)

##Variación de % en votos
grafico_votos = function(partido, color){
  x = por_resultados2018%>%
    select(codigo, paste0(partido,18))%>%
    left_join(
      (por_resultados2014%>%
         select(codigo, paste0(partido,14))),
      by="codigo")%>%
    gather(anio, votos, - codigo)%>%
    mutate(anio=ifelse(anio==paste0(partido,14), 2014, 2018))
  par(las=1, bty="l", family="mono", font=1, bg="transparent")
  return(
    beeswarm(votos ~ anio, data=x, col=color, pch=16, method="hex",
             cex=0.8, horizontal=TRUE, ylab="", xlab=paste("Porcentaje de
votos del", toupper(partido)),
             main=paste("Porcentaje de votos del", toupper(partido)),
             xlim=c(0, 60))
  )
}

grafico_votos("pac", "#BEf000")