#We are going to extract the adittional houses  with from consolidad_2011_2015 and then join 
#with AQP_GPS_GOGLE and get the most of houses to Arequipa. 

#ubicando la ruta dond esta la funcion   
setwd("C:/Users/Rodrigo/Documents/claudia codigos r")
source("funcionadcoord.R")

#Aplicando la funcion para  añadir las coordenadas a las casas adicionadas  
data<-read.csv("C:/Users/Rodrigo/Documents/claudia codigos r/casas_rociado.csv")
aux <- AdCoord(data)

#ubicando los duplicados creados por la funcion  
duplicados<-aux[duplicated(aux$UNICODE,fromLast = TRUE),]

#eliminado duplicados creados por la funcion 
aux<- aux[!duplicated(aux$UNICODE),]

#RENOMBRANDO LA BASE 
ADIC_AQP_GOGLE_GPS<-(aux)


# Obtendiendo  un subset del rociado I Y II de las  casas auxiliares y 
# y sus gps para unirla con AQP_GPS_GOGLE
ADIC_AQP_GOGLE_GPS$UNICODE<-as.character(ADIC_AQP_GOGLE_GPS$UNICODE)
ADIC_AQP_GOGLE_GPS<-ADIC_AQP_GOGLE_GPS[grepl("[A-Z]",ADIC_AQP_GOGLE_GPS$UNICODE),]

#escogiendo las variables que nos interesan 
varstokeep<-c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")

ADIC_AQP_GOGLE_GPS<-ADIC_AQP_GOGLE_GPS[,varstokeep]

write.csv(ADIC_AQP_GOGLE_GPS,"ADIC_AQP_GOGLE_GPS.csv",row.names = FALSE)
