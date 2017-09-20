#Requerimientos
#- Que las casas que esten antes y despues esten en la misma cuadra para poder aplicar esta funcion.

library(sp)
library(rgdal)
library(data.table)
#ruta_1 <- '~/github/PETM-shiny/unicode_numberMzn/'
#Para poder cambiar lat y long en primero hay que convertir las unidades a UTM,para 
#evitar errores eliminameros mientras las casas adicionadas con NA en lat y long 

#Leer los archivos
casas_rociado <- read.csv("~/PETM-shiny/unicode_numberMzn/AREQUIPA_GPS_GOOGLE/casas_rociado.csv")
casas_rociado <- casas_rociado[which(!duplicated(casas_rociado$UNICODE)),]
casas_rociado<- as.data.table(casas_rociado)
casas_rociado_SINNA <- casas_rociado[!which(is.na(casas_rociado$LATITUDE))]
casas_rociado_SINNA<-casas_rociado_SINNA[,c('UNICODE','LATITUDE','LONGITUDE')]
write.csv(casas_rociado_SINNA,'~/CLAUDIA-DATOS-/CASASROCIADO.csv')
casas_rociado_SINNA = SpatialPoints(cbind(casas_rociado_SINNA$LONGITUDE, -casas_rociado_SINNA$LATITUDE), proj4string=CRS("+proj=longlat"))
#casas_aqp<-read.csv("AREQUIPA_GPS_GOOGLE/AREQUIPA_GPS_GOOGLE.csv

# Transforming coordinate to UTM using EPSG=32719 for WGS=84, UTM Zone=19S,

# Southern Hemisphere)
casas_rociadoUTM<- spTransform(casas_rociado_SINNA, CRS("+init=epsg:32719"))
casas_rociadoUTM<-as.data.table(casas_rociadoUTM)

write.csv(casas_rociadoUTM,'~/CLAUDIA-DATOS-/CASASUTM.csv')

#AL PONER EL NUMERO DE ORDEN CREAMOS UN INDICE PARA PODER LIGAR LAS DOS TABLAS 
#leyendo los dos archivos para poder mergear y anhadir las casas sin LAT Y LONG
sprayed_houses<-read.csv('~/CLAUDIA-DATOS-/CASASROCIADO.csv')
UTM_houses<-read.csv('~/CLAUDIA-DATOS-/CASASUTM.csv')
UTM_houses<-as.data.table(UTM_houses)
sprayed_houses<-as.data.table(sprayed_houses)


houses<-merge(sprayed_houses,UTM_houses,by='X',all=TRUE)
houses<-houses[,c('UNICODE','LONGITUDE','LATITUDE','coords.x1','coords.x2')]
#UNIENDO PARA RECUPERAR LAS CASAS ADICIONADAS 
houses<-merge(casas_rociado,houses,by='UNICODE',all.x=TRUE)
houses<-as.data.table(houses)
#cambiando los nombres de los campos y ordenandolos 
houses<-houses[,c('UNICODE','LATITUDE.x','LONGITUDE.x','coords.x1','coords.x2','P_TRIAT','I_TRIAT','FECHA','CICLO','P','D','L','V','X')]
setnames(houses,'LONGITUDE.x','LONGITUDE')
setnames(houses,'LATITUDE.x','LATITUDE')

#cumpliendo la condicion se creara el nuevo valos de latitude y longitude para las casas adicionadas 

library(RMySQL)
 if
aux[,GROUP_PAR2:=ifelse(GROUP_PAR1=="CONTROL","AFICHE","CONTROL")]
merge_ExP[, ADDED_II_CICLO := ifelse(ESTA_ESTRATEGIAS== 0, 1, 0)]

houses[,lati:=if(houses$LATITUDE=='NA',houses$coords.x2+10)]



#coordenadas de casa Original: Xo, Yo
#Coordenadas de Siguiente casa en la cuadra: Xs, Ys
#Nuevas coordenadas para la casa adicionada: (Xo + Xs)/2, (Yo + Ys)/2

n_row<-nrow(houses)
for (i in 1:nrow(houses)) {
  if (is.na(houses$coords.x2)) {
   houses$coords.x2[i]<-(houses$coords.x2[i-1] + houses$coords.x2[i+1])/2
  }
}




for (i in 1:nrow(attackdata)) {
  if (attackdata$pos_sprayed[i] == 1) {
    attackdata$INSP_POSITIVA[i] <- 1
  }
}

for (i in 1:nrow(attackdata)) {
  if (attackdata$pos_sprayed[i] == 0 & attackdata$not_sprayed[i] == 0) {
    attackdata$INSP_POSITIVA[i] <- 0
  }
}

return(attackdata)
}






#Convertir a character
casas_rociado$UNICODE <- as.character(casas_rociado$UNICODE)
casas_rociado$L <- as.character(casas_rociado$L)
casas_rociado$V <- as.character(casas_rociado$V)
#casas_aqp$UNICODE <- as.character(casas_aqp$UNICODE)
#casas_aqp$L <- as.character(casas_aqp$L)
#casas_aqp$V <- as.character(casas_aqp$V)

#Seleccionar solo los datos unicos
casas_rociado_unicos <- casas_rociado[which(!duplicated(casas_rociado$UNICODE)),]
data <- casas_rociado_unicos

#Selecionando solo columnas que nos importan
data <- data[,c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")]

#Agregamos una columna para almacenar la letra de las adicionales
data$ADDED <- NA

#Obtener todos las viviendas adicionadas que no tienen gps
n_row <- nrow(data)  

for (i in 1:n_row) {
  #Solo los que no tienen dato EN LATITUDE
  if (is.na(data$V[i])) {
    #Separando UNICODE
    split_unicode <- unlist(strsplit(data$UNICODE[i], ".", fixed = TRUE))
    
    letter_number <- regexpr("[A-Z]", split_unicode[4])
    #Pregunta si el segundo argumento es numero
    if (  letter_number[1]!=-1 ) {
      data$P[i] <- split_unicode[1]
      data$D[i] <- split_unicode[2]
      data$L[i] <- split_unicode[3]
      viv <- gsub("([A-Z])", "",split_unicode[4])
      
      num1<-nchar(viv)
      num2<-nchar(split_unicode[4])
      
      data$V[i] <- substring(split_unicode[4], 1, num1)
      data$ADDED[i] <- substring(split_unicode[4], num2)
    }
  }
}









#Buscamos todas las adicionadas, basandonos en la columna ADDED
data_added <- data[!is.na(data$ADDED),]
#Agregando un campo de UNICODE_FALSO
data_added$UNICODE_FALSO <- NA

n_row<-nrow(data_added) 
for (i in 1:n_row) {
  #Creamos un unicode falso para poder obtener los GPS
  data_added$UNICODE_FALSO[i] <- paste(data_added$P[i],data_added$D[i],data_added$L[i],data_added$V[i], sep = ".")
}
#Elimino campos que me causaran inconveniente al hacer el MERGE
data_added<- data_added[,c("UNICODE","ADDED","UNICODE_FALSO")]

#Haciendo MERGE con la base principal "casas_aqp"
data_added <- merge(data_added, casas_aqp, by.x = "UNICODE_FALSO", by.y = "UNICODE")
#NOTA: Por el momento al realizar el merge hay 42 viviendas que no tienen su dato correspondiente
#en la base "casas_aqp" pero si esta en "aqp_rociado" pero no tienen GPS por eso por el momento no 
#lo tomaremos en cuenta, para un futuro si desean tener todas las adicionadas tienen que solucionar
#esos inconvenientes poniendoles GPS

#Eliminar la columna UNICODE_FALSO
data_added <- data_added[,-1]
#Contruyendo el numero de vivienda que corresponde realmente "V + ADDED"
n_row<-nrow(data_added) 
for (i in 1:n_row) {
  #Creamos un unicode falso para poder obtener los GPS
  data_added$V[i] <- paste(data_added$V[i], data_added$ADDED[i] ,sep = "")
}
#Eliminando la columna ADDED
data_added<- data_added[,-2]

#--------------------------------------------------------------------  
#Imprimiendo Resultados
#--------------------------------------------------------------------

#Resultado de las viviendas adicionadas
write.csv(data_added,"AREQUIPA_GPS_GOOGLE/added_aqpI.csv",row.names = FALSE)