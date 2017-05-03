#Requerimientos
#- Que las casas adicionadas esten en la misma base que las casas a la que corresponden
#- Tener columnas: UNICODE, V

#-RUTAS UTILIZADAS
ruta_1 <- '~/github/PETM-shiny/unicode_numberMzn/'

#Ruta
setwd(ruta_1)

#Leer los archivos
casas_rociado <- read.csv("AREQUIPA_GPS_GOOGLE/casas_rociado.csv")
casas_aqp<-read.csv("AREQUIPA_GPS_GOOGLE/AREQUIPA_GPS_GOOGLE.csv")

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
  #Solo los que no tienen dato
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