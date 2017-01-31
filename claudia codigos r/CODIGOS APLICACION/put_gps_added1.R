#Requerimientos
#- Que las casas adicionadas esten en la misma base que las casas a la que corresponden
#- Tener columnas: UNICODE, V

#-RUTAS UTILIZADAS
  ruta_1 <- '~/claudia codigos r'

#Ruta
  setwd(ruta_1)

#Leer los archivos
  attack_2006_2009 <- read.csv("~/claudia codigos r/attack_gps2006_2009.csv")
  gps_aqp<-read.csv("~/claudia codigos r/AREQUIPA_GPS_GOOGLE.csv")

#escogiendo variablesque nos interesan y cambiando los nombres a las otras columnas  
  
  varstokeep <- c("UNICODE", "P.x","D.x","L.x","V.x","FECHA", "CICLO","LATITUDE","LONGITUDE")
  attack_2006_2009 <- attack_2006_2009[, varstokeep]
 
  #changed columns names 
  setnames(attack_2006_2009,"P.x","P")
  setnames(attack_2006_2009,"D.x","D")
  setnames(attack_2006_2009,"L.x","L")
  setnames(attack_2006_2009,"V.x","V") 

           
  #Convertir a character
  attack_2006_2009$UNICODE <- as.character(attack_2006_2009$UNICODE)
  attack_2006_2009$L <- as.character(attack_2006_2009$L)
  attack_2006_2009$V <- as.character(attack_2006_2009$V)
  gps_aqp$UNICODE <- as.character(gps_aqp$UNICODE)
  gps_aqp$L <- as.character(gps_aqp$L)
  gps_aqp$V <- as.character(gps_aqp$V)

#Seleccionar solo los datos unicos
  attack_2006_2009_unicos <- attack_2006_2009[which(!duplicated(attack_2006_2009$UNICODE)),]
  data <- attack_2006_2009_unicos
  
#Agregamos una columna para almacenar la letra de las adicionales
  data$ADDED <- NA
  
#Obtener todos las viviendas adicionadas que no tienen gps
  n_row <- nrow(data)  

  for (i in 1:n_row) {
    #Solo los que no tienen dato
    if (is.na(data$LATITUDE[i])) {
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
  
#Haciendo MERGE con la base principal "gps_aqp"
  data_added <- merge(data_added, gps_aqp, by.x = "UNICODE_FALSO", by.y = "UNICODE")

  #NOTA: Por el momento al realizar el merge hay 42 viviendas que no tienen su dato correspondiente
  #en la base "gps_aqp" pero si esta en "aqp_rociado" pero no tienen GPS por eso por el momento no 
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
  write.csv(data_added,"added_aqpII.csv",row.names = FALSE)
  