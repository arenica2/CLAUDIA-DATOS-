#Requerimientos
#- Que las casas adicionadas esten en la misma base que las casas a la que corresponden
#- Tener columnas: UNICODE, V

#-RUTAS UTILIZADAS
  ruta_1 <- '~/CLAUDIA-DATOS-/claudia codigos r'

#Ruta
  setwd(ruta_1)

library(data.table)  
  
#Leer los archivos
  attack_2006_2015 <- read.csv("~/CLAUDIA-DATOS-/claudia codigos r/ATTACK_2006_2015/attack_2006_2015.csv")
  gps_aqp<-read.csv("~/claudia codigos r/AREQUIPA_GPS_GOOGLE.csv")
  añadidas_2006_2015<-read.csv("~/CLAUDIA-DATOS-/claudia codigos r/added_aqp_2006_2015.csv")

aqp_completo<-rbind(gps_aqp,añadidas_2006_2015)
  
#haciendo el merge para ver cuantas caasas regulares no tienen gps despues de haber 
#unido todos los gps con las adicionales .
aux<-merge(attack_2006_2015,aqp_completo,by = "UNICODE",all.x = TRUE)
length(which(is.na(aux$LATITUDE)))

write.csv(aux,"casasregullressingps.csv",row.names = FALSE)

rociado_gps<-merge(attack_2006_2015,gps_aqp,by = "UNICODE",all.x = TRUE)
rociado_gps<-as.data.table(rociado_gps)
rociado_gps$LATITUDE<-as.character(rociado_gps$LATITUD)



write.csv(rociado_gps,"casas_rociado_gps.csv",row.names =FALSE)

#escogiendo variablesque nos interesan y cambiando los nombres a las otras columnas  
length(which(is.na(rociado_gps$LATITUDE)))     #5865 observations with no coordinates
casas_singps<- (which(is.na(rociado_gps$LATITUDE)))
casas_singps<-as.data.frame(casas_singps)
missing.gps <- missing.gps[which(is.na(rociado_gps$LATITUDE))]   
 missing.gps<-as.data.frame(missing.gps)
  

varstokeep <- c("UNICODE", "P.y","D.y","L.y","V.y","FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO","LATITUDE","LONGITUDE")
rociado_gps <- rociado_gps[,varstokeep]
rociado_gps<-as.data.table(rociado_gps) 
  #changed columns names 
  setnames(rociado_gps,"P.y","P")
  setnames(rociado_gps,"D.y","D")
  setnames(rociado_gps,"L.y","L")
  setnames(rociado_gps,"V.y","V") 

           
  #Convertir a character
  rociado_gps$UNICODE <- as.character(rociado_gps$UNICODE)
  rociado_gps$L <- as.character(rociado_gps$L)
  rociado_gps$V <- as.character(rociado_gps$V)
  gps_aqp$UNICODE <- as.character(gps_aqp$UNICODE)
  gps_aqp$L <- as.character(gps_aqp$L)
  gps_aqp$V <- as.character(gps_aqp$V)

#Seleccionar solo los datos unicos
  rociado_gps_unicos <- rociado_gps[which(!duplicated(rociado_gps$UNICODE)),]
  data <- rociado_gps_unicos
  
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
 
  data_unicos <- data_added[which(!duplicated(data_added$UNICODE)),]
  data <- data_unicos 
   
#--------------------------------------------------------------------  
#Imprimiendo Resultados
#--------------------------------------------------------------------
  
  #Resultado de las viviendas adicionadas
  write.csv(data_added,"added_aqp_2006_2015.csv",row.names = FALSE)
  