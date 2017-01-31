#Requisitos
#- En la columna IDENT tiene que estar el numero de cuadra con el siguiente formato para que pueda ser contado como cuadra válida: "1.1.2-4"
#- En la columna LONG existe NA
#- Debe contener las columna "LAT" y "LONG"
#- Debe estar ordenado por grupos de localidad, ejm todos los de la localidad 7 deben estar uno siguiendo a otro.

#Librerias utilizadas
  install.packages("spatialEco")
  install.packages("sp")
  library(spatialEco)
  library(sp)
#-RUTAS UTILIZADAS
  
  ruta_1 <- 'C:/Users/Rodrigo/Documents/claudia codigos r'

#Ruta
  
  setwd(ruta_1)
  
#Leer los archivos
  #Base de datos del consolidado 2009-2015
    attack_db <- read.csv("C:/Users/Rodrigo/Documents/claudia codigos r/cons_rociado_2009_2015.csv")
  
  casas_aqp<-read.csv("C:/Users/Rodrigo/Documents/claudia codigos r/AREQUIPA_GPS_GOOGLE.csv")
  casas_aqp_adicionadas <- read.csv("C:/Users/Rodrigo/Documents/claudia codigos r/added_aqpI.csv")
  #nc_hunter<-read.csv("C:/Users/Rodrigo/Documents/claudia codigos r/Manzanas _Arequipa/Jacobo Hunter/Jacobo Hunter MZ.csv")
  #nc_hunter<-read.csv("C:/Users/Rodrigo/Documents/claudia codigos r/Manzanas _Arequipa/Cayma/Cayma_Mz.csv")
  #nc_hunter <- nc_hunter[,c("type","ident","lat","long","y_proj","x_proj","new_seg")]
 
 nc_hunter$lat <- gsub(" ","",nc_hunter$lat, fixed = T) 
   nc_hunter$long <- gsub(" ","",nc_hunter$long, fixed = T)
   Cayma_Mz <- read.csv("Cayma_Mz.csv")
   
   blank_to_NA <- function(nc_hunter) {
     num_row <- nrow(nc_hunter)
     num_col <- ncol(nc_hunter)
     for (i in 1:num_row) {
       for (j in 1:num_col) {
         if (nc_hunter[i,j] == " ") {
           nc_hunter[i,j] <- NA
         }
         else if (nc_hunter[i,j] == "") {
           nc_hunter[i,j] <- NA
         }
       }
     }
     return(nc_hunter)
   }
   nc_hunter<-blank_to_NA(nc_hunter)
 

   
 n_row<-nrow(nc_hunter)
 n_col<- ncol(nc_hunter)
   for(i in 1:n_row) {
    for (j in 1:n_col) {
      if (nc_hunter[i,][,j] == ""){
        nc_hunter[i,][,j] <- NA
      }
    }
  }
  
#nc_hunter<-as.character(nc_hunter$lat)
  #nc_hunter<-as.character(nc_hunter$long)
  
  #nc_asa<-read.csv("C:/Users/Rodrigo/Documents/claudia codigos r/Manzanas _Arequipa/ASA/Alto Selva Alegre_Mz.csv",sep =";")
  #nc_ccolorado<-read.csv("C:/Users/Rodrigo/Documents/claudia codigos r/Manzanas _Arequipa/Cerro Colorado/Cerro Colorado MZ.csv",sep=",")
  #nc_lajoya<-read.csv("C:/Users/Rodrigo/Documents/claudia codigos r/Manzanas _Arequipa/La Joya/La Joya MZ.csv",sep=",")
  #nc_jlbyrivero<-read.csv("C:/Users/Rodrigo/Documents/claudia codigos r/Manzanas _Arequipa/JLByRivero/JLByRibero MZ.csv")
  #nc_miraflores<-read.csv("file:///C:/Users/Rodrigo/Documents/claudia codigos r/Manzanas _Arequipa/Miraflores/Miraflores_mz.csv",sep=";")
  nc_paucarpata<-read.csv("file:///C:/Users/Rodrigo/Documents/claudia codigos r/Manzanas _Arequipa/Paucarpata/Paucarpata.csv",sep = ",")
  #nc_socabaya<-read.csv("C:/Users/Rodrigo/Documents/claudia codigos r/Manzanas _Arequipa/Socabaya/socabaya_mz.csv")
  
#--SOLO CONSOLIDADO--
  #Extrayendo solo datos de MARIANO MELGAR del consolidado
    #attack_mm <- attack_db[(1==attack_db$P & 10==attack_db$D),]
  #Seleccionando solo los campos que necesito
    #attack_mm <- attack_mm[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
    #Extrayendo solo datos de Cayma del consolidado
    attack_cayma <- attack_db[(1==attack_db$P & 3==attack_db$D),]
    #Seleccionando solo los campos que necesito
    attack_cayma <- attack_cayma[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
    #Extrayendo solo datos de Cayma del consolidado
    #attack_ccolorado <- attack_db[(1==attack_db$P & 4==attack_db$D),]
    #Seleccionando solo los campos que necesito
    #attack_ccolorado <- attack_ccolorado[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
    #Extrayendo solo datos de Cayma del consolidado
    #attack_hunter <- attack_db[(1==attack_db$P & 7==attack_db$D),]
    #Seleccionando solo los campos que necesito
    #attack_hunter <- attack_hunter[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
    #Extrayendo solo datos de lajoya del consolidado
    #attack_lajoya <- attack_db[(1==attack_db$P & 9==attack_db$D),]
    #Seleccionando solo los campos que necesito
    #attack_lajoya <- attack_lajoya[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
  #Extrayendo solo datos de lajoya del consolidado
  #attack_jlbyrivero <- attack_db[(1==attack_db$P & 8==attack_db$D),]
  #Seleccionando solo los campos que necesito
  #attack_jlbyrivero <- attack_lajoya[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
  #Extrayendo solo datos de miraflores del consolidado
  #attack_miraflores <- attack_db[(1==attack_db$P & 11==attack_db$D),]
  #Seleccionando solo los campos que necesito
  #attack_miraflores <- attack_miraflores[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
  #Extrayendo solo datos de Paucarpata 
  attack_paucarpata <- attack_db[(1==attack_db$P & 13==attack_db$D),]
  #Seleccionando solo los campos que necesito
  #attack_paucarpata <- attack_paucarpata[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
  
   #--------------------------
  
#Juntando las casas adicionadas con el total de viviendas
  casas_aqp_total <- rbind(casas_aqp, casas_aqp_adicionadas)

#Verificar si hay duplicados
  indice_dupli <- casas_aqp[which(duplicated(casas_aqp$UNICODE)),1]
  duplicados<-casas_aqp[casas_aqp$UNICODE %in% indice_dupli,]
  duplicados <- duplicados[order(duplicados$UNICODE),]
  
  indice_dupli <- casas_aqp_total[which(duplicated(casas_aqp_total$UNICODE)),1]
  duplicados<-casas_aqp_total[casas_aqp_total$UNICODE %in% indice_dupli,]
  duplicados <- duplicados[order(duplicados$UNICODE),]

#Bases utilizadas en el proceso
  #nc_hunter <- nc_hunter[,c("type","ident","lat","long","y_proj","x_proj","new_seg","display","color","altitude","depth","temp","time","model","filename","ltime")]
  nc_polygon <- nc_paucarpata
  old_casa_aqp <- casas_aqp_total
  
#Convirtiendo de factor a character o numero a caracter
  nc_polygon$ident <- as.character(nc_polygon$ident)
  nc_polygon$long <- as.character(nc_polygon$long)
  nc_polygon$lat <- as.character(nc_polygon$lat)
  casas_aqp$D <- as.character(casas_aqp$D)
  casas_aqp$L <- as.character(casas_aqp$L)

#Creando columna para el numero de localidad y de cuadra
  nc_polygon$num_distrito <- NA
  nc_polygon$num_localidad <- NA
  nc_polygon$num_cuadra <- NA

#Creando variables
  x<-NULL
  y<-NULL
  distrito <- 0
  localidad <- 0
  aqp_gps_block <- data.frame()
  aux <- data.frame()
  no_block <- data.frame()#Alamcenaremos los que no tienen cuadra
  inicio <-1#si tiene 1 es por que es un primer elemento 
  
  n_row <- nrow(nc_polygon)+1
  
  i<-1 #Controlador del bucle
  while (i <= n_row) {
    if (!is.na(nc_polygon$long[i])) {
      #Acumulando los "x" e "y"
        x<-c(x,nc_polygon$long[i])#Longitud
        y<-c(y,nc_polygon$lat[i])#Latitud
    } else{
      
      if(1!=inicio){
        #Pregunta si tienen el mismo distrito y localidad, si no entonces se extrae otra porcion de viviendas
        if (distrito!=nc_polygon$num_distrito[indice] | localidad!=nc_polygon$num_localidad[indice]) {
          no_block <- rbind(no_block,aux)
          aux <- old_casa_aqp[nc_polygon$num_distrito[indice]== old_casa_aqp$D & nc_polygon$num_localidad[indice]== old_casa_aqp$L,]
          aux$block <- NA
          old_casa_aqp <- old_casa_aqp[!(nc_polygon$num_distrito[indice]== old_casa_aqp$D & nc_polygon$num_localidad[indice]== old_casa_aqp$L),]
        }
          
        for (j in 1:nrow(aux)) {
          #Utilizando la funcion "point.in.polygon" para ver si el punto esta dentro del poligono
          opcion <- point.in.polygon(aux$LONGITUDE[j],aux$LATITUDE[j], x,y)
          if (0!=opcion) {
            aux$block[j] <- nc_polygon$num_cuadra[indice]
          }
        }
        #Almacenando los puntos que tienen numero de cuadra en una base de datos final
        aqp_gps_block <- rbind(aqp_gps_block,aux[!is.na(aux$block),])
        #Alamcenando los codigos que aun falta poner cuadra
        aux <- aux[is.na(aux$block),]
        
        #Inicializando los puntos del poligono
        x<-NULL
        y<-NULL
        #Almacenando distrito y localidad
        distrito <- nc_polygon$num_distrito[indice]
        localidad <- nc_polygon$num_localidad[indice]
      }
      if (i < n_row) {
        #Separar donde encuentre el caracter "-"
        split_ident <- unlist(strsplit(nc_polygon$ident[i], "-", fixed = TRUE))
        #Si es distinto a NA continuará el ciclo, pueden haber nombre que NO contengan el caracter "-"
        if (!is.na(split_ident[2])) {
          #Revisando si existe una letra
            letter_number <- regexpr("[A-Za-z]", split_ident[2])
            #Pregunta si el segundo argumento es numero
            if (  letter_number[1]==-1 ) {
              #Separando el codigo por cada punto
              codigo <- unlist(strsplit(split_ident[1], ".", fixed = TRUE))                              
              #Almacenando numero de distrito
                nc_polygon$num_distrito[i] <- codigo[2]
              #Almacenando numero de localidad
                nc_polygon$num_localidad[i] <- codigo[3]
              #Almacenando el numero de cuadra
                nc_polygon$num_cuadra[i] <- split_ident[2]
              #Almacenando indice
                indice <- i   
                inicio <- 0
            } else {
              while (!is.na(nc_polygon$long[i+1])) {
                i<-i+1
                inicio <-1
              }
            }
        } else {
          while (!is.na(nc_polygon$long[i+1])) {
            i<-i+1
            inicio <-1
          }
        }
      }
    }
    i<-i+1
  }
  #Escribiendo el file resultante del codigo manzanas number  
  #en el caso de alguno distritos donde no tenemos informacion en la tabla
  #del rociado ,solo obtendremos los files de las cuadras con los gps .
  #write.csv(aqp_gps_block,"manzanas_number_hunter.csv", row.names= FALSE)
  #write.csv(aqp_gps_block,"manzanas_number_jlbyrivero.csv", row.names= FALSE)
  
  #--------JUNTANDO CONSOLIDADO CON CUADRAS -----
  #Diferencia C-S v S-C
    #diff1<-setdiff(attack_mm$UNICODE,aqp_gps_block$UNICODE) # 10 viviendas
    #diff2<-setdiff(aqp_gps_block$UNICODE,attack_mm$UNICODE) # 5055
  #Interseccion
    #interseption<-intersect(attack_mm$UNICODE, aqp_gps_block$UNICODE)
    #Diferencia C-S v S-C
    #diff1<-setdiff(attack_asa$UNICODE,aqp_gps_block$UNICODE) # 76 viviendas
    #diff2<-setdiff(aqp_gps_block$UNICODE,attack_asa$UNICODE) # 14853
    #Interseccion
    #interseption<-intersect(attack_asa$UNICODE, aqp_gps_block$UNICODE) 
    
    #Diferencia C-S v S-C
    diff1<-setdiff(attack_miraflores$UNICODE,aqp_gps_block$UNICODE) # 4 viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_miraflores$UNICODE) # 5510
    #Interseccion
    interseption<-intersect(attack_miraflores$UNICODE, aqp_gps_block$UNICODE)#6950 viviendas  
    
    
    #Merge
    #mmelgar_gps_rociado <- merge(aqp_gps_block,attack_mm, all= TRUE, by = "UNICODE")
    #asa_gps_rociado <- merge(aqp_gps_block,attack_asa, all= TRUE, by = "UNICODE")
    miraflores_gps_rociado <- merge(aqp_gps_block,attack_miraflores, all= TRUE, by = "UNICODE")
    
    #Comprobando
    aux1 <- attack_miraflores[attack_miraflores$UNICODE%in%diff1,]
    aux2 <- miraflores_gps_rociado[miraflores_gps_rociado$UNICODE%in%diff1,]
    aux3 <- miraflores_gps_rociado[miraflores_gps_rociado$UNICODE%in%diff2,]
#----------------------------------------------
  
#--------------------------------------------------------------------  
#Imprimiendo Resultados
#--------------------------------------------------------------------
  
  #Resultado de las viviendas de Mariano Melgar que tienen numero de cuadra
  write.csv(miraflores_gps_rociado,"C:/Users/Rodrigo/Documents/claudia codigos r/miraflores_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de Mariano Melgar que NO tienen numero de cuadra
  write.csv(no_block,"C:/Users/Rodrigo/Documents/claudia codigos r/no_block.csv", row.names = FALSE)
  
  
  
  