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
  ruta_1 <- '~/claudia codigos r/'

#Ruta
  
  setwd(ruta_1)
  
#Leer los archivos
  #Base de datos del consolidado  de  phase attack 2009-2015(DISTRITOS:CAYMA,CERRO COLORADO,ASA,CHARACATO
  #MARIANO MELGAR,MIRAFLORES,MOLLEBAYA,PAUCARPATA(2) Y YARABAMBA)
  attack_db_2 <- read.csv("~/claudia codigos r/attack_gps_2009_2015.csv")
# Base de Ataque de union rociado antes del 2009(DISTRITOS:HUNTER,JLB Y R ,LA JOYA ,PAUCARPATA(1),SACHACA
  #UCHUMAYO,TIABAYA,SOCABAYA)  
  
  attack_db_1 <- read.csv("~/claudia codigos r/attack_gps_2006_2009.csv")
  
  #LEYENDO LOS ARCHIVOS QUE CONTIENES LOS GP/S DE CASAS NORMALES Y ADICIONADAS .
  casas_aqp<-read.csv("~/claudia codigos r/AREQUIPA_GPS_GOOGLE.csv")
  casas_aqp_adicionadas_I <- read.csv("~/claudia codigos r/added_aqpI.csv")
  casas_aqp_adicionadas_II<- read.csv("~/claudia codigos r/added_aqpII.csv")
  
  
  #OBTENIENDO LAS BASES DE MANZANA  HUNTER ,JLB Y RIVERO ,LA JOYA ,SACHACA,UCHUMAYO,TIABAYA Y SOCABAYA 
  
  nc_HUNTER<-read.csv("~/claudia codigos r//Manzanas _Arequipa/Jacobo Hunter/Jacobo Hunter MZ.csv")
  nc_JLB_RIVERO<-read.csv("~/claudia codigos r/Manzanas _Arequipa/JLByRivero/JLByRibero MZ.csv")
  nc_LAJOYA<-read.csv("~/claudia codigos r/Manzanas _Arequipa/La Joya/La Joya MZ.csv")
  nc_SACHACA<-read.csv("~/claudia codigos r/Manzanas _Arequipa/Sachaca/Sachaca_mz.csv",sep = ";")
  nc_UCHUMAYO<-read.csv("~/claudia codigos r/Manzanas _Arequipa/Uchumayo/Uchumayo.csv")
  nc_TIABAYA<-read.csv("~/claudia codigos r/Manzanas _Arequipa/Tiabaya/Tiabaya.csv",sep = ";")
  nc_SOCABAYA<-read.csv("~/claudia codigos r/Manzanas _Arequipa/Socabaya/socabaya_mz.csv",sep = ";")
  
  #OBTENIENDO LAS BASES DE  MANZANA ASA ,CAYMA ,CERRO COLORADO,CHARACATO ,MARIANO MELGAR,MIRAFLORES,MOLLEBAYA
  #PAUCARPATA(2)Y YARABAMBA. 
  nc_melgar<-read.csv("unicode_numberMzn/Manzanas _Arequipa/Mariano Melgar/MARIANO MELGAR.csv")
  nc_CAYMA <-read.csv("~/claudia codigos r/Manzanas _Arequipa/Cayma/Cayma_Mz.csv", sep = ",")
  nc_ASA <-read.csv("~/claudia codigos r/Manzanas _Arequipa/ASA/Alto Selva Alegre_Mz.csv", sep = ";")
  nc_YARABAMBA<-read.csv ("~/claudia codigos r/Manzanas _Arequipa/Yarabamba/Yarabamba.csv",sep = ";")
  nc_MIRAFLORES <-read.csv ("~/claudia codigos r/Manzanas_Arequipa/Miraflores/Miraflores_mz.csv",  sep = ";")
  #nc_CERRO_COLORADO <-read.csv("~/claudia codigos r/Manzanas _Arequipa/Cerro Colorado/Cerro Colorado MZ.csv", sep = ";")
  nc_CHARACATO <-read.csv("~/claudia codigos r/Manzanas _Arequipa/Characato/Characato MZ.csv", sep = ";")
  nc_PAUCARPATA<-read.csv("~/claudia codigos r/Manzanas _Arequipa/Paucarpata/Paucarpata.csv")
 
 #---------------------------------------------------------------------------------- 
  
   #--SOLO CONSOLIDADO--
  #Extrayendo solo datos de MARIANO MELGAR del consolidado
    #attack_mm <- attack_db[(1==attack_db$P & 10==attack_db$D),]
  #Seleccionando solo los campos que necesito
    #attack_mm <- attack_mm[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
#--------------------------
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de HUNTER del consolidado
    attack_HUNTER <- attack_db_1[(1==attack_db_1$P & 7==attack_db_1$D),]
    #Seleccionando solo los campos que necesito
    attack_HUNTER <- attack_HUNTER[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
    #-------------------------- 
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de MARIANO MELGAR del consolidado
    attack_JLB_RIVERO <- attack_db_1[(1==attack_db_1$P & 8==attack_db_1$D),]
    #Seleccionando solo los campos que necesito
    attack_JLB_RIVERO <- attack_JLB_RIVERO[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
    #-------------------------- 
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de Yarabamba del consolidado
    attack_Yarabamba <- attack_db_2[(1==attack_db_2$P & 28==attack_db_2$D),]
    #Seleccionando solo los campos que necesito
    attack_Yarabamba <- attack_Yarabamba[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
    #-------------------------- 
        #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de MARIANO MELGAR del consolidado
    attack_ASA<- attack_db_2[(1==attack_db_2$P & 1==attack_db_2$D),]
    #Seleccionando solo los campos que necesito
    attack_ASA <- attack_ASA[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
    #--------------------------
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de PAUCARPATA del consolidado
    attack_p_1 <- attack_db_1[(1==attack_db_1$P & 13==attack_db_1$D),]
    #Extrayendo solo datos de PAUCARPATA del consolidado
    attack_p_2 <- attack_db_2[(1==attack_db_2$P & 13==attack_db_2$D),]
 
   #UNIENDO LA BASE DE PAUCARPATA DE AMBOS ROCIADOS 
    attack_PAUCARPATA<-rbind(attack_p_1,attack_p_2)  
    #Seleccionando solo los campos que necesito
    attack_PAUCARPATA <- attack_PAUCARPATA[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
    #-------------------------- 
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de YARABAMABA del consolidado
    attack_yarabamba <- attack_db[(1==attack_db$P & 28==attack_db$D),]
    #Seleccionando solo los campos que necesito
    attack_yarabamba <- attack_yarabamba[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
    #-------------------------- 
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de  LA JOYA  del consolidado
    attack_LA_JOYA <- attack_db_1[(1==attack_db_1$P & 9==attack_db_1$D),]
    #Seleccionando solo los campos que necesito
    attack_LA_JOYA <- attack_LA_JOYA[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
    #-------------------------- 
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de CAYMA del consolidado
    attack_cayma  <- attack_db_2[(1==attack_db_2$P & 3==attack_db_2$D),]
    #Seleccionando solo los campos que necesito
    attack_cayma <- attack_cayma[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
   
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de  CHARACATO del consolidado
    attack_CHARACATO  <- attack_db_2[(1==attack_db_2$P & 5==attack_db_2$D),]
    #Seleccionando solo los campos que necesito
    attack_CHARACATO <- attack_CHARACATO[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
 
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de  CHARACATO del consolidado
    attack_SACHACA  <- attack_db_1[(1==attack_db_1$P & 18==attack_db_1$D),]
    #Seleccionando solo los campos que necesito
    attack_SACHACA <- attack_SACHACA[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
    
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de  CHARACATO del consolidado
    attack_TIABAYA  <- attack_db_1[(1==attack_db_1$P & 24==attack_db_1$D),]
    #Seleccionando solo los campos que necesito
    attack_TIABAYA <- attack_TIABAYA[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
    
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de  CHARACATO del consolidado
    attack_SOCABAYA  <- attack_db_1[(1==attack_db_1$P & 25==attack_db_1$D),]
    #Seleccionando solo los campos que necesito
    attack_SOCABAYA <- attack_SOCABAYA[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
        
    #Juntando las casas adicionadas con el total de viviendas
  casas_aqp_total <- rbind(casas_aqp, casas_aqp_adicionadas_I,casas_aqp_adicionadas_II)

#Verificar si hay duplicados
  indice_dupli <- casas_aqp[which(duplicated(casas_aqp$UNICODE)),1]
  duplicados<-casas_aqp[casas_aqp$UNICODE %in% indice_dupli,]
  duplicados <- duplicados[order(duplicados$UNICODE),]
  
  indice_dupli <- casas_aqp_total[which(duplicated(casas_aqp_total$UNICODE)),1]
  duplicados<-casas_aqp_total[casas_aqp_total$UNICODE %in% indice_dupli,]
  duplicados <- duplicados[order(duplicados$UNICODE),]

#Bases utilizadas en el proceso
  #nc_polygon <- nc_melgar
  nc_polygon <- nc_HUNTER
  nc_polygon <- nc_JLB_RIVERO
  nc_polygon <- nc_LAJOYA
  nc_polygon <- nc_CAYMA
  nc_polygon <- nc_ASA
  nc_polygon <- nc_YARABAMBA
  nc_polygon <- nc_CHARACATO
  nc_polygon <- nc_PAUCARPATA
  nc_polygon <- nc_SACHACA
  nc_polygon <- nc_TIABAYA
  nc_polygon <- nc_SOCABAYA
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
  
  aux_null <- 0 #Si es 0 significa que si existen datos en la variable "aux"
  
  n_row <- nrow(nc_polygon)+1
  
  i<-1 #Controlador del bucle
  while (i <= n_row) {
    if ( !is.na(nc_polygon$long[i]) && !(nc_polygon$long[i]==" ") ) {
      #Acumulando los "x" e "y"
        x<-c(x,nc_polygon$long[i])#Longitud
        y<-c(y,nc_polygon$lat[i])#Latitud
    } else{
      
      if(1!=inicio){
        #Pregunta si tienen el mismo distrito y localidad, si no entonces se extrae otra porcion de viviendas
        if (distrito!=nc_polygon$num_distrito[indice] | localidad!=nc_polygon$num_localidad[indice]) {
          no_block <- rbind(no_block,aux)
          aux <- old_casa_aqp[nc_polygon$num_distrito[indice]== old_casa_aqp$D & nc_polygon$num_localidad[indice]== old_casa_aqp$L,]
          if ( nrow(aux)!=0 ) {
            aux$block <- NA
            old_casa_aqp <- old_casa_aqp[!(nc_polygon$num_distrito[indice]== old_casa_aqp$D & nc_polygon$num_localidad[indice]== old_casa_aqp$L),] 
            aux_null <- 0
          }
          else {
            aux_null <- 1
          }
        } else {
          if (nrow(aux)==0){
            aux_null <- 1
          }
        }
        
        if ( aux_null == 0 ) {
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
          
          #Almacenando distrito y localidad
          distrito <- nc_polygon$num_distrito[indice]
          localidad <- nc_polygon$num_localidad[indice]
        }
        #Inicializando los puntos del poligono
        x<-NULL
        y<-NULL
      }
      if (i < n_row ) {
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
              while (!is.na(nc_polygon$long[i+1]) && !(nc_polygon$long[i+1]==" ") ) {
                i<-i+1
                inicio <-1
              }
            }
        } else {
          while (!is.na(nc_polygon$long[i+1]) && !(nc_polygon$long[i+1]==" ") ) {
            i<-i+1
          }
          
          inicio <-1
          #aux_null <- 0
        }
      }
    }
    i<-i+1
  }
  
#--------JUNTANDO CONSOLIDADO CON CUADRAS -----
  #Diferencia C-S v S-C
    diff1<-setdiff(attack_mm$UNICODE,aqp_gps_block$UNICODE) # 10 viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_mm$UNICODE) # 5055
  #Interseccion
    interseption<-intersect(attack_mm$UNICODE, aqp_gps_block$UNICODE)
    
  #Merge
    mmelgar_gps_rociado <- merge(aqp_gps_block,attack_mm, all= TRUE, by = "UNICODE")
  #Comprobando
    aux1 <- attack_mm[attack_mm$UNICODE%in%diff1,]
    aux2 <- mmelgar_gps_rociado[mmelgar_gps_rociado$UNICODE%in%diff1,]
    aux3 <- mmelgar_gps_rociado[mmelgar_gps_rociado$UNICODE%in%diff2,]
   
    
         #--------JUNTANDO CONSOLIDADO CON CUADRAS -----
    #Diferencia C-S v S-C
    diff1<-setdiff(attack_HUNTER$UNICODE,aqp_gps_block$UNICODE) # 318 viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_HUNTER$UNICODE) # 544
    #Interseccion
    interseption<-intersect(attack_HUNTER$UNICODE, aqp_gps_block$UNICODE)#10183
        #Merge
    HUNTER_gps_rociado <- merge(aqp_gps_block,attack_HUNTER, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_HUNTER[attack_HUNTER$UNICODE%in%diff1,]#484 VIVIENDAS 
    aux2 <- HUNTER_gps_rociado[HUNTER_gps_rociado$UNICODE%in%diff1,]
    aux3 <- HUNTER_gps_rociado[HUNTER_gps_rociado$UNICODE%in%diff2,]#544
    
    #Diferencia C-S v S-C
    diff1<-setdiff(attack_JLB_RIVERO$UNICODE,aqp_gps_block$UNICODE) # 20 viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_JLB_RIVERO$UNICODE) # 5055
    #Interseccion
    interseption<-intersect(attack_JLB_RIVERO$UNICODE, aqp_gps_block$UNICODE)
    
    #Merge
    JLB_RIVERO_gps_rociado <- merge(aqp_gps_block,attack_JLB_RIVERO, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_JLB_RIVERO[attack_JLB_RIVERO$UNICODE%in%diff1,]
    aux2 <- JLB_RIVERO_gps_rociado[JLB_RIVERO_gps_rociado$UNICODE%in%diff1,]
    aux3 <- JLB_RIVERO_gps_rociado[JLB_RIVERO_gps_rociado$UNICODE%in%diff2,]
    
    #_______________________________
    
#---------------------------------------------
    #Diferencia C-S v S-C
    diff1<-setdiff(attack_LA_JOYA$UNICODE,aqp_gps_block$UNICODE) #  viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_LA_JOYA$UNICODE) # 5055
    #Interseccion
    interseption<-intersect(attack_LA_JOYA$UNICODE, aqp_gps_block$UNICODE)
    
    #Merge
    LA_JOYA_gps_rociado <- merge(aqp_gps_block,attack_LA_JOYA, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_LA_JOYA[attack_LA_JOYA$UNICODE%in%diff1,]
    aux2 <- LA_JOYA_gps_rociado[LA_JOYA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- LA_JOYA_gps_rociado[LA_JOYA_gps_rociado$UNICODE%in%diff2,]
#_____________________________________
    
    #uniendo bases 
    diff1<-setdiff(attack_cayma$UNICODE,aqp_gps_block$UNICODE) #  72viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_cayma$UNICODE) # 11491
    #Interseccion
    interseption<-intersect(attack_cayma$UNICODE, aqp_gps_block$UNICODE)#6581
    
    #Merge
    CAYMA_gps_rociado <- merge(aqp_gps_block,attack_cayma, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_cayma[attack_cayma$UNICODE%in%diff1,]#112
    aux2 <- CAYMA_gps_rociado[CAYMA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- CAYMA_gps_rociado[CAYMA_gps_rociado$UNICODE%in%diff2,]#11491
    
    
    #uniendo bases 
    diff1<-setdiff(attack_ASA$UNICODE,aqp_gps_block$UNICODE) #  76viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_ASA$UNICODE) # 14853
    #Interseccion
    interseption<-intersect(attack_ASA$UNICODE, aqp_gps_block$UNICODE)#6012
    
    #Merge
    ASA_gps_rociado <- merge(aqp_gps_block,attack_ASA, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_ASA[attack_ASA$UNICODE%in%diff1,]#140 observaciones 
    aux2 <- ASA_gps_rociado[ASA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- ASA_gps_rociado[ASA_gps_rociado$UNICODE%in%diff2,]#14853 
 #_--------------------------------------------------------------------
    #uniendo bases 
    diff1<-setdiff(attack_Yarabamba$UNICODE,aqp_gps_block$UNICODE) #  5viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_Yarabamba$UNICODE) # 327Viviendas 
    #Interseccion
    interseption<-intersect(attack_Yarabamba$UNICODE, aqp_gps_block$UNICODE)#180
    
    #Merge
    YARABAMBA_gps_rociado <- merge(aqp_gps_block,attack_Yarabamba, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_Yarabamba[attack_Yarabamba$UNICODE%in%diff1,]#10 observaciones 
    aux2 <- YARABAMBA_gps_rociado[YARABAMBA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- YARABAMBA_gps_rociado[YARABAMBA_gps_rociado$UNICODE%in%diff2,]#327  
 #------------------------------------------------------------------------   
    #uniendo bases 
    diff1<-setdiff(attack_CHARACATO$UNICODE,aqp_gps_block$UNICODE) #  240viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_CHARACATO$UNICODE) # 1928Viviendas 
    #Interseccion
    interseption<-intersect(attack_CHARACATO$UNICODE, aqp_gps_block$UNICODE)#1486
    
    #Merge
    CHARACATO_gps_rociado <- merge(aqp_gps_block,attack_CHARACATO, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_CHARACATO[attack_CHARACATO$UNICODE%in%diff1,]#345 observaciones 
    aux2 <- CHARACATO_gps_rociado[CHARACATO_gps_rociado$UNICODE%in%diff1,]
    aux3 <- CHARACATO_gps_rociado[CHARACATO_gps_rociado$UNICODE%in%diff2,]#1928   
 #------------------------------------------------------------------------------------      
    #uniendo bases 
    diff1<-setdiff(attack_SACHACA$UNICODE,aqp_gps_block$UNICODE) #  293viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_SACHACA$UNICODE) # 1526Viviendas 
    #Interseccion
    interseption<-intersect(attack_SACHACA$UNICODE, aqp_gps_block$UNICODE)#2870
    
    #Merge
    SACHACA_gps_rociado <- merge(aqp_gps_block,attack_SACHACA, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_SACHACA[attack_SACHACA$UNICODE%in%diff1,]#577 observaciones 
    aux2 <- SACHACA_gps_rociado[SACHACA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- SACHACA_gps_rociado[SACHACA_gps_rociado$UNICODE%in%diff2,]#1526
    
 #--------------------------------------------------------------------------------   
    #uniendo bases 
    diff1<-setdiff(attack_TIABAYA$UNICODE,aqp_gps_block$UNICODE) #  246viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_TIABAYA$UNICODE) # 25Viviendas 
    #Interseccion
    interseption<-intersect(attack_TIABAYA$UNICODE, aqp_gps_block$UNICODE)#3032
    
    #Merge
    TIABAYA_gps_rociado <- merge(aqp_gps_block,attack_TIABAYA, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_TIABAYA[attack_TIABAYA$UNICODE%in%diff1,]#479 observaciones 
    aux2 <- TIABAYA_gps_rociado[TIABAYA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- TIABAYA_gps_rociado[TIABAYA_gps_rociado$UNICODE%in%diff2,]#25
 #-----------------------------------------------------------------------------------   
    #uniendo bases 
    diff1<-setdiff(attack_SOCABAYA$UNICODE,aqp_gps_block$UNICODE) #  138viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_SOCABAYA$UNICODE) # 5200Viviendas 
    #Interseccion
    interseption<-intersect(attack_SOCABAYA$UNICODE, aqp_gps_block$UNICODE)#10143
    
    #Merge
    SOCABAYA_gps_rociado <- merge(aqp_gps_block,attack_SOCABAYA, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_SOCABAYA[attack_SOCABAYA$UNICODE%in%diff1,]#256 observaciones 
    aux2 <- SOCABAYA_gps_rociado[SOCABAYA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- SOCABAYA_gps_rociado[SOCABAYA_gps_rociado$UNICODE%in%diff2,]#5200
    
#--------------------------------------------------------------------  
#Imprimiendo Resultados ROCIADOS DEL 2009 EN ADELANTE 
#--------------------------------------------------------------------
  
  #Resultado de las viviendas de Mariano Melgar que tienen numero de cuadra
  write.csv(mmelgar_gps_rociado,"unicode_numberMzn/resultados/mmelgar_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de Mariano Melgar que NO tienen numero de cuadra
  write.csv(no_block,"unicode_numberMzn/resultados/no_b~lock_MARIANOMELGAR.csv", row.names = FALSE)
 
   #Resultado de las viviendas de CAYMA  que tienen numero de cuadra
  write.csv(CAYMA_gps_rociado,"~/claudia codigos r/CAYMA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de CAYMA  que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_CAYMA.csv", row.names = FALSE)
  
  setwd('~/claudia codigos r')
  #Resultado de las viviendas de ASA que tienen numero de cuadra
  write.csv(ASA_gps_rociado,"~/claudia codigos r/ASA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de ASA que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_ASA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de YARABAMBA  que tienen numero de cuadra
  write.csv(YARABAMBA_gps_rociado,"~/claudia codigos r/YARABAMBA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de YARABAMBA  que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_YARABAMBA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de CHARACATO  que tienen numero de cuadra
  write.csv(CHARACATO_gps_rociado,"~/claudia codigos r/CHARACATO_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de CHARACATO  que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_CHARACATO.csv", row.names = FALSE)
  
  
  
  
  
  
  #Resultado de las viviendas de HUNTER que tienen numero de cuadra
  write.csv(HUNTER_gps_rociado,"~/claudia codigos r/HUNTER_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de HUNTER que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_HUNTER.csv", row.names = FALSE)
  
  #Resultado de las viviendas de JLB Y RIVERO que tienen numero de cuadra
  write.csv(JLB_RIVERO_gps_rociado,"~/claudia codigos r/JLB_RIVERO_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de HUNTER que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_JLB.csv", row.names = FALSE)
  
  #Resultado de las viviendas de LA JOYA  que tienen numero de cuadra
  write.csv(LA_JOYA_gps_rociado,"~/claudia codigos r/LA_JOYA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de HUNTER que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_LAJOYA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de CAYMA  que tienen numero de cuadra
  write.csv(CAYMA_gps_rociado,"~/claudia codigos r/CAYMA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de CAYMA  que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_CAYMA.csv", row.names = FALSE)
 
  #Resultado de las viviendas de ASA que tienen numero de cuadra
  write.csv(ASA_gps_rociado,"~/claudia codigos r/ASA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de ASA que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_ASA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de YARABAMBA  que tienen numero de cuadra
  write.csv(YARABAMBA_gps_rociado,"~/claudia codigos r/YARABAMBA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de YARABAMBA  que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_YARABAMBA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de CHARACATO  que tienen numero de cuadra
  write.csv(CHARACATO_gps_rociado,"~/claudia codigos r/CHARACATO_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de CHARACATO  que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_CHARACATO.csv", row.names = FALSE)
  
  #Resultado de las viviendas de SACHACA  que tienen numero de cuadra
  write.csv(SACHACA_gps_rociado,"~/claudia codigos r/SACHACA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de SACHACA  que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_SACHACA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de TIABAYA  que tienen numero de cuadra
  write.csv(TIABAYA_gps_rociado,"~/claudia codigos r/blocks_attack_all_districts/TIABAYA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de TIABAYA  que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_TIABAYA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de TIABAYA  que tienen numero de cuadra
  write.csv(SOCABAYA_gps_rociado,"~/claudia codigos r/blocks_attack_all_districts/SOCABAYA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de TIABAYA  que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_SOCABAYA.csv", row.names = FALSE)
  