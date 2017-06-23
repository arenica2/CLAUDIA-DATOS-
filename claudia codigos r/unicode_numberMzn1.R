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
  library(dplyr)
  library(data.table)

#-RUTAS UTILIZADAS
  ruta_1 <- '~/CLAUDIA-DATOS-/claudia codigos r/'
#Ruta
  setwd(ruta_1)
  
#Leer los archivos
#Base de datos CONSOLIDADO GENERAL TODOS LOS DISTRITOS 2006-2015 hasta antes de Cerro Colorado 
   attack<-read.csv("~/CLAUDIA-DATOS-/claudia codigos r/ATTACK_2006_2015/ATTACK_2006_2015.csv")
   attack$UNICODE <- gsub('\\s+', '',attack$UNICODE)
#LEYENDO LOS ARCHIVOS QUE CONTIENEn LOS GP/S DE CASAS NORMALES Y ADICIONADAS .
  
     casas_aqp<-read.csv("~/PETM-shiny/unicode_numberMzn/AREQUIPA_GPS_GOOGLE/AQP_GPS_GOOGLE_EARTH_PUNTOS_05_jun_2017.csv",sep = ',')
     casas_aqp$UNICODE <- gsub('\\s+', '',casas_aqp$UNICODE)
  #OBTENIENDO LAS BASES DE MANZANA  HUNTER ,JLB Y RIVERO ,LA JOYA ,SACHACA,UCHUMAYO,TIABAYA Y SOCABAYA 
  
   nc_HUNTER<-read.csv("~/CLAUDIA-DATOS-/Hunter_Mz_06FEB2017.csv")
   nc_HUNTER[nc_HUNTER==" "]<-"NA"
  
  nc_JLB_RIVERO<-read.csv("~/CLAUDIA-DATOS-/JLByRivero_Mz_corregido.csv",sep = ",")
  nc_LAJOYA<-read.csv("~/CLAUDIA-DATOS-/La_Joya_Mz_corregido.csv")
  nc_SACHACA<-read.csv("~/CLAUDIA-DATOS-/Sachaca_Mz_02FEB2017.csv")
  nc_SACHACA[nc_SACHACA== "NA"]<-NA
  nc_UCHUMAYO<-read.csv("~/CLAUDIA-DATOS-/Uchumayo_Mz_07FEB2017.csv",sep = ",")
  
  nc_TIABAYA<-read.csv("~/CLAUDIA-DATOS-/Tiabaya_Mz_corregido.csv")
  nc_SOCABAYA<-read.csv("~/Downloads/Manzanas_Socabaya_19JUN2017_actualizado.csv",sep = ';')
  
  #OBTENIENDO LAS BASES DE  MANZANA ASA ,CAYMA ,CERRO COLORADO,CHARACATO ,MARIANO MELGAR,MIRAFLORES,MOLLEBAYA
  #PAUCARPATA(2)Y YARABAMBA. 
  nc_melgar<-read.csv("~/Downloads/Manzanas_MnoMelgar_08JUN2017_actualizados..csv")
  nc_melgar<-as.data.table(nc_melgar)
  nc_melgar[ident == "1.10-16-14", ident := "1.10.16-14"]
  nc_melgar<-as.data.frame(nc_melgar)
  
  nc_CAYMA <-read.csv("~/CLAUDIA-DATOS-/Cayma_mz_corregido.csv", sep = ",")
  nc_ASA <-read.csv("~/CLAUDIA-DATOS-/ASA_Mz_corregido.csv", sep = ",")
  nc_YARABAMBA<-read.csv ("~/CLAUDIA-DATOS-/Yarabamba_Mz_corregido.csv")
  nc_MIRAFLORES <-read.csv ("~/CLAUDIA-DATOS-/Miraflores_mz_corregido.csv")
  #nc_CERRO_COLORADO <-read.csv("~/claudia codigos r/Manzanas _Arequipa/Cerro Colorado/Cerro Colorado MZ.csv", sep = ";")
  nc_CHARACATO <-read.csv("~/CLAUDIA-DATOS-/Characato_Mz_03FEB2017.csv")
  nc_PAUCARPATA<-read.csv("~/CLAUDIA-DATOS-/Paucarpata_Mz_07FEB2017.csv")
 
 #---------------------------------------------------------------------------------- 
  
   #--SOLO CONSOLIDADO--
  #Extrayendo solo datos de MARIANO MELGAR del consolidado
  attack_mm <- attack[(1==attack$P & 10==attack$D),]
  #Seleccionando solo los campos que necesito
  attack_mm <- attack_mm[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
  attack_mm <- as.data.table(attack_mm)
  attack_mm$UNICODE <- gsub("\\s+", "", attack_mm$UNICODE)
  
  
  
  
#--------------------------
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de HUNTER del consolidado
    attack_HUNTER <- attack[(1==attack$P & 7==attack$D),]
    #Seleccionando solo los campos que necesito
    attack_HUNTER <- attack_HUNTER[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
    attack_HUNTER <- as.data.table(attack_HUNTER)
    attack_HUNTER$UNICODE <- gsub("\\s ","", attack_HUNTER$UNICODE)
    
    #-------------------------- 
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de MARIANO MELGAR del consolidado
    attack_JLB_RIVERO <- attack[(1==attack$P & 8==attack$D),]
    #Seleccionando solo los campos que necesito
    attack_JLB_RIVERO <- attack_JLB_RIVERO[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
    #-------------------------- 
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de Yarabamba del consolidado
    attack_Yarabamba <- attack[(1==attack$P & 28==attack$D),]
    #Seleccionando solo los campos que necesito
    attack_Yarabamba <- attack_Yarabamba[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
    #-------------------------- 
        #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de MARIANO MELGAR del consolidado
    attack_ASA<- attack[(1==attack$P & 1==attack$D),]
    #Seleccionando solo los campos que necesito
    attack_ASA <- attack_ASA[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
    #--------------------------
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de PAUCARPATA del consolidado
    attack_PAUCARPATA <- attack[(1==attack$P & 13==attack$D),]
    #Extrayendo solo los datos que necesito 
    attack_PAUCARPATA <- attack_PAUCARPATA[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
 
   #UNIENDO LA BASE DE PAUCARPATA DE AMBOS ROCIADOS 
    attack_PAUCARPATA<-rbind(attack_p_1,attack_p_2)  
    #Seleccionando solo los campos que necesito
    attack_PAUCARPATA <- attack_PAUCARPATA[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO")]
    #-------------------------- 
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de YARABAMABA del consolidado
    attack_yarabamba <- attack_db[(1==attack_db$P & 28==attack_db$D),]
    #Seleccionando solo los campos que necesito
    attack_yarabamba <- attack_yarabamba[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
    #-------------------------- 
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de  LA JOYA  del consolidado
    attack_LA_JOYA <- attack[(1==attack$P & 9==attack$D),]
    #Seleccionando solo los campos que necesito
    attack_LA_JOYA <- attack_LA_JOYA[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
    #-------------------------- 
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de CAYMA del consolidado
    attack_cayma  <- attack[(1==attack$P & 3==attack$D),]
    #Seleccionando solo los campos que necesito
    attack_cayma <- attack_cayma[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
   
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de CAYMA del consolidado
    attack_MIRAFLORES  <- attack[(1==attack$P & 11==attack$D),]
    #Seleccionando solo los campos que necesito
    attack_MIRAFLORES <- attack_MIRAFLORES[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
    
    
    
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de  CHARACATO del consolidado
    attack_CHARACATO  <- attack[(1==attack$P & 5==attack$D),]
    #Seleccionando solo los campos que necesito
    attack_CHARACATO <- attack_CHARACATO[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
 
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de  CHARACATO del consolidado
    attack_SACHACA  <- attack[(1==attack$P & 18==attack$D),]
    #Seleccionando solo los campos que necesito
    attack_SACHACA <- attack_SACHACA[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
    
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de  CHARACATO del consolidado
    attack_TIABAYA  <- attack[(1==attack$P & 24==attack$D),]
    #Seleccionando solo los campos que necesito
    attack_TIABAYA <- attack_TIABAYA[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
    
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de  CHARACATO del consolidado
    attack_SOCABAYA  <- attack[(1==attack$P & 25==attack$D),]
    #Seleccionando solo los campos que necesito
    attack_SOCABAYA <- attack_SOCABAYA[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
     
    #--SOLO CONSOLIDADO--
    #Extrayendo solo datos de  CHARACATO del consolidado
    attack_UCHUMAYO <- attack[(1==attack$P & 23==attack$D),]
    #Seleccionando solo los campos que necesito
    attack_UCHUMAYO <- attack_UCHUMAYO[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]   
  
    
#Verificar si hay duplicados
    casas_aqp<-as.data.table(casas_aqp) 
    DUPLICADOS<-casas_aqp[duplicated(casas_aqp$UNICODE)]
   
    
  #Eliminando duplicados 
    casas_aqp<-casas_aqp[!duplicated(casas_aqp$UNICODE), ]


  #indice_dupli <- casas_aqp[which(duplicated(casas_aqp$UNICODE)),1]
  #duplicados<-casas_aqp[casas_aqp$UNICODE %in% indice_dupli,]
  #duplicados <- duplicados[order(duplicados$UNICODE),]
  #casas_aqp<-casas_aqp[!duplicated(casas_aqp$UNICODE), ]
  
  #indice_dupli <- casas_aqp_total[which(duplicated(casas_aqp_total$UNICODE)),1]
  
  #duplicados<-casas_aqp_total[casas_aqp_total$UNICODE %in% indice_dupli,]
  #duplicados <- duplicados[order(duplicados$UNICODE),]

  

 # casas_aqp_total<-distinct(casas_aqp_total)
  
  #casas_aqp_total[duplicated(casas_aqp_total)]
  
  #casas_aqp_total[!duplicated(casas_aqp_total)]

  #Bases utilizadas en el proceso
  nc_polygon <- nc_melgar
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
  nc_polygon <- nc_UCHUMAYO
  nc_polygon <- nc_MIRAFLORES
  
  old_casa_aqp <- casas_aqp
  
#Convirtiendo de factor a character o numero a caracter
  nc_polygon$ident <- as.character(nc_polygon$ident)
  nc_polygon$long <- as.character(nc_polygon$long)
  nc_polygon$lat <- as.character(nc_polygon$lat)
  old_casa_aqp$D <- as.character(old_casa_aqp$D)
  old_casa_aqp$L <- as.character(old_casa_aqp$L)

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
  inicio <-1 #si tiene 1 es por que es un primer elemento 
  
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
    diff1<-setdiff(attack_mm$UNICODE,aqp_gps_block$UNICODE) # 28 viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_mm$UNICODE) # 3172
  #Interseccion
    interseption<-intersect(attack_mm$UNICODE, aqp_gps_block$UNICODE)#9703
    
  #Merge
    mmelgar_gps_rociado <- merge(aqp_gps_block,attack_mm, all= TRUE, by = "UNICODE")
  #Comprobando
    aux1 <- attack_mm[attack_mm$UNICODE%in%diff1,]#78
    aux2 <- mmelgar_gps_rociado[mmelgar_gps_rociado$UNICODE%in%diff1,]#78
    aux3 <- mmelgar_gps_rociado[mmelgar_gps_rociado$UNICODE%in%diff2,]#3762
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/mmelgar_dif_sir_nogoo.csv",row.names = FALSE)
    
         #--------JUNTANDO CONSOLIDADO CON CUADRAS -----
    #Diferencia C-S v S-C
    diff1<-setdiff(attack_HUNTER$UNICODE,aqp_gps_block$UNICODE) # 19 viviendas las que etsan en el ataque pero no tenemos puntos 
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_HUNTER$UNICODE) # 597 viviendas que hay  en aqp_block
    #Interseccion
    interseption<-intersect(attack_HUNTER$UNICODE, aqp_gps_block$UNICODE)#10365
    #Merge
    HUNTER_gps_rociado <- merge(aqp_gps_block,attack_HUNTER, all= TRUE, by = "UNICODE")
    
    faltantes<-HUNTER_gps_rociado[which(is.na(HUNTER_gps_rociado$LATITUDE))]
    duplicados<-faltantes[duplicated(faltantes$UNICODE)]
    #Comprobando
    aux1 <- attack_HUNTER[attack_HUNTER$UNICODE%in%diff1,]#131 VIVIENDAS 
    aux2 <- HUNTER_gps_rociado[HUNTER_gps_rociado$UNICODE%in%diff1,]
    aux3 <- HUNTER_gps_rociado[HUNTER_gps_rociado$UNICODE%in%diff2,]#597
  
    idff<-setdiff(attack_HUNTER$UNICODE,casas_aqp$UNICODE)
    baux<-attack_HUNTER[attack_HUNTER$UNICODE%in%idff,]#130
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/hunter_dif_sir_nogoo.csv",row.names = FALSE)
    
    
    #-------------------------------------------------------------------------------  
    #Diferencia C-S v S-C
    diff1<-setdiff(attack_JLB_RIVERO$UNICODE,aqp_gps_block$UNICODE) # 1 viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_JLB_RIVERO$UNICODE) # 3090
    #Interseccion
    interseption<-intersect(attack_JLB_RIVERO$UNICODE, aqp_gps_block$UNICODE)#5168
    
    #Merge
    JLB_RIVERO_gps_rociado <- merge(aqp_gps_block,attack_JLB_RIVERO, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_JLB_RIVERO[attack_JLB_RIVERO$UNICODE%in%diff1,]#2
    aux2 <- JLB_RIVERO_gps_rociado[JLB_RIVERO_gps_rociado$UNICODE%in%diff1,]
    aux3 <- JLB_RIVERO_gps_rociado[JLB_RIVERO_gps_rociado$UNICODE%in%diff2,]#3091
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/jbrivero_dif_sir_nogoo.csv",row.names = FALSE)
    
    #_______________________________
    
#---------------------------------------------
    #Diferencia C-S v S-C
    diff1<-setdiff(attack_LA_JOYA$UNICODE,aqp_gps_block$UNICODE) #  933viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_LA_JOYA$UNICODE) # 344
    #Interseccion
    interseption<-intersect(attack_LA_JOYA$UNICODE, aqp_gps_block$UNICODE)#4676
    
    #Merge
    LA_JOYA_gps_rociado <- merge(aqp_gps_block,attack_LA_JOYA, all= TRUE, by = "UNICODE")#10204
    #Comprobando
    aux1 <- attack_LA_JOYA[attack_LA_JOYA$UNICODE%in%diff1,]#1468
    aux2 <- LA_JOYA_gps_rociado[LA_JOYA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- LA_JOYA_gps_rociado[LA_JOYA_gps_rociado$UNICODE%in%diff2,]#344

    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/lajoya_dif_sir_nogoo.csv",row.names = FALSE)
    
    
    #_____________________________________
    
    #uniendo bases 
    diff1<-setdiff(attack_cayma$UNICODE,aqp_gps_block$UNICODE) #  76viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_cayma$UNICODE) # 10276
    #Interseccion
    interseption<-intersect(attack_cayma$UNICODE, aqp_gps_block$UNICODE)#7844
    
    #Merge
    CAYMA_gps_rociado <- merge(aqp_gps_block,attack_cayma, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_cayma[attack_cayma$UNICODE%in%diff1,]#137
    aux2 <- CAYMA_gps_rociado[CAYMA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- CAYMA_gps_rociado[CAYMA_gps_rociado$UNICODE%in%diff2,]#10276
    
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/cayma_dif_sir_nogoo.csv",row.names = FALSE)
 #-----------------------------------------------------------------------------------------   
    #uniendo bases 
    diff1<-setdiff(attack_ASA$UNICODE,aqp_gps_block$UNICODE) #  133viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_ASA$UNICODE) # 14335
    #Interseccion
    interseption<-intersect(attack_ASA$UNICODE, aqp_gps_block$UNICODE)#6563
    
    #Merge
    ASA_gps_rociado <- merge(aqp_gps_block,attack_ASA, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_ASA[attack_ASA$UNICODE%in%diff1,]#236 observaciones 
    aux2 <- ASA_gps_rociado[ASA_gps_rociado$UNICODE%in%diff1,]#236
    aux3 <- ASA_gps_rociado[ASA_gps_rociado$UNICODE%in%diff2,]#14335 
 
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/asa_dif_sir_nogoo.csv",row.names = FALSE)
    
    #_--------------------------------------------------------------------
    #uniendo bases 
    diff1<-setdiff(attack_Yarabamba$UNICODE,aqp_gps_block$UNICODE) #  25viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_Yarabamba$UNICODE) # 176Viviendas 
    #Interseccion
    interseption<-intersect(attack_Yarabamba$UNICODE, aqp_gps_block$UNICODE)#332
    
    #Merge
    YARABAMBA_gps_rociado <- merge(aqp_gps_block,attack_Yarabamba, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_Yarabamba[attack_Yarabamba$UNICODE%in%diff1,]#39 observaciones 
    aux2 <- YARABAMBA_gps_rociado[YARABAMBA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- YARABAMBA_gps_rociado[YARABAMBA_gps_rociado$UNICODE%in%diff2,]#176
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/yarabamba_dif_sir_nogoo.csv",row.names = FALSE)
 #------------------------------------------------------------------------   
    #uniendo bases 
    diff1<-setdiff(attack_CHARACATO$UNICODE,aqp_gps_block$UNICODE) #  580viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_CHARACATO$UNICODE) # 917Viviendas 
    #Interseccion
    interseption<-intersect(attack_CHARACATO$UNICODE, aqp_gps_block$UNICODE)#2548
    
    #Merge
    CHARACATO_gps_rociado <- merge(aqp_gps_block,attack_CHARACATO, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_CHARACATO[attack_CHARACATO$UNICODE%in%diff1,]#712 observaciones 
    aux2 <- CHARACATO_gps_rociado[CHARACATO_gps_rociado$UNICODE%in%diff1,]
    aux3 <- CHARACATO_gps_rociado[CHARACATO_gps_rociado$UNICODE%in%diff2,]#917   
 
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/characato_dif_sir_nogoo.csv",row.names = FALSE)
    
    #------------------------------------------------------------------------------------      
    #uniendo bases 
    diff1<-setdiff(attack_SACHACA$UNICODE,aqp_gps_block$UNICODE) #  172viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_SACHACA$UNICODE) # 1547Viviendas 
    #Interseccion
    interseption<-intersect(attack_SACHACA$UNICODE, aqp_gps_block$UNICODE)#2991
    
    #Merge
    SACHACA_gps_rociado <- merge(aqp_gps_block,attack_SACHACA, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_SACHACA[attack_SACHACA$UNICODE%in%diff1,]#338 observaciones 
    aux2 <- SACHACA_gps_rociado[SACHACA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- SACHACA_gps_rociado[SACHACA_gps_rociado$UNICODE%in%diff2,]#1547
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/hunter_dif_sir_nogoo.csv",row.names = FALSE)
 #--------------------------------------------------------------------------------   
    #uniendo bases 
    diff1<-setdiff(attack_TIABAYA$UNICODE,aqp_gps_block$UNICODE) #  113viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_TIABAYA$UNICODE) # 26Viviendas 
    #Interseccion
    interseption<-intersect(attack_TIABAYA$UNICODE, aqp_gps_block$UNICODE)#3165
    
    #Merge
    TIABAYA_gps_rociado <- merge(aqp_gps_block,attack_TIABAYA, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_TIABAYA[attack_TIABAYA$UNICODE%in%diff1,]#229 observaciones 
    aux2 <- TIABAYA_gps_rociado[TIABAYA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- TIABAYA_gps_rociado[TIABAYA_gps_rociado$UNICODE%in%diff2,]#26
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/TIABAYA_dif_sir_nogoo.csv",row.names = FALSE)
    
    #-----------------------------------------------------------------------------------   
    #uniendo bases 
    diff1<-setdiff(attack_SOCABAYA$UNICODE,aqp_gps_block$UNICODE) #  109viviendas que estan en el rociado pero no en en los puntos con manzana
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_SOCABAYA$UNICODE) # 5345Viviendas que estan en aqp pero que no estan en el rociado  
    #Interseccion
    interseption<-intersect(attack_SOCABAYA$UNICODE, aqp_gps_block$UNICODE)#10172 que coinciden en amabas tablas 
    
    #Merge
    SOCABAYA_gps_rociado <- merge(aqp_gps_block,attack_SOCABAYA, all= TRUE, by = "UNICODE")#24966 resgistros entre roaciadas 
    #Comprobando
    aux1 <- attack_SOCABAYA[attack_SOCABAYA$UNICODE%in%diff1,]#201 observaciones 
    aux2 <- SOCABAYA_gps_rociado[SOCABAYA_gps_rociado$UNICODE%in%diff1,]#201 observaciones 
    aux3 <- SOCABAYA_gps_rociado[SOCABAYA_gps_rociado$UNICODE%in%diff2,]#5345

    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/socabaya_dif_sir_nogoo.csv",row.names = FALSE)
    #------------------------------------------------------------------------------    
    #uniendo bases PAUCARPATA 2006-2009
    diff1<-setdiff(attack_PAUCARPATA$UNICODE,aqp_gps_block$UNICODE) #  701viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_PAUCARPATA$UNICODE) # 7741Viviendas 
    #Interseccion
    interseption<-intersect(attack$UNICODE,aqp_gps_block$UNICODE)#19872
    
    #Merge
    PAUCARPATA_gps_rociado <- merge(aqp_gps_block,attack_PAUCARPATA, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_PAUCARPATA[attack_PAUCARPATA$UNICODE%in%diff1,]# 805 observaciones 
    aux2 <- PAUCARPATA_gps_rociado[PAUCARPATA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- PAUCARPATA_gps_rociado[PAUCARPATA_gps_rociado$UNICODE%in%diff2,]#7741 houses 
    
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/PAUCARPATA_dif_sir_nogoo.csv",row.names = FALSE)
 #--------------------------------------------------------------------------   
    
    #UNIENDO BASES PAUCARPATA 2009-2015
    #uniendo bases 
    diff1<-setdiff(attack_p_2$UNICODE,aqp_gps_block$UNICODE) #  62viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_p_2$UNICODE) # 22170Viviendas 
    #Interseccion
    interseption<-intersect(attack_p_2$UNICODE, aqp_gps_block$UNICODE)#5119
    
    #Merge
    attack_p_2_gps_rociado <- merge(aqp_gps_block,attack_p_2, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_p_2[attack_p_2$UNICODE%in%diff1,]#83 observaciones 
    aux2 <- attack_p_2_gps_rociado[attack_p_2_gps_rociado$UNICODE%in%diff1,]
    aux3 <- attack_p_2_gps_rociado[attack_p_2_gps_rociado$UNICODE%in%diff2,]#22170 
    
    #------------------------------------------------------------------------------------      
    #uniendo bases 
    diff1<-setdiff(attack_UCHUMAYO$UNICODE,aqp_gps_block$UNICODE) #  123viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack$UNICODE) # 1215Viviendas 
    #Interseccion
    interseption<-intersect(attack_UCHUMAYO$UNICODE, aqp_gps_block$UNICODE)#1846
    #Merge
    UCHUMAYO_gps_rociado <- merge(aqp_gps_block,attack_UCHUMAYO, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_UCHUMAYO[attack_UCHUMAYO$UNICODE%in%diff1,]#243 observaciones 
    aux2 <- UCHUMAYO_gps_rociado[UCHUMAYO_gps_rociado$UNICODE%in%diff1,]
    aux3 <- UCHUMAYO_gps_rociado[UCHUMAYO_gps_rociado$UNICODE%in%diff2,]#1215
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/uchumayo_dif_sir_nogoo.csv",row.names = FALSE)
    
    #------------------------------------------------------------------------------------      
    #uniendo bases 
    diff1<-setdiff(attack_MIRAFLORES$UNICODE,aqp_gps_block$UNICODE) #  123viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_MIRAFLORES$UNICODE) # 4234Viviendas 
    #Interseccion
    interseption<-intersect(attack_MIRAFLORES$UNICODE, aqp_gps_block$UNICODE)#8278
    #Merge
    MIRAFLORES_gps_rociado <- merge(aqp_gps_block,attack_MIRAFLORES, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_MIRAFLORES[attack_MIRAFLORES$UNICODE%in%diff1,]#243 observaciones 
    aux2 <- MIRAFLORES_gps_rociado[MIRAFLORES_gps_rociado$UNICODE%in%diff1,]
    aux3 <- MIRAFLORES_gps_rociado[MIRAFLORES_gps_rociado$UNICODE%in%diff2,]#4234
    
#--------------------------------------------------------------------  
#Imprimiendo Resultados ROCIADOS DEL 2009 EN ADELANTE 
#--------------------------------------------------------------------
  
  #Resultado de las viviendas de Mariano Melgar que tienen numero de cuadra
  write.csv(mmelgar_gps_rociado,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/mmelgar_gps_rociado_JUN_2017.csv", row.names = FALSE)
  #Resultado de las viviendas de Mariano Melgar que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_MARIANOMELGAR_JUN_2017.csv", row.names = FALSE)
 
  #Resultado de las viviendas de CAYMA  que tienen numero de cuadra
  write.csv(CAYMA_gps_rociado,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/CAYMA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de CAYMA  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_CAYMA.csv", row.names = FALSE)
  
  setwd('~/claudia codigos r')
  #Resultado de las viviendas de ASA que tienen numero de cuadra
  write.csv(ASA_gps_rociado,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/ASA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de ASA que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_ASA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de YARABAMBA  que tienen numero de cuadra
  write.csv(YARABAMBA_gps_rociado,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/YARABAMBA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de YARABAMBA  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_YARABAMBA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de CHARACATO  que tienen numero de cuadra
  write.csv(CHARACATO_gps_rociado,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/CHARACATO_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de CHARACATO  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_CHARACATO.csv", row.names = FALSE)
  
  #Resultado de las viviendas de CHARACATO  que tienen numero de cuadra
  write.csv(PAUCARPATA_gps_rociado,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/PAUCARPATA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de CHARACATO  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_PAUCARPATA.csv", row.names = FALSE)
  
  
  
  
  #Resultado de las viviendas de HUNTER que tienen numero de cuadra
  write.csv(HUNTER_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/HUNTER_gps_rociado_JUN.csv", row.names = FALSE)
  #Resultado de las viviendas de HUNTER que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_HUNTER.csv", row.names = FALSE)
  
  #Resultado de las viviendas de JLB Y RIVERO que tienen numero de cuadra
  write.csv(JLB_RIVERO_gps_rociado,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/JLB_RIVERO_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de HUNTER que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_JLB.csv", row.names = FALSE)
  
  #Resultado de las viviendas de LA JOYA  que tienen numero de cuadra
  write.csv(LA_JOYA_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/LA_JOYA_gps_rociado_JUN.csv", row.names = FALSE)
  #Resultado de las viviendas de HUNTER que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_LAJOYA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de CAYMA  que tienen numero de cuadra
  write.csv(CAYMA_gps_rociado,"~/claudia codigos r/CAYMA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de CAYMA  que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_CAYMA.csv", row.names = FALSE)
 
  #Resultado de las viviendas de ASA que tienen numero de cuadra
  write.csv(ASA_gps_rociado,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/ASA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de ASA que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_ASA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de YARABAMBA  que tienen numero de cuadra
  write.csv(YARABAMBA_gps_rociado,"~/claudia codigos r/YARABAMBA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de YARABAMBA  que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_YARABAMBA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de CHARACATO  que tienen numero de cuadra
  write.csv(CHARACATO_gps_rociado,"~/claudia codigos r/CHARACATO_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de CHARACATO  que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_CHARACATO.csv", row.names = FALSE)
  
  #Resultado de las viviendas de SACHACA  que tienen numero de cuadra
  write.csv(SACHACA_gps_rociado,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/SACHACA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de SACHACA  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_SACHACA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de TIABAYA  que tienen numero de cuadra
  write.csv(TIABAYA_gps_rociado,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/TIABAYA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de TIABAYA  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_TIABAYA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de TIABAYA  que tienen numero de cuadra
  write.csv(SOCABAYA_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/SOCABAYA_gps_rociado_JUNIO.csv", row.names = FALSE)
  #Resultado de las viviendas de TIABAYA  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_SOCABAYA1.csv", row.names = FALSE)
  
 #Resultado de las viviendas de UCHUMAYO   que tienen numero de cuadra
  write.csv(MIRAFLORES_gps_rociado,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/MIRAFLORES_rociado.csv", row.names = FALSE)
  
  #Resultado de las viviendas de TIABAYA  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_UCHUMAYO.csv", row.names = FALSE) 
  
  #Resultado de las viviendas de PAUCARPATA   que tienen numero de cuadra
  write.csv(UCHUMAYO_gps_rociado,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/UCHUMAYO_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de TIABAYA  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_UCHUMAYO.csv", row.names = FALSE) 
  
  
  