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
   attack<-read.csv("~/CLAUDIA-DATOS-/claudia codigos r/ATTACK_2006_2015/ATTACK_2006_2016.csv")
   attack$I_TRIAT<-as.numeric(attack$I_TRIAT)
   attack$P_TRIAT<-as.numeric(attack$P_TRIAT)
   
   attack$UNICODE <- gsub('\\s+', '',attack$UNICODE)
   attack$IN_TRI <- gsub('\\s+', '',attack$IN_TRI)
   attack$PE_TRI<- gsub('\\s+', '',attack$PE_TRI)
   
   
   DUPLICADOS<-attack[duplicated(attack$UNICODE),]
   
   attack<-attack[!duplicated(attack$UNICODE), ]
   
   table(attack$TRATADA=='1')
   
   table(attack$CICLO=='1'& attack$TRATADA=='1')
   table(attack$CICLO=='2'& attack$TRATADA=='1')
   
 #leyendo 
   encuestas<-fread("~/Participation/merge_participacion/bd/encuestas_pre_rociado_2015.csv")
   table(encuestas$L)
   #LEYENDO LOS ARCHIVOS QUE CONTIENEn LOS GP/S DE CASAS NORMALES Y ADICIONADAS .
  
     casas_aqp<-fread("~/PETM-shiny/unicode_numberMzn/AREQUIPA_GPS_GOOGLE/AQP_GPS_GOOGLE_EARTH_PUNTOS_30_abril_2019.csv")
     casas_aqp$UNICODE <- gsub('\\s+', '',casas_aqp$UNICODE)
  #OBTENIENDO LAS BASES DE MANZANA  HUNTER ,JLB Y RIVERO ,LA JOYA ,SACHACA,UCHUMAYO,TIABAYA Y SOCABAYA 
     nc_CCOLORADO<-fread("~/PETM-shiny/unicode_numberMzn/Manzanas _Arequipa/Cerro_Colorado_correcciones/Cerro Colorado MZ_actualizado31012019.csv")
     nc_CCOLORADO[nc_CCOLORADO=="NA"]<-NA
    
      nc_CCOLORADO<-as.data.table(nc_CCOLORADO)
     nc_CCOLORADO<-nc_CCOLORADO[,c('codeloc','block'):=tstrsplit(ident,'-', fixed=TRUE)]
     nc_CCOLORADO<-nc_CCOLORADO[codeloc=='1.4.1'|codeloc=='1.4.2'|codeloc=='1.4.3'|codeloc=='1.4.4'|codeloc=='1.4.5'|codeloc=='1.4.14'|codeloc=='1.4.15'
                                |codeloc=='1.4.6'|codeloc=='1.4.8'|codeloc=='1.4.9'|codeloc=='1.4.10'|codeloc=='1.4.11'|
                                  codeloc=='1.4.12'|codeloc=='1.4.13'|codeloc=='1.4.16'|codeloc=='1.4.17'|codeloc=='1.4.18'|codeloc=='1.4.19'|codeloc=='1.4.20'|
                                codeloc=='1.4.21'|codeloc=='1.4.22'|codeloc=='1.4.23'|codeloc=='1.4.24'|codeloc=='1.4.26'|
                                codeloc=='1.4.34'|codeloc=='1.4.35'|codeloc=='1.4.36'|codeloc=='1.4.37'|codeloc=='1.4.38'|codeloc=='1.4.39'|
                                  codeloc=='1.4.40'|codeloc=='1.4.41'|codeloc=='1.4.42'|codeloc=='1.4.43'|codeloc=='1.4.44'|codeloc=='1.4.45'|
                                  codeloc=='1.4.46'|codeloc=='1.4.47'|codeloc=='1.4.48'|codeloc=='1.4.49'|codeloc=='1.4.51'|
                                  codeloc=='1.4.52'|codeloc=='1.4.53'|codeloc=='1.4.69'|codeloc=='1.4.70'|codeloc=='1.4.71'|
                                  codeloc=='1.4.73'|codeloc=='1.4.74'|codeloc=='1.4.75'|codeloc=='1.4.76'|codeloc=='1.4.77'|
                                  codeloc=='1.4.83'|codeloc=='1.4.84'|codeloc=='1.4.85'|codeloc=='1.4.93']
    
     nc_CCOLORADO<-nc_CCOLORADO[!codeloc=='1.4.233']
     nc_CCOLORADO<-nc_CCOLORADO[!codeloc=='1.4.205']
     nc_CCOLORADO<-nc_CCOLORADO[!codeloc=='1.4.97']
     
    nc_HUNTER<-read.csv("~/PETM-shiny/unicode_numberMzn/Manzanas _Arequipa/Hunter_correcciones/Hunter_Mz_16FEB2017.csv",sep = ";")
  nc_HUNTER[nc_HUNTER==" "]<-"NA"
  
  nc_JLB_RIVERO<-read.csv("~/Downloads/JLByRivero_Mz_26JUN2017.csv",sep = ";")
  nc_YARABAMBA<-read.csv("~/CLAUDIA-DATOS-/PUNTOS  Y MANZANAS CORREGIDOS/Yarabamba_Mz_corregido.csv",sep = ",")
  
  
  nc_LAJOYA<-read.csv("~/CLAUDIA-DATOS-/PUNTOS  Y MANZANAS CORREGIDOS/La_Joya_Mz_corregido.csv")
  nc_SACHACA<-read.csv("~/Downloads/Sachaca_Mz_26JUN2017 (1).csv",sep = ';')
  nc_SACHACA[nc_SACHACA== "NA"]<-NA
  nc_UCHUMAYO<-read.csv("~/CLAUDIA-DATOS-/PUNTOS  Y MANZANAS CORREGIDOS/Uchumayo_Mz_07FEB2017_SIN20.csv",sep = ",")
  
  nc_TIABAYA<-fread("~/Downloads/Tiabaya_Mz_26JUN2017.csv",sep = ';')
  nc_TIABAYA<-nc_TIABAYA[ident=='1.24.9.-Los_Perales',ident:='1.24.9-1']
  nc_TIABAYA<-nc_TIABAYA[ident=='1.24.11A-Cural_de_Tiabaya',ident:='1.24.11A-1']
  nc_TIABAYA<-nc_TIABAYA[ident=='1.24.12-El_Molino',ident:='1.24.12-1']
  nc_TIABAYA<-nc_TIABAYA[ident=='1.24.13A-Alto_Tunales',ident:='1.24.13A-1']
  nc_TIABAYA<-nc_TIABAYA[ident=='1.24.3A-Patasagua_Bajo',ident:='1.24.3A-1']
  
  
                           
                         
                       
  
  nc_SOCABAYA<-read.csv("~/Downloads/Manzanas_Socabaya_19JUN2017_actualizado.csv",sep = ';')
  
  #OBTENIENDO LAS BASES DE  MANZANA ASA ,CAYMA ,CERRO COLORADO,CHARACATO ,MARIANO MELGAR,MIRAFLORES,MOLLEBAYA
  #PAUCARPATA(2)Y YARABAMBA. 
  nc_melgar<-read.csv("~/Downloads/Manzanas_MnoMelgar_08JUN2017_actualizados..csv")
  nc_melgar<-as.data.table(nc_melgar)
  nc_melgar[ident == "1.10-16-14", ident := "1.10.16-14"]
  nc_melgar<-as.data.frame(nc_melgar)
  
  nc_CAYMA <-read.csv("~/CLAUDIA-DATOS-/PUNTOS  Y MANZANAS CORREGIDOS/Cayma_mz_corregido.csv", sep = ",")
  nc_ASA <-read.csv("~/CLAUDIA-DATOS-/PUNTOS  Y MANZANAS CORREGIDOS/ASA_Mz_corregido.csv", sep = ",")
  nc_YARABAMBA<-read.csv ("~/CLAUDIA-DATOS-/PUNTOS  Y MANZANAS CORREGIDOS/Yarabamba_Mz_corregido.csv")
  nc_MIRAFLORES <-read.csv ("~/CLAUDIA-DATOS-/PUNTOS  Y MANZANAS CORREGIDOS/Miraflores_mz_corregido.csv")
  #nc_CERRO_COLORADO <-read.csv("~/claudia codigos r/Manzanas _Arequipa/Cerro Colorado/Cerro Colorado MZ.csv", sep = ";")
  nc_CHARACATO <-read.csv("~/Downloads/Characato_Mz_26JUN2017.csv",sep = ';')
  
  nc_PAUCARPATA<-read.csv("~/Downloads/Paucarpata_Mz_25SET2017.csv",sep = ',')
  nc_PAUCARPATA<- as.data.table(nc_PAUCARPATA)
  nc_PAUCARPATA[ident=='1.13-51-90',ident:='1.13.51-90']
  nc_PAUCARPATA<-nc_PAUCARPATA[!ident=="1.13.71-1"]
  
  #---------------------------------------------------------------------------------- 
  
   #--SOLO CONSOLIDADO--
  #Extrayendo solo datos de MARIANO MELGAR del consolidado
  attack_mm <- attack[(1==attack$P & 10==attack$D),]
  #Seleccionando solo los campos que necesito
  attack_mm <- attack_mm[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
  attack_mm <- as.data.table(attack_mm)
  attack_mm$UNICODE <- gsub("\\s+", "", attack_mm$UNICODE)
  
  #--SOLO CONSOLIDADO--
  #Extrayendo solo datos de cerro colorado del consolidado
  attack_cc <- attack[(1==attack$P & 4==attack$D),]
  #Seleccionando solo los campos que necesito
  
  attack_cc <- attack_cc[,c("UNICODE", "I_TRIAT", "P_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","LOCAL_PUBLICO","LOTE_VACIO")]
  attack_cc <- as.data.table(attack_cc)
  attack_cc$UNICODE <- gsub("\\s+", "", attack_cc$UNICODE)
  
  
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
  nc_polygon <- nc_CCOLORADO
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
  diff1<-setdiff(attack_cc$UNICODE,aqp_gps_block$UNICODE) # 55 viviendas
  diff2<-setdiff(aqp_gps_block$UNICODE,attack_cc$UNICODE) # 3751
  #Interseccion
  interseption<-intersect(attack_cc$UNICODE, aqp_gps_block$UNICODE)#9692
  
  #Merge
  ccerro_gps_rociado <- merge(aqp_gps_block,attack_cc, all= TRUE, by = "UNICODE")
  #Comprobando
  aux1 <- attack_cc[attack_cc$UNICODE%in%diff1,]#408 auxuliares la mayoria
  aux2 <- ccerro_gps_rociado[ccerro_gps_rociado$UNICODE%in%diff1,]#408
  aux3 <- ccerro_gps_rociado[ccerro_gps_rociado$UNICODE%in%diff2,]#48172
  
  ccerro_gps_rociado_NA<-ccerro_gps_rociado[is.na(ccerro_gps_rociado$LATITUDE)]
  DUPLICADOS<-ccerro_gps_rociado_NA[duplicated(ccerro_gps_rociado_NA$UNICODE)]
  ccerro_gps_rociado_NA<-ccerro_gps_rociado_NA[!duplicated(ccerro_gps_rociado_NA$UNICODE), ]
  
  
  write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/mmelgar_dif_sir_nogoo.csv",row.names = FALSE)
  #Resultado de las viviendas de ASA que tienen numero de cuadra
  write.csv(ccerro_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/cerro_gps_rociado_may2019.csv", row.names = FALSE)
  #Resultado de las viviendas de ASA que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_ccmay2019.csv", row.names = FALSE)
  
  
  
  
  #--------JUNTANDO CONSOLIDADO CON CUADRAS -----
  #Diferencia C-S v S-C
    diff1<-setdiff(attack_mm$UNICODE,aqp_gps_block$UNICODE) # 55 viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_mm$UNICODE) # 3751
  #Interseccion
    interseption<-intersect(attack_mm$UNICODE, aqp_gps_block$UNICODE)#9692
    
  #Merge
    mmelgar_gps_rociado <- merge(aqp_gps_block,attack_mm, all= TRUE, by = "UNICODE")
  #Comprobando
    aux1 <- attack_mm[attack_mm$UNICODE%in%diff1,]#78
    aux2 <- mmelgar_gps_rociado[mmelgar_gps_rociado$UNICODE%in%diff1,]#78
    aux3 <- mmelgar_gps_rociado[mmelgar_gps_rociado$UNICODE%in%diff2,]#3762
    
    mmelgar_gps_rociado_NA<-mmelgar_gps_rociado[is.na(mmelgar_gps_rociado$LATITUDE)]
    DUPLICADOS<-mmelgar_gps_rociado_NA[duplicated(mmelgar_gps_rociado_NA$UNICODE)]
    mmelgar_gps_rociado_NA<-mmelgar_gps_rociado_NA[!duplicated(mmelgar_gps_rociado_NA$UNICODE), ]
    
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/mmelgar_dif_sir_nogoo.csv",row.names = FALSE)
    
         #--------JUNTANDO CONSOLIDADO CON CUADRAS -----
    #Diferencia C-S v S-C
    diff1<-setdiff(attack_HUNTER$UNICODE,aqp_gps_block$UNICODE) # 134 viviendas las que etsan en el ataque pero no tenemos puntos 
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_HUNTER$UNICODE) # 609 viviendas que hay  en aqp_block
    #Interseccion
    interseption<-intersect(attack_HUNTER$UNICODE, aqp_gps_block$UNICODE)#10379
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
    diff1<-setdiff(attack_JLB_RIVERO$UNICODE,aqp_gps_block$UNICODE) # 3 viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_JLB_RIVERO$UNICODE) # 3090
    #Interseccion
    interseption<-intersect(attack_JLB_RIVERO$UNICODE, aqp_gps_block$UNICODE)#5168
    
    #Merge
    JLB_RIVERO_gps_rociado <- merge(aqp_gps_block,attack_JLB_RIVERO, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_JLB_RIVERO[attack_JLB_RIVERO$UNICODE%in%diff1,]#6
    aux2 <- JLB_RIVERO_gps_rociado[JLB_RIVERO_gps_rociado$UNICODE%in%diff1,]
    aux3 <- JLB_RIVERO_gps_rociado[JLB_RIVERO_gps_rociado$UNICODE%in%diff2,]#3091
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/jbrivero_dif_sir_nogoo.csv",row.names = FALSE)
    
    #_______________________________
    
#---------------------------------------------
    #Diferencia C-S v S-C
    diff1<-setdiff(attack_LA_JOYA$UNICODE,aqp_gps_block$UNICODE) #  973viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_LA_JOYA$UNICODE) # 343
    #Interseccion
    interseption<-intersect(attack_LA_JOYA$UNICODE, aqp_gps_block$UNICODE)#4636
    
    #Merge
    LA_JOYA_gps_rociado <- merge(aqp_gps_block,attack_LA_JOYA, all= TRUE, by = "UNICODE")#10204
    #Comprobando
    aux1 <- attack_LA_JOYA[attack_LA_JOYA$UNICODE%in%diff1,]#1468
    aux2 <- LA_JOYA_gps_rociado[LA_JOYA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- LA_JOYA_gps_rociado[LA_JOYA_gps_rociado$UNICODE%in%diff2,]#344

    LA_JOYA_gps_rociado_NA<-LA_JOYA_gps_rociado[is.na(LA_JOYA_gps_rociado$LATITUDE)]
    DUPLICADOS<-LA_JOYA_gps_rociado_NA[duplicated(LA_JOYA_gps_rociado_NA$UNICODE)]
    LA_JOYA_gps_rociado_NA<-LA_JOYA_gps_rociado_NA[!duplicated(LA_JOYA_gps_rociado_NA$UNICODE), ]
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/lajoya_dif_sir_nogoo.csv",row.names = FALSE)
    
    
    #_____________________________________
    
    #uniendo bases 
    diff1<-setdiff(attack_cayma$UNICODE,aqp_gps_block$UNICODE) #  91viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_cayma$UNICODE) # 10276
    #Interseccion
    interseption<-intersect(attack_cayma$UNICODE, aqp_gps_block$UNICODE)#7829
    
    #Merge
    CAYMA_gps_rociado <- merge(aqp_gps_block,attack_cayma, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_cayma[attack_cayma$UNICODE%in%diff1,]#137
    aux2 <- CAYMA_gps_rociado[CAYMA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- CAYMA_gps_rociado[CAYMA_gps_rociado$UNICODE%in%diff2,]#10276
    
    CAYMA_gps_rociado_NA<-CAYMA_gps_rociado[is.na(CAYMA_gps_rociado$LATITUDE)]
    DUPLICADOS<-CAYMA_gps_rociado_NA[duplicated(CAYMA_gps_rociado_NA$UNICODE)]
    CAYMA_gps_rociado_NA<-CAYMA_gps_rociado_NA[!duplicated(CAYMA_gps_rociado_NA$UNICODE), ]
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/cayma_dif_sir_nogoo.csv",row.names = FALSE)
 #-----------------------------------------------------------------------------------------   
    #uniendo bases 
    diff1<-setdiff(attack_ASA$UNICODE,aqp_gps_block$UNICODE) #  133viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_ASA$UNICODE) # 14369
    #Interseccion
    interseption<-intersect(attack_ASA$UNICODE, aqp_gps_block$UNICODE)#6667
    
    #Merge
    ASA_gps_rociado <- merge(aqp_gps_block,attack_ASA, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_ASA[attack_ASA$UNICODE%in%diff1,]#72 observaciones 
    aux2 <- ASA_gps_rociado[ASA_gps_rociado$UNICODE%in%diff1,]#72
    aux3 <- ASA_gps_rociado[ASA_gps_rociado$UNICODE%in%diff2,]#14369 
 
    ASA_gps_rociado_NA<-ASA_gps_rociado[is.na(ASA_gps_rociado$LATITUDE)]
    DUPLICADOS<-ASA_gps_rociado_NA[duplicated(ASA_gps_rociado_NA$UNICODE)]
    ASA_gps_rociado_NA<-ASA_gps_rociado_NA[!duplicated(ASA_gps_rociado_NA$UNICODE), ]
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/asa_dif_sir_nogoo.csv",row.names = FALSE)
    
    #_--------------------------------------------------------------------
    #uniendo bases 
    diff1<-setdiff(attack_Yarabamba$UNICODE,aqp_gps_block$UNICODE) #  27viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_Yarabamba$UNICODE) # 176Viviendas 
    #Interseccion
    interseption<-intersect(attack_Yarabamba$UNICODE, aqp_gps_block$UNICODE)#330
    
    #Merge
    YARABAMBA_gps_rociado <- merge(aqp_gps_block,attack_Yarabamba, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_Yarabamba[attack_Yarabamba$UNICODE%in%diff1,]#39 observaciones 
    aux2 <- YARABAMBA_gps_rociado[YARABAMBA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- YARABAMBA_gps_rociado[YARABAMBA_gps_rociado$UNICODE%in%diff2,]#176
    
    YARABAMBA_gps_rociado_NA<-YARABAMBA_gps_rociado[is.na(YARABAMBA_gps_rociado$LATITUDE)]
    DUPLICADOS<-YARABAMBA_gps_rociado_NA[duplicated(YARABAMBA_gps_rociado_NA$UNICODE)]
    
    YARABAMBA_gps_rociado_NA<-YARABAMBA_gps_rociado_NA[!duplicated(YARABAMBA_gps_rociado_NA$UNICODE), ]
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/yarabamba_dif_sir_nogoo.csv",row.names = FALSE)
 #------------------------------------------------------------------------   
    #uniendo bases 
    diff1<-setdiff(attack_CHARACATO$UNICODE,aqp_gps_block$UNICODE) #  565viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_CHARACATO$UNICODE) # 949 
    #Interseccion
    interseption<-intersect(attack_CHARACATO$UNICODE, aqp_gps_block$UNICODE)#2557
    
    #Merge
    CHARACATO_gps_rociado <- merge(aqp_gps_block,attack_CHARACATO, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_CHARACATO[attack_CHARACATO$UNICODE%in%diff1,]#712 observaciones 
    aux2 <- CHARACATO_gps_rociado[CHARACATO_gps_rociado$UNICODE%in%diff1,]
    aux3 <- CHARACATO_gps_rociado[CHARACATO_gps_rociado$UNICODE%in%diff2,]#917   
    
    
    CHARACATO_gps_rociado_NA<-CHARACATO_gps_rociado[is.na(CHARACATO_gps_rociado$LATITUDE)]
    DUPLICADOS<-CHARACATO_gps_rociado_NA[duplicated(CHARACATO_gps_rociado_NA$UNICODE)]
    
    CHARACATO_gps_rociado_NA<-CHARACATO_gps_rociado_NA[!duplicated(CHARACATO_gps_rociado_NA$UNICODE), ]
    write.csv(CHARACATO_gps_rociado,"~/PETM-shiny/autoModel/model/input/MERGES_BLOCKS_GPS_ROCIADO/Characato_gps_rociado.csv")
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/characato_dif_sir_nogoo.csv",row.names = FALSE)
    
    #------------------------------------------------------------------------------------      
    #uniendo bases 
    diff1<-setdiff(attack_SACHACA$UNICODE,aqp_gps_block$UNICODE) #  3viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_SACHACA$UNICODE) # 2028viviendas 
    #Interseccion
    interseption<-intersect(attack_SACHACA$UNICODE, aqp_gps_block$UNICODE)#3160
    
    #Merge
    SACHACA_gps_rociado <- merge(aqp_gps_block,attack_SACHACA, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_SACHACA[attack_SACHACA$UNICODE%in%diff1,]#6 observaciones 
    aux2 <- SACHACA_gps_rociado[SACHACA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- SACHACA_gps_rociado[SACHACA_gps_rociado$UNICODE%in%diff2,]#2028
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/hunter_dif_sir_nogoo.csv",row.names = FALSE)
 #--------------------------------------------------------------------------------   
    #uniendo bases 
    diff1<-setdiff(attack_TIABAYA$UNICODE,aqp_gps_block$UNICODE) #  9viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_TIABAYA$UNICODE) # 26Viviendas 
    #Interseccion
    interseption<-intersect(attack_TIABAYA$UNICODE, aqp_gps_block$UNICODE)#3269
    
    #Merge
    TIABAYA_gps_rociado <- merge(aqp_gps_block,attack_TIABAYA, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_TIABAYA[attack_TIABAYA$UNICODE%in%diff1,]#208 observaciones 
    aux2 <- TIABAYA_gps_rociado[TIABAYA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- TIABAYA_gps_rociado[TIABAYA_gps_rociado$UNICODE%in%diff2,]#26
    TIABAYA_gps_rociado_NA<-TIABAYA_gps_rociado[is.na(TIABAYA_gps_rociado$LATITUDE)]
    DUPLICADOS<-TIABAYA_gps_rociado_NA[duplicated(TIABAYA_gps_rociado_NA$UNICODE)]
    TIABAYA_gps_rociado_NA<-TIABAYA_gps_rociado_NA[!duplicated(TIABAYA_gps_rociado_NA$UNICODE), ]
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/TIABAYA_dif_sir_nogoo.csv",row.names = FALSE)
    
    #-----------------------------------------------------------------------------------   
    #uniendo bases 
    diff1<-setdiff(attack_SOCABAYA$UNICODE,aqp_gps_block$UNICODE) #  37VI que estan en el rociado pero no en en los puntos con manzana
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_SOCABAYA$UNICODE) # 5442 VIVIENDAS que estan en aqp pero que no estan en el rociado  
    #Interseccion
    interseption<-intersect(attack_SOCABAYA$UNICODE, aqp_gps_block$UNICODE)#10244 que coinciden en amabas tablas 
    
    #Merge
    SOCABAYA_gps_rociado <- merge(aqp_gps_block,attack_SOCABAYA, all= TRUE, by = "UNICODE")#24966 resgistros entre roaciadas 
    #Comprobando
    aux1 <- attack_SOCABAYA[attack_SOCABAYA$UNICODE%in%diff1,]#74 observaciones 
    aux2 <- SOCABAYA_gps_rociado[SOCABAYA_gps_rociado$UNICODE%in%diff1,]#74 observaciones 
    aux3 <- SOCABAYA_gps_rociado[SOCABAYA_gps_rociado$UNICODE%in%diff2,]#5442

    SOCABAYA_gps_rociado_NA<-SOCABAYA_gps_rociado[is.na(SOCABAYA_gps_rociado$LATITUDE)]
    DUPLICADOS<-SOCABAYA_gps_rociado_NA[duplicated(SOCABAYA_gps_rociado_NA$UNICODE)]
    SOCABAYA_gps_rociado_NA<-SOCABAYA_gps_rociado_NA[!duplicated(SOCABAYA_gps_rociado_NA$UNICODE), ]
    
    
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/socabaya_dif_sir_nogoo.csv",row.names = FALSE)
    #------------------------------------------------------------------------------    
    #uniendo bases PAUCARPATA 2006-2009
    diff1<-setdiff(attack_PAUCARPATA$UNICODE,aqp_gps_block$UNICODE) #  73viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_PAUCARPATA$UNICODE) # 6880Viviendas 
    #Interseccion
    interseption<-intersect(attack$UNICODE,aqp_gps_block$UNICODE)#19872
    
    #Merge
    PAUCARPATA_gps_rociado <- merge(aqp_gps_block,attack_PAUCARPATA, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_PAUCARPATA[attack_PAUCARPATA$UNICODE%in%diff1,]# 805 observaciones 
    aux2 <- PAUCARPATA_gps_rociado[PAUCARPATA_gps_rociado$UNICODE%in%diff1,]
    aux3 <- PAUCARPATA_gps_rociado[PAUCARPATA_gps_rociado$UNICODE%in%diff2,]#6880 houses 
    
    PAUCARPATA_gps_rociado_NA<-PAUCARPATA_gps_rociado[is.na(PAUCARPATA_gps_rociado$LATITUDE)]
    DUPLICADOS<-PAUCARPATA_gps_rociado_NA[duplicated(PAUCARPATA_gps_rociado_NA$UNICODE)]
    PAUCARPATA_gps_rociado_NA<-PAUCARPATA_gps_rociado_NA[!duplicated(PAUCARPATA_gps_rociado_NA$UNICODE), ]
    
    
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
    diff1<-setdiff(attack_UCHUMAYO$UNICODE,aqp_gps_block$UNICODE) #  124viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack$UNICODE) # 1216Viviendas 
    #Interseccion
    interseption<-intersect(attack_UCHUMAYO$UNICODE, aqp_gps_block$UNICODE)#1846
    #Merge
    UCHUMAYO_gps_rociado <- merge(aqp_gps_block,attack_UCHUMAYO, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_UCHUMAYO[attack_UCHUMAYO$UNICODE%in%diff1,]#243 observaciones 
    aux2 <- UCHUMAYO_gps_rociado[UCHUMAYO_gps_rociado$UNICODE%in%diff1,]
    aux3 <- UCHUMAYO_gps_rociado[UCHUMAYO_gps_rociado$UNICODE%in%diff2,]#1215
    
    UCHUMAYO_gps_rociado_NA<-UCHUMAYO_gps_rociado[is.na(UCHUMAYO_gps_rociado$LATITUDE)]
    DUPLICADOS<-UCHUMAYO_gps_rociado_NA[duplicated(UCHUMAYO_gps_rociado_NA$UNICODE)]
    UCHUMAYO_gps_rociado_NA<-UCHUMAYO_gps_rociado_NA[!duplicated(UCHUMAYO_gps_rociado_NA$UNICODE), ]
    
    write.csv(aux1,"~/CLAUDIA-DATOS-/claudia codigos r/dif_roci_aqp/uchumayo_dif_sir_nogoo.csv",row.names = FALSE)
    
    #------------------------------------------------------------------------------------      
    #uniendo bases 
    diff1<-setdiff(attack_MIRAFLORES$UNICODE,aqp_gps_block$UNICODE) #  9viviendas
    diff2<-setdiff(aqp_gps_block$UNICODE,attack_MIRAFLORES$UNICODE) # 4238Viviendas 
    #Interseccion
    interseption<-intersect(attack_MIRAFLORES$UNICODE, aqp_gps_block$UNICODE)#8270
    #Merge
    MIRAFLORES_gps_rociado <- merge(aqp_gps_block,attack_MIRAFLORES, all= TRUE, by = "UNICODE")
    #Comprobando
    aux1 <- attack_MIRAFLORES[attack_MIRAFLORES$UNICODE%in%diff1,]#243 observaciones 
    aux2 <- MIRAFLORES_gps_rociado[MIRAFLORES_gps_rociado$UNICODE%in%diff1,]
    aux3 <- MIRAFLORES_gps_rociado[MIRAFLORES_gps_rociado$UNICODE%in%diff2,]#4234
    MIRAFLORES_gps_rociado_NA<-MIRAFLORES_gps_rociado[is.na(MIRAFLORES_gps_rociado$LATITUDE)]
    DUPLICADOS<-MIRAFLORES_gps_rociado_NA[duplicated(MIRAFLORES_gps_rociado_NA$UNICODE)]
    MIRAFLORES_gps_rociado_NA<-MIRAFLORES_gps_rociado_NA[!duplicated(MIRAFLORES_gps_rociado_NA$UNICODE), ]
#--------------------------------------------------------------------  
#Imprimiendo Resultados ROCIADOS DEL 2009 EN ADELANTE 
#--------------------------------------------------------------------
  
  #Resultado de las viviendas de Mariano Melgar que tienen numero de cuadra
  write.csv(mmelgar_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/mmelgar_gps_rociado_JUN_2017.csv", row.names = FALSE)
  #Resultado de las viviendas de Mariano Melgar que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_MARIANOMELGAR_JUN_2017.csv", row.names = FALSE)
 
  #Resultado de las viviendas de CAYMA  que tienen numero de cuadra
  write.csv(CAYMA_gps_rociado,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/CAYMA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de CAYMA  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_CAYMA.csv", row.names = FALSE)
  
  setwd('~/claudia codigos r')
  #Resultado de las viviendas de ASA que tienen numero de cuadra
  write.csv(ASA_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/ASA_gps_rociado_JUN.csv", row.names = FALSE)
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
  write.csv(PAUCARPATA_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/PAUCARPATA_gps_rociado_JUN.csv", row.names = FALSE)
  #Resultado de las viviendas de CHARACATO  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_PAUCARPATA.csv", row.names = FALSE)
  
  
  
  
  #Resultado de las viviendas de cwerro solo rociado que tienen numero de cuadra
  write.csv(ccerro_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/cerro_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de HUNTER que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_HUNTER.csv", row.names = FALSE)
  
  
  
  
  #Resultado de las viviendas de HUNTER que tienen numero de cuadra
  write.csv(HUNTER_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/hunter_gps_rociadosep2019.csv", row.names = FALSE)
  #Resultado de las viviendas de HUNTER que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_HUNTER.csv", row.names = FALSE)
  
  #Resultado de las viviendas de JLB Y RIVERO que tienen numero de cuadra
  write.csv(JLB_RIVERO_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/JLB_RIVERO_gps_rociado_JUN.csv", row.names = FALSE)
  #Resultado de las viviendas de HUNTER que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_JLB.csv", row.names = FALSE)
  
  #Resultado de las viviendas de LA JOYA  que tienen numero de cuadra
  write.csv(LA_JOYA_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/LA_JOYA_gps_rociado_JUN.csv", row.names = FALSE)
  #Resultado de las viviendas de HUNTER que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_LAJOYA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de CAYMA  que tienen numero de cuadra
  write.csv(CAYMA_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/CAYMA_gps_rociado_JUN.csv", row.names = FALSE)
  #Resultado de las viviendas de CAYMA  que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_CAYMA.csv", row.names = FALSE)
 
  #Resultado de las viviendas de ASA que tienen numero de cuadra
  write.csv(ASA_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/ASA_gps_rociado_JUN.csv", row.names = FALSE)
  #Resultado de las viviendas de ASA que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_ASA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de YARABAMBA  que tienen numero de cuadra
  write.csv(YARABAMBA_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/YARABAMBA_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de YARABAMBA  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_block_YARABAMBA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de CHARACATO  que tienen numero de cuadra
  write.csv(CHARACATO_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/CHARACATO_gps_rociado.csv_JUN.csv", row.names = FALSE)
  #Resultado de las viviendas de CHARACATO  que NO tienen numero de cuadra
  write.csv(no_block,"~/claudia codigos r/no_block_CHARACATO.csv", row.names = FALSE)
  
  #Resultado de las viviendas de SACHACA  que tienen numero de cuadra
  write.csv(SACHACA_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/Sachaca_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de SACHACA  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_SACHACA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de TIABAYA  que tienen numero de cuadra
  write.csv(TIABAYA_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/Tiabaya_gps_rociado.csv", row.names = FALSE)
  #Resultado de las viviendas de TIABAYA  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_TIABAYA.csv", row.names = FALSE)
  
  #Resultado de las viviendas de TIABAYA  que tienen numero de cuadra
  write.csv(SOCABAYA_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/SOCABAYA_gps_rociado_JUN.csv", row.names = FALSE)
  #Resultado de las viviendas de TIABAYA  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_SOCABAYA1.csv", row.names = FALSE)
  
 #Resultado de las viviendas de UCHUMAYO   que tienen numero de cuadra
  write.csv(MIRAFLORES_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/MIRAFLORES_rociado_JUN.csv", row.names = FALSE)
  
  #Resultado de las viviendas de TIABAYA  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_UCHUMAYO.csv", row.names = FALSE) 
  
  #Resultado de las viviendas de PAUCARPATA   que tienen numero de cuadra
  write.csv(UCHUMAYO_gps_rociado,"~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/UCHUMAYO_gps_rociado_JUN.csv", row.names = FALSE)
  #Resultado de las viviendas de TIABAYA  que NO tienen numero de cuadra
  write.csv(no_block,"~/CLAUDIA-DATOS-/claudia codigos r/no_blocks_arequipa/no_block_UCHUMAYO.csv", row.names = FALSE) 
  
  
  