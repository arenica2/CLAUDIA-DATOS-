
##BD MINSA

#-RUTAS UTILIZADAS
ruta_1 <- '/home/javier/Documentos/github/Participation/bd_minsa/minsa_ccolorado/'

#-------------------------------------------------------------------
#============ Funcion CreateUnicode =========================================
# Solo si la data tiene la columna "CODCNSLDD"
#--------------------------------------------------------------------
CreateUnicode<-function(data) {
  #Esta funcion retorna un data frame con los con el UNICODE
  #
  #ARGS
  # data = La base donde se desea obtener el UNICODE
  #
  #RETURNS
  # data = Con las columnas UNICODE, P, D, L, V al inicio del dataframe
  #
  
  #Separando en "P", "D", "L" y "V"
  data$P<-substr(data$CODCNSLDD,1,1)
  data$D<-substr(data$CODCNSLDD,2,3)
  data$L<-substr(data$CODCNSLDD,4,5)
  data$V<-data$CODIGO
  
  #Eliminado espacios en blanco
  data$D <- gsub(" ","",data$D, fixed = T)
  data$L <- gsub(" ","",data$L, fixed = T)
  data$V <- gsub(" ","",data$V, fixed = T) 
  
  #Contruyendo UNICODE
  data$UNICODE<-paste(data$P, data$D, data$L, data$V, sep = ".")
  
  #Ordenando Las columnas
  n_col <- length(data)
  data <- data[, c(n_col, n_col-4,n_col-3,n_col-2,n_col-1, 1:(n_col-5) )]
  
  return(data)
}
#=========================================================================

#----------------------------------------------------------------------------
#============ Funcion ELIMINAR DUPLICADOS UNICODE X STATUS ==================
# Requerimientos:
#   - Las bases de datos deben contener las columna "UNICODE", "STATUS", "POS_CHIRI"
#--------------------------------------------------------------------
ElimDupliUxS<-function(data) {
  #Esta funcion retorna un data frame con los datos de unicode UNICOS poniendo en prioridad el STATUS de las viviendas 
  #("T", "R", "C", "DES", "LP", "LV"), para con esto seleccionar el duplicado que debe quedar en el data.frame de respuesta
  #
  #ARGS
  # data = Data frame conteniendo los datos
  #
  #RETURNS
  # data_sindupli = Data frame sin duplicados
  #
  
  #Obtenemos en un array todos los UNICODEs que son duplicados
  uni_dupli <- unique(data$UNICODE[which(duplicated(data$UNICODE))])
  
  #Almacenamos en una variable los datos que NO contienen UNICODEs duplicados
  data_sindupli <- data[!(data$UNICODE %in% uni_dupli),]
  
  #Almacenamos en una variable todos los registros que SI son duplicados
  data_condupli <- data[data$UNICODE %in% uni_dupli,]
  
  #Trabajamos con los duplicados
  #Dando prioridad a los UNICODEs que son tratadas(STATUS=1) y positivas (POS_CHIRI=1)
  variable <- data_condupli[1 == data_condupli$STATUS & 1 == data_condupli$POS_CHIRI,]
  #Juntamos solo las viviendas sin duplicados, dependiendo de la prioridad del STATUS (T, R, C, DES, LP, LV)
  i <- 1
  while (length(uni_dupli)) {
    #Almacenamos los datos sin duplicados
    data_sindupli <- rbind(data_sindupli,variable)
    #Actualizando variables
    uni_dupli <- setdiff(uni_dupli, variable$UNICODE)
    x <- data[data$UNICODE %in% uni_dupli,]
    data_condupli <- data_condupli[data_condupli$UNICODE %in% uni_dupli,]
    variable <- data_condupli[i == data_condupli$STATUS,]
    i <- i+1
  }
  
  return(data_sindupli)
}
#=========================================================================

# install.packages (“foreign”) #Instalar el paquete que nos permite abrir archivos ".dbf"
library(foreign) #Llamando al paquete

#Ruta
setwd(ruta_1)

#Obtenemos las bases que nos interesan
t_dist<- read.dbf(path.expand("control/t_dist.dbf"), as.is = TRUE)#contiene los códigos y nombre de distritos, formulacion de insecticida
t_loc<- read.dbf(path.expand("control/t_loc.dbf"), as.is = TRUE)#contiene nombre de localidades, codigo distrito, y nro de viv total de las loc
t_roc<- read.dbf(path.expand("control/t_roc.dbf"), as.is = TRUE)#contiene códigos y nombres de los rociadores
t_mat<- read.dbf(path.expand("control/t_mat.dbf"), as.is = TRUE)#contiene el código y el nombre de los materiales de las viviendas
td_cnsldd<- read.dbf(path.expand("control/td_cnsldd.dbf"), as.is = TRUE)#tabla de consolidación donde se encuentra la mayoría de la información
th_cnsldd<- read.dbf(path.expand("control/th_cnsldd.dbf"), as.is = TRUE)#contiene las fechas de rociado de las viviendas, ciclo de rociado 

#Eliminando inconsistencias en bases foxpro
t_dist<- t_dist[!grepl("00015", t_dist$CODDIST, fixed = TRUE),]#Eliminamos duplicadado "Cerro Colorado"
t_loc<- t_loc[!grepl("00194", t_loc$CODLOC, fixed = TRUE),]#Eliminamos "Huanca-Murco"
t_loc<- t_loc[!grepl("00195", t_loc$CODLOC, fixed = TRUE),]#Eliminamos duplicadado "Canco"
t_loc<- t_loc[!grepl("00196", t_loc$CODLOC, fixed = TRUE),]#Eliminamos "Huambo-Canca"
t_roc<- t_roc[!(grepl('00081', t_roc$CODROC, fixed = TRUE) & grepl('Pepe Quintanilla', t_roc$ROCIADOR, fixed = TRUE)),]#Eliminamos codigo "00081" con "Pepe Quintanilla" ya que se repite con otro codigo igual
t_roc<- t_roc[!grepl("Andy", t_roc$ROCIADOR, fixed = TRUE),]#Eliminamos "Andy Catacora" nunca rocea

#Reemplazando tildes
t_dist$INSECTICID<-enc2utf8(t_dist$INSECTICID)
t_dist$INSECTICID<- gsub("<ed>","i",t_dist$INSECTICID)#reemplazando la vocal "i"
t_loc$LOCALIDAD<- enc2utf8(t_loc$LOCALIDAD)
t_loc$LOCALIDAD<- gsub("<e1>","a",t_loc$LOCALIDAD)#reemplazando "á" por "a"
t_loc$LOCALIDAD<- gsub("<e9>","e",t_loc$LOCALIDAD)#reemplazando "é" por "e"
t_loc$LOCALIDAD<- gsub("<ed>","i",t_loc$LOCALIDAD)#reemplazando "í" por "i"
t_loc$LOCALIDAD<- gsub("<f3>","o",t_loc$LOCALIDAD)#reemplazando "ó" por "o"
t_loc$LOCALIDAD<- gsub("<fa>","u",t_loc$LOCALIDAD)#reemplazando "ú" por "u"
t_loc$LOCALIDAD<- gsub("<f1>","ni",t_loc$LOCALIDAD)#reemplazando "ñ" por "ni"
t_roc$ROCIADOR<- enc2utf8(t_roc$ROCIADOR)
t_roc$ROCIADOR<- gsub("<e1>","a",t_roc$ROCIADOR)#reemplazando "á" por "a"
t_roc$ROCIADOR<- gsub("<e9>","e",t_roc$ROCIADOR)#reemplazando "é" por "e"
t_roc$ROCIADOR<- gsub("<ed>","i",t_roc$ROCIADOR)#reemplazando "í" por "i"
t_roc$ROCIADOR<- gsub("<f3>","o",t_roc$ROCIADOR)#reemplazando "ó" por "o"
t_roc$ROCIADOR<- gsub("<fa>","u",t_roc$ROCIADOR)#reemplazando "ú" por "u"
t_roc$ROCIADOR<- gsub("<f1>","ni",t_roc$ROCIADOR)#reemplazando "ñ" por "ni"
t_mat$MATERIAL<- enc2utf8(t_mat$MATERIAL)
t_mat$MATERIAL<- gsub("<f3>","o",t_mat$MATERIAL)#reemplazando "ó" por "o"
td_cnsldd$JEFEVIV<- enc2utf8(td_cnsldd$JEFEVIV)
td_cnsldd$JEFEVIV<- gsub("<e1>","a",td_cnsldd$JEFEVIV)#reemplazando "á" por "a"
td_cnsldd$JEFEVIV<- gsub("<e9>","e",td_cnsldd$JEFEVIV)#reemplazando "é" por "e"
td_cnsldd$JEFEVIV<- gsub("<ed>","i",td_cnsldd$JEFEVIV)#reemplazando "í" por "i"
td_cnsldd$JEFEVIV<- gsub("<f3>","o",td_cnsldd$JEFEVIV)#reemplazando "ó" por "o"
td_cnsldd$JEFEVIV<- gsub("<fa>","u",td_cnsldd$JEFEVIV)#reemplazando "ú" por "u"
td_cnsldd$JEFEVIV<- gsub("<f1>","ni",td_cnsldd$JEFEVIV)#reemplazando "ñ" por "ni"
td_cnsldd$IN_MAT<- enc2utf8(td_cnsldd$IN_MAT)
td_cnsldd$IN_MAT<- gsub("<f3>","o",td_cnsldd$IN_MAT)#reemplazando "ó" por "o"
td_cnsldd$PE_MAT<- enc2utf8(td_cnsldd$PE_MAT)
td_cnsldd$PE_MAT<- gsub("<f3>","o",td_cnsldd$PE_MAT)#reemplazando "ó" por "o"

#Seleccionado solo columnas con datos
distrito<- t_dist[,c("CODDIST","DISTRITO","CODIGO","CODPRV","CODNSTCD","INSECTICID")]
localidad<- t_loc[,c("CODLOC","LOCALIDAD","CODIGO","NUMVIV","CODDIST","CODINT")]
materiales<-t_mat[,c("CODMAT","MATERIAL")]
rociador<-t_roc[,c("CODROC","ROCIADOR","BOSSONOFF")]
fecha_roc_minsa<-th_cnsldd[,c("NUMERO","CODCNSLDD","CODPRV","CODDIST","CODLOC","CODROC","CICLO","FECHA","CODNSTCD","SAVED","CODJEF","INICIO","FINAL")]
cons_roc_minsa<-td_cnsldd[,c("CODCNSLDD","CODIGO","SEEKVIV","CODVIV","JEFEVIV","NUMRES","IN_AMB","IN_MAT","IN_GRI","IN_TRI","PE_AMB","PE_MAT","PE_GRI","PE_TRI","AN_TEC","AN_PAT","TRARES_T","TRARES_RR","CARGANSTCD","SUPER","NOTAS","NUMREC","IN_CODMAT","PE_CODMAT","CICLO")]

#---------------------------------------------------------------------#
#-------                fecha_roc_minsa                      ---------#
#---------------------------------------------------------------------#
#Separando la fecha
fecha_roc_minsa$FR_A<-format( fecha_roc_minsa$FECHA, format="%Y" ) 
fecha_roc_minsa$FR_M<-format( fecha_roc_minsa$FECHA, format="%m" ) 
fecha_roc_minsa$FR_D<-format( fecha_roc_minsa$FECHA, format="%d" ) 
#Merge entre "fecha_roc_minsa" y "rociador"
fecha<- merge(fecha_roc_minsa,rociador)
fecha<-fecha[,c(3,8,14:16,1,17,12,13)]

#---------------------------------------------------------------------#
#--------                  cons_roc_minsa                    ---------#
#---------------------------------------------------------------------#
#Se selecciona solo los que tienen "0" los demas son sumatorias de este.
cons_roc_minsa<-cons_roc_minsa[0==cons_roc_minsa$NUMREC,]

#Separando en "P", "D", "L" y "V"
cons_roc_minsa$P<-substr(cons_roc_minsa$CODCNSLDD,1,1)
cons_roc_minsa$D<-substr(cons_roc_minsa$CODCNSLDD,2,3)
cons_roc_minsa$L<-substr(cons_roc_minsa$CODCNSLDD,4,5)
cons_roc_minsa$V<-cons_roc_minsa$CODIGO

#Eliminado espacios en blanco
cons_roc_minsa$D <- gsub(" ","",cons_roc_minsa$D, fixed = T)
cons_roc_minsa$L <- gsub(" ","",cons_roc_minsa$L, fixed = T)
cons_roc_minsa$V <- gsub(" ","",cons_roc_minsa$V, fixed = T)

#Contruyendo UNICODE
cons_roc_minsa$UNICODE<-paste(cons_roc_minsa$P, cons_roc_minsa$D, cons_roc_minsa$L, cons_roc_minsa$V, sep = ".")

#Completando ciclos de rociado I y II
cons_roc_minsa$CICLO_ROCIADO<-substr(cons_roc_minsa$CODCNSLDD,8,8)#comprobado que salen bien

#Creando columna "MAS20TRI_IN" y "MAS20TRI_PE"
cons_roc_minsa$MAS20TRI_IN<- unlist(0)
cons_roc_minsa$MAS20TRI_PE<- unlist(0)
cons_roc_minsa[grepl("+",cons_roc_minsa$IN_TRI, fixed = TRUE),"MAS20TRI_IN"]<- 1
cons_roc_minsa[grepl("+",cons_roc_minsa$PE_TRI, fixed = TRUE),"MAS20TRI_PE"]<- 1

#Creando columnas de materiales en el INTRA y asignando su valor
cons_roc_minsa$IN_SIL<- unlist(0)
cons_roc_minsa$IN_NOB<- unlist(0)
cons_roc_minsa$IN_LAD<- unlist(0)
cons_roc_minsa$IN_ADO<- unlist(0)
cons_roc_minsa$IN_BLOQ<- unlist(0)
cons_roc_minsa[grepl("Sillar",cons_roc_minsa$IN_MAT, fixed = TRUE),"IN_SIL"]<- 1
cons_roc_minsa[grepl("Noble",cons_roc_minsa$IN_MAT, fixed = TRUE),"IN_NOB"]<- 1
cons_roc_minsa[grepl("Ladrillo",cons_roc_minsa$IN_MAT, fixed = TRUE),"IN_LAD"]<- 1
cons_roc_minsa[grepl("Adobe",cons_roc_minsa$IN_MAT, fixed = TRUE),"IN_ADO"]<- 1
cons_roc_minsa[grepl("Bloqueta",cons_roc_minsa$IN_MAT, fixed = TRUE),"IN_BLOQ"]<- 1

#Creando columnas de materiales en el PERI y asignando su valor
cons_roc_minsa$PE_SIL<- unlist(0)
cons_roc_minsa$PE_NOB<- unlist(0)
cons_roc_minsa$PE_LAD<- unlist(0)
cons_roc_minsa$PE_ADO<- unlist(0)
cons_roc_minsa$PE_BLOQ<- unlist(0)
cons_roc_minsa$PE_PIE<- unlist(0)
cons_roc_minsa[grepl("Sillar",cons_roc_minsa$PE_MAT, fixed = TRUE),"PE_SIL"]<- 1
cons_roc_minsa[grepl("Noble",cons_roc_minsa$PE_MAT, fixed = TRUE),"PE_NOB"]<- 1
cons_roc_minsa[grepl("Ladrillo",cons_roc_minsa$PE_MAT, fixed = TRUE),"PE_LAD"]<- 1
cons_roc_minsa[grepl("Adobe",cons_roc_minsa$PE_MAT, fixed = TRUE),"PE_ADO"]<- 1
cons_roc_minsa[grepl("Bloqueta",cons_roc_minsa$PE_MAT, fixed = TRUE),"PE_BLOQ"]<- 1
cons_roc_minsa[grepl("Piedra",cons_roc_minsa$PE_MAT, fixed = TRUE),"PE_PIE"]<- 1

#Creando columnas de animales en el TECHO y asignando su valor
cons_roc_minsa$TEC_CUY<- unlist(0)
cons_roc_minsa$TEC_CON<- unlist(0)
cons_roc_minsa$TEC_OVE<- unlist(0)
cons_roc_minsa$TEC_PER<- unlist(0)
cons_roc_minsa$TEC_AVE<- unlist(0)
cons_roc_minsa$TEC_GAT<- unlist(0)
cons_roc_minsa[grepl("Cuyes",cons_roc_minsa$AN_TEC, fixed = TRUE),"TEC_CUY"]<- 1
cons_roc_minsa[grepl("Conejos",cons_roc_minsa$AN_TEC, fixed = TRUE),"TEC_CON"]<- 1
cons_roc_minsa[grepl("Ovejas",cons_roc_minsa$AN_TEC, fixed = TRUE),"TEC_OVE"]<- 1
cons_roc_minsa[grepl("Perros",cons_roc_minsa$AN_TEC, fixed = TRUE),"TEC_PER"]<- 1
cons_roc_minsa[grepl("Aves",cons_roc_minsa$AN_TEC, fixed = TRUE),"TEC_AVE"]<- 1
cons_roc_minsa[grepl("Gatos",cons_roc_minsa$AN_TEC, fixed = TRUE),"TEC_GAT"]<- 1

#Creando columnas de animales en el PATIO y asignando su valor
cons_roc_minsa$PAT_CUY<- unlist(0)
cons_roc_minsa$PAT_CON<- unlist(0)
cons_roc_minsa$PAT_OVE<- unlist(0)
cons_roc_minsa$PAT_PER<- unlist(0)
cons_roc_minsa$PAT_AVE<- unlist(0)
cons_roc_minsa$PAT_GAT<- unlist(0)
cons_roc_minsa[grepl("Cuyes",cons_roc_minsa$AN_PAT, fixed = TRUE),"PAT_CUY"]<- 1
cons_roc_minsa[grepl("Conejos",cons_roc_minsa$AN_PAT, fixed = TRUE),"PAT_CON"]<- 1
cons_roc_minsa[grepl("Ovejas",cons_roc_minsa$AN_PAT, fixed = TRUE),"PAT_OVE"]<- 1
cons_roc_minsa[grepl("Perros",cons_roc_minsa$AN_PAT, fixed = TRUE),"PAT_PER"]<- 1
cons_roc_minsa[grepl("Aves",cons_roc_minsa$AN_PAT, fixed = TRUE),"PAT_AVE"]<- 1
cons_roc_minsa[grepl("Gatos",cons_roc_minsa$AN_PAT, fixed = TRUE),"PAT_GAT"]<- 1

#CONSOLIDADO FINAL
CONS_ROCIADO_2009_2015<- merge(cons_roc_minsa, fecha)
CONS_ROCIADO_2009_2015<- CONS_ROCIADO_2009_2015[,c(30,26:29,57:60,62,5:8,34:38,9,10,32,11,12,39:44,13,14,33,15,45:50,16,51:56,31,19:21)]# Tener cuidado cuando las tablas cambien de tamanio ya que esta en numeros
#Obteniendo solo las filas que son unicas, eliminando duplicados de filas
CONS_ROCIADO_2009_2015<- unique(CONS_ROCIADO_2009_2015)# se eliminaron 1274 registros

#Filtrando para que no se esté buscando localidades que no estan en el estudio en ASA
filtro<-function(dat,dentro)
{
  filtrado<-dat[(1==dat$P & 4==dat$D & is.element(dat$L,dentro)==TRUE),]
  return(filtrado)
}
dentro<-c(14,15,16,17,20,21,24,37,40,41,43,45,47,51,52)
#Aplicando funcion filtro
tratadas_CC<-filtro(CONS_ROCIADO_2009_2015,dentro)

write.csv(tratadas_CC,"tratadas_CC.csv", row.names=F)

uni_tratadas_CC <- tratadas_CC$UNICODE

dif_unicol_tccminsa <- setdiff(uni_colector,uni_tratadas_CC)
dif_tccminsa_unicol <- setdiff(uni_tratadas_CC, uni_colector)

#--------------------------------hasta aqui las tratadas del MINSA I Ciclo
# cuandfo se tenga las cerradas, renuentes del MINSA (oficial) se continua


  #--------------------------------------------------------------------------------------
# Solo que tengan las columnas UNICODE, CICLO y STATUS
#--------------------------------------------------------------------------------------
#Cerradas 
tdc_cnsldd<- read.dbf(path.expand("control/tdc_cnsldd.dbf"), as.is = TRUE)
#Renuente
tdr_cnsldd<- read.dbf(path.expand("control/tdr_cnsldd.dbf"), as.is = TRUE)
#Deshabitada 
tdd_cnsldd<- read.dbf(path.expand("control/tdd_cnsldd.dbf"), as.is = TRUE)
#Local publico
tdp_cnsldd<- read.dbf(path.expand("control/tdp_cnsldd.dbf"), as.is = TRUE)
#Lote vacio
tdv_cnsldd<- read.dbf(path.expand("control/tdv_cnsldd.dbf"), as.is = TRUE)

#Utilizando la funcion CreateUnicode, para obtener los UNICODEs
tdc_cnsldd <- CreateUnicode(tdc_cnsldd)
tdr_cnsldd <- CreateUnicode(tdr_cnsldd)
tdd_cnsldd <- CreateUnicode(tdd_cnsldd)
tdp_cnsldd <- CreateUnicode(tdp_cnsldd)
tdv_cnsldd <- CreateUnicode(tdv_cnsldd)

# Obteniendo el ciclo de rociado real sacado de la columna CODCNSLDD
tdc_cnsldd$CICLO_ROCIADO<-substr(tdc_cnsldd$CODCNSLDD,8,8)
tdr_cnsldd$CICLO_ROCIADO<-substr(tdr_cnsldd$CODCNSLDD,8,8)
tdd_cnsldd$CICLO_ROCIADO<-substr(tdd_cnsldd$CODCNSLDD,8,8)
tdp_cnsldd$CICLO_ROCIADO<-substr(tdp_cnsldd$CODCNSLDD,8,8)
tdv_cnsldd$CICLO_ROCIADO<-substr(tdv_cnsldd$CODCNSLDD,8,8)

#Creando una columna STATUS
tdc_cnsldd$STATUS <- unlist("C")
tdr_cnsldd$STATUS <- unlist("R")
tdd_cnsldd$STATUS <- unlist("DES")
tdp_cnsldd$STATUS <- unlist("LP")
tdv_cnsldd$STATUS <- unlist("LV")

#Obteniendo solo columna que necesito
tdc_cnsldd <- tdc_cnsldd[, c(1:5,19,20)]
tdr_cnsldd <- tdr_cnsldd[, c(1:5,19,20)]
tdd_cnsldd <- tdd_cnsldd[, c(1:5,19,20)]
tdp_cnsldd <- tdp_cnsldd[, c(1:5,19,20)]
tdv_cnsldd <- tdv_cnsldd[, c(1:5,19,20)]

#Juntando 
C_R <- merge(tdc_cnsldd,tdr_cnsldd, all = TRUE)
C_R_D <- merge(C_R,tdd_cnsldd, all = TRUE)
C_R_D_LP <- merge(C_R_D,tdp_cnsldd, all = TRUE)
C_R_D_LP_LV <-merge(C_R_D_LP,tdv_cnsldd, all = TRUE)

#Obteniendo registros unicos de todos los registros de Arequipa
C_R_D_LP_LV <- unique(C_R_D_LP_LV)

#Aplicando funcion filtro para obtener solo los datos del distrito ASA
statusMinsa<-filtro(C_R_D_LP_LV,dentro)

#Juntando con las tratadas    
tratadas <- tratadas_ASA
tratadas$STATUS <- unlist("T")
tratadas <- tratadas[,c(1:5,21,32,48,52)]
tratadas <- unique(tratadas)

#Merge
statusMinsaASA <- merge(statusMinsa, tratadas, all = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#------------------       CONSOLIDADO GENERAL 2006 - 2015 MINSA        ------------------------ 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Leer datos
cons2006_2009 <- read.csv("UNION_ROCIADOS.csv")

#Eliminado los que tienen "0" en la columna "P"
cons2006_2009 <- cons2006_2009[!(cons2006_2009$P==0),]

#Eliminamos columna "ID"
cons2006_2009 <- subset(cons2006_2009, select = -ID)
#Cambiamos de nombre a la columna CICLO por CICLO_ROCIADO
colnames(cons2006_2009)[57] <- "CICLO_ROCIADO"
#Contruyendo UNICODE
cons2006_2009$UNICODE<-paste(cons2006_2009$P, cons2006_2009$D, cons2006_2009$L, cons2006_2009$V, sep = ".")
cons2006_2009 <- cons2006_2009[,c(62,1:61)] # Ordenando columnas
#Eliminar espacios en blanco de la columna UNICODE
cons2006_2009$UNICODE <- gsub(" ","",cons2006_2009$UNICODE, fixed = T)

#Convirtiendo todo los que tienen CICLO_ROCIADO = 0 a CICLO_ROCIADO = 1 (según javier)
cons2006_2009$CICLO_ROCIADO[0 == cons2006_2009$CICLO_ROCIADO] <- 1

#Creando la columana STATUS
cons2006_2009$STATUS[cons2006_2009$Residual.T==1 | cons2006_2009$Residual.Rec==1] <- 1 #"T"
cons2006_2009$STATUS[cons2006_2009$Residual.R==1 & is.na(cons2006_2009$STATUS)] <- 2 #"R"
cons2006_2009$STATUS[cons2006_2009$Residual.C==1 & is.na(cons2006_2009$STATUS)] <- 3 #"C"
cons2006_2009$STATUS[cons2006_2009$Residual.D==1 & is.na(cons2006_2009$STATUS)] <- 4 #"DES"
#NOTA: El codigo "1.25.26.1769" tiene dos estados: "C" y "Rec", haremos valer el "Rec"
#--------------------------------------------------------------------------------------

#Completando STATUS con LP  y LV que se encuentran en el campo OBSERVACIONES
cons2006_2009$STATUS[is.na(cons2006_2009$STATUS) & grepl("P.",cons2006_2009$OBSERVACIONES, fixed = TRUE)] <- 5 #"LP"
cons2006_2009$STATUS[is.na(cons2006_2009$STATUS) & (grepl("V.",cons2006_2009$OBSERVACIONES, fixed = TRUE) | grepl("LOTE VACIO",cons2006_2009$OBSERVACIONES))] <- 6 #"LV"
#Si STATUS = NA y contiene la palabra "CONSTRUCCIÓN" o "NO SE ROCIO" ponemos C
cons2006_2009$STATUS[is.na(cons2006_2009$STATUS) & (grepl("CONSTRUCCIÓN",cons2006_2009$OBSERVACIONES) | grepl("NO SE ROCIO",cons2006_2009$OBSERVACIONES))] <- 3 #"C"
#Si STATUS = NA y contiene la palabra "FUE ROCIADA"
cons2006_2009$STATUS[is.na(cons2006_2009$STATUS) & grepl("FUE ROCIADA",cons2006_2009$OBSERVACIONES)] <- 1 #"T"
#Viviendas que tienen dos UNICODEs
cons2006_2009$STATUS["1.7.25.193"== cons2006_2009$UNICODE & 1==cons2006_2009$CICLO] <- 1 #"T" # Vivienda 183 es T
cons2006_2009$STATUS["1.7.25.236"== cons2006_2009$UNICODE & 1==cons2006_2009$CICLO] <- 1 #"T" # Vivienda 194 es T
cons2006_2009$STATUS["1.7.41.199"== cons2006_2009$UNICODE & 1==cons2006_2009$CICLO] <- 1 #"T" # Vivienda 200 es T
cons2006_2009$STATUS["1.7.41.238"== cons2006_2009$UNICODE & 1==cons2006_2009$CICLO] <- 1 #"T" # Vivienda 237 es T
cons2006_2009$STATUS["1.7.41.292"== cons2006_2009$UNICODE & 1==cons2006_2009$CICLO] <- 1 #"T" # Vivienda 291 es T
cons2006_2009$STATUS["1.7.8A.357"== cons2006_2009$UNICODE & 1==cons2006_2009$CICLO] <- 1 #"T" # Vivienda 356 es T
cons2006_2009$STATUS["1.7.41.238"== cons2006_2009$UNICODE & 2==cons2006_2009$CICLO] <- 1 #"T" # Vivienda 237 es T
cons2006_2009$STATUS["1.7.48C.15"== cons2006_2009$UNICODE & 2==cons2006_2009$CICLO] <- 1 #"T" # Vivienda 12 es T
#NOTA: Quedan 6 UNICODEs con NA en la columna STATUS
#------------------------------------------------------------------------------

#Creando la columna POS_CHIRI
cons2006_2009$POS_CHIRI[1==cons2006_2009$I_TRIAT | 1==cons2006_2009$P_TRIAT] <- 1
cons2006_2009$POS_CHIRI[is.na(cons2006_2009$POS_CHIRI)] <- 0

#Seleccionando solo las columna UNICODE, CICLO, STATUS y POS_CHIRI
status_cons2006_2009 <- subset(cons2006_2009, select = c(UNICODE, CICLO_ROCIADO, STATUS, POS_CHIRI))
status_cons2006_2009 <- unique(status_cons2006_2009)

#Utilizando funcion ElimDupliUxS()
c1_cons2006_2009 <- ElimDupliUxS(status_cons2006_2009[1 == status_cons2006_2009$CICLO_ROCIADO,])
c2_cons2006_2009 <- ElimDupliUxS(status_cons2006_2009[2 == status_cons2006_2009$CICLO_ROCIADO,])

#Juntando los dos ciclos en uno solo y SIN NINGUN dato duplicado
status_cons2006_2009 <- rbind(c1_cons2006_2009, c2_cons2006_2009)

#----   
#Acomodando los datos del consolidado 2009-2015 MINSA
#Creando la columna STATUS
CONS_ROCIADO_2009_2015$STATUS <- "T"
#Creando columna POS_CHIRI
CONS_ROCIADO_2009_2015$POS_CHIRI[is.na(CONS_ROCIADO_2009_2015$IN_TRI) & is.na(CONS_ROCIADO_2009_2015$PE_TRI)] <- 0
CONS_ROCIADO_2009_2015$POS_CHIRI[is.na(CONS_ROCIADO_2009_2015$POS_CHIRI)] <- 1
C_R_D_LP_LV$POS_CHIRI <- 0
#Seleccionando solo las columnas "UNICODE", "CICLO_ROCIADO", "STATUS", "POS_CHIRI"
status_T <- subset(CONS_ROCIADO_2009_2015, select = c("UNICODE", "CICLO_ROCIADO", "STATUS", "POS_CHIRI"))
status_C_R_D_LP_LV <- subset(C_R_D_LP_LV, select = c("UNICODE", "CICLO_ROCIADO", "STATUS", "POS_CHIRI"))
#Juntando consolidado 2009 - 2015
status_cons2009_2015 <- rbind(status_T, status_C_R_D_LP_LV)
#Registros unicos
status_cons2009_2015 <- unique(status_cons2009_2015)
#Cambiando el STATUS de letras (T, R, C, etct) por numeros de orden de prioridad (T=1, R=2, C=3, etc)
status_cons2009_2015$STATUS["T"==status_cons2009_2015$STATUS] <- 1
status_cons2009_2015$STATUS["R"==status_cons2009_2015$STATUS] <- 2
status_cons2009_2015$STATUS["C"==status_cons2009_2015$STATUS] <- 3
status_cons2009_2015$STATUS["DES"==status_cons2009_2015$STATUS] <- 4
status_cons2009_2015$STATUS["LP"==status_cons2009_2015$STATUS] <- 5
status_cons2009_2015$STATUS["LV"==status_cons2009_2015$STATUS] <- 6
#Utilizando funcion ElimDupliUxS()
c1_cons2009_2015 <- ElimDupliUxS(status_cons2009_2015[1 == status_cons2009_2015$CICLO_ROCIADO,])
c2_cons2009_2015 <- ElimDupliUxS(status_cons2009_2015[2 == status_cons2009_2015$CICLO_ROCIADO,])
#Juntando los dos ciclos en uno solo y SIN NINGUN dato duplicado
status_cons2009_2015 <- rbind(c1_cons2009_2015, c2_cons2009_2015)

#-----      
#CONSOLIDADO COMPLETO DESDE EL 2006 HASTA 2015 DEL MINSA CON LAS COLUMNAS: UNICODE, CICLO_ROCIADO, STATUS, POS_CHIRI
#Juntando consolidado final 
STATUS_CONS_2006_2015 <- rbind(status_cons2006_2009, status_cons2009_2015) #181941
#Registros unicos
STATUS_CONS_2006_2015 <- unique(STATUS_CONS_2006_2015) # 181416, se eliminaron #525
#Utilizando funcion ElimDupliUxS()
c1_cons2006_2015 <- ElimDupliUxS(STATUS_CONS_2006_2015[1==STATUS_CONS_2006_2015$CICLO_ROCIADO,])
c2_cons2006_2015 <- ElimDupliUxS(STATUS_CONS_2006_2015[2==STATUS_CONS_2006_2015$CICLO_ROCIADO,])
#Juntando los dos ciclos en uno solo y SIN NINGUN dato duplicado
STATUS_CONS_2006_2015 <- rbind(c1_cons2006_2015, c2_cons2006_2015) #181245: se eliminaron 171 registros duplicados
#Cambiando los valores de la columna STATUS por T,R,C,DES,LP,LV para que no haya confusiones
STATUS_CONS_2006_2015$STATUS[1==STATUS_CONS_2006_2015$STATUS] <- "T"
STATUS_CONS_2006_2015$STATUS[2==STATUS_CONS_2006_2015$STATUS] <- "R"
STATUS_CONS_2006_2015$STATUS[3==STATUS_CONS_2006_2015$STATUS] <- "C"
STATUS_CONS_2006_2015$STATUS[4==STATUS_CONS_2006_2015$STATUS] <- "DES"
STATUS_CONS_2006_2015$STATUS[5==STATUS_CONS_2006_2015$STATUS] <- "LP"
STATUS_CONS_2006_2015$STATUS[6==STATUS_CONS_2006_2015$STATUS] <- "LV"
#Separando por ciclos para poder poner cambiar los nombres de los campos
c1_cons2006_2015 <- STATUS_CONS_2006_2015[1==STATUS_CONS_2006_2015$CICLO_ROCIADO,]
c2_cons2006_2015 <- STATUS_CONS_2006_2015[2==STATUS_CONS_2006_2015$CICLO_ROCIADO,]
#Cambiando los nombres de los campos dependiendo del ciclo al que pertenecen
colnames(c1_cons2006_2015) <- c("UNICODE", "C1" , "C1_STATUS", "C1_POS_CHIRI")
colnames(c2_cons2006_2015) <- c("UNICODE", "C2" , "C2_STATUS", "C2_POS_CHIRI")
#Cambiando el valor de la columna C2 por 1=Presencia o O=Ausencia
c2_cons2006_2015$C2 <- 1
#Juntando los dos ciclos en un solo registro
STATUS_CONS_2006_2015 <- merge(c1_cons2006_2015, c2_cons2006_2015, by = "UNICODE", all = TRUE)
#Ordenando por UNICODE
STATUS_CONS_2006_2015 <- STATUS_CONS_2006_2015[order(STATUS_CONS_2006_2015$UNICODE),]

#----------------------------------------------------------------------------------------
# Imprimiendo datos
#----------------------------------------------------------------------------------------
setwd("/home/gianfranco/Documentos/github/Participation/bd_minsa/resultados")
write.csv(STATUS_CONS_2006_2015,'STATUS_CONS_2006_2015.csv')
write.csv(CONS_ROCIADO_2009_2015,'CONS_ROCIADO_2009_2015.csv')
write.csv(statusMinsaASA,'statusMinsaASA.csv')
write.csv(tratadas_ASA,'tratadas_ASA.csv')


