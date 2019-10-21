##------------------------------------------------------------------
# obteniendo bases de datos que nos interesan
# bd del MINSA desde 2006 hasta el 2016
library(data.table)
MINSA_all <- fread("~/Downloads/generalRociadoPA.2018-04-04 12:59:47.csv")

# bd de codigos de localidades que hizo Cynthia en excel
locality <- fread("~/Downloads/CODIGOS_LOCALIDADES.csv")
setnames(locality,"Codigo","codloc")
# aumentando el campo con el que vamos a realizar la union
MINSA_all$codloc <- paste(MINSA_all$P,MINSA_all$D,MINSA_all$L,sep=".")

# separando I ciclo y II ciclo

MINSA_all$IN_TRI <- as.integer(gsub("\\+","",MINSA_all$IN_TRI))
MINSA_all$PE_TRI <- as.integer(gsub("\\+","",MINSA_all$PE_TRI))
MINSA_all$vivPos <- unlist(0)
MINSA_all$vivPos <- ifelse((MINSA_all$IN_TRI!=0 | MINSA_all$PE_TRI!=0),1,0)

# seleccionando campos que usaremos tanto del I ciclo como de II ciclo
MINSA_Ic<-subset(MINSA_all,P==1 & CICLO_ROCIADO==1,select = c(codloc,IN_TRI,PE_TRI,Residual_T:Residual_D,Residual_LV,Residual_LP,CICLO_ROCIADO,FECHA,vivPos))
MINSA_IIc<-subset(MINSA_all,P==1 & CICLO_ROCIADO==2,select = c(codloc,IN_TRI,PE_TRI,Residual_T:Residual_D,Residual_LV,Residual_LP,CICLO_ROCIADO,FECHA,vivPos))
# --- I ciclo cambiando NA x 0
MINSA_Ic$IN_TRI[is.na(MINSA_Ic$IN_TRI)] <- 0
MINSA_Ic$PE_TRI[is.na(MINSA_Ic$PE_TRI)] <- 0
MINSA_Ic$vivPos[is.na(MINSA_Ic$vivPos)] <- 0
# --- II ciclo cambiando NA x 0
MINSA_IIc$IN_TRI[is.na(MINSA_IIc$IN_TRI)] <- 0
MINSA_IIc$PE_TRI[is.na(MINSA_IIc$PE_TRI)] <- 0
MINSA_IIc$vivPos[is.na(MINSA_IIc$vivPos)] <- 0

# asociando los campos necesarios para realizar la sumatoria I ciclo
cons <- MINSA_Ic[,c("IN_TRI","PE_TRI","Residual_T","Residual_C","Residual_R","Residual_D","Residual_LV","Residual_LP","vivPos")]
consIc<- aggregate(cons, by=list(codloc=MINSA_Ic$codloc), sum)
# asociando los campos necesarios para realizar la sumatoria II ciclo
consII <- MINSA_IIc[,c("IN_TRI","PE_TRI","Residual_T","Residual_C","Residual_R","Residual_D","Residual_LV","Residual_LP","vivPos")]
consIIc<- aggregate(consII, by=list(codloc=MINSA_IIc$codloc), sum)

# primera union entre las localidades y el consolidado del I ciclo
preFinal <- merge(locality,consIc, by="codloc",all.x = T)

# segunda union entre las localidades, consolidado I ciclo y consolidado II ciclo
Final <- merge(preFinal,consIIc, by="codloc",all.x = T)
names(Final) <- gsub("\\.x", "_Ic", names(Final))
names(Final) <- gsub("\\.y", "_IIc", names(Final))

# para sacar la cantidad de casas que pertenecen a la localidad
# usamos la base de puntos de arequipa_gps 
aqpgps <- fread("~/PETM-shiny/unicode_numberMzn/AREQUIPA_GPS_GOOGLE/AQP_GPS_GOOGLE_EARTH_PUNTOS_30_abril_2019.csv")
aqpgps$codloc <- paste(aqpgps$P,aqpgps$D,aqpgps$L, sep = ".")
aqpgps <- subset(aqpgps, P==1, select = c(UNICODE,codloc))
total_viv <- as.data.frame(table(aqpgps$codloc))
names(total_viv) <- gsub("Var1","codloc",names(total_viv))


# UNION final de viviendas totales por localidad y el consolidado final de ambos rociados
FinalGPS <- merge(Final,total_viv,by="codloc",all.x = T) 
FinalGPS <- FinalGPS[order(FinalGPS$D,FinalGPS$L),]

setwd("~/CLAUDIA-DATOS-/claudia codigos r/bd_minsa/")

write.csv(consIc,"consolidado_rociadosIciclo.csv", row.names=F)
write.csv(consIIc,"consolidado_rociadoIIciclo.csv", row.names=F)
write.csv(FinalGPS,"consolidado_final.csv", row.names = F)



