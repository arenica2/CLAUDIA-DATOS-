setwd ("~/claudia codigos r") 
#limpiando las bases de datos de los consolidados de rociado (attack data 2009 2015,
# y union rociados 2006-2009)de localidades que no nos interesan() .

attack_data_2009_2015<-read.csv("~/claudia codigos r/cons_rociado_2009_2015.csv") 

library(dplyr)
library(data.table)

#drop unwanted columns
varstokeep <- c("UNICODE","P","D","L","V", "P_TRIAT", "I_TRIAT", "FECHA", "CICLO")
attack_data_2009_2015 <- attack_data_2009_2015[, varstokeep]
#Seleccionando solo P=1
attack_data_2009_2015<-as.data.table(attack_data_2009_2015)
attack_data_2009_2015<-attack_data_2009_2015[P==1]
#Seleccionando solo D= 1,3,4,5,10,11,12,13,28 que estan dentro de nuestro estudio
attack_data_2009_2015<-attack_data_2009_2015[D==1|D==3|D==4|D==5|D==10|D==11|D==12|D==13|D==28]

#all gps data (most have blocks)
gps_aqp <- read.csv("~/claudia codigos r/AREQUIPA_GPS_GOOGLE.csv")
gps_aqp_adicionadas <- read.csv("C:/Users/Rodrigo/Documents/claudia codigos r/added_aqpI.csv")# 
gps_aqp_total <- rbind(gps_aqp, gps_aqp_adicionadas)

#merge gps and spray data
attack_gps_2009_2015 <- merge(attack_data_2009_2015, gps_aqp_total, by ="UNICODE",all.x =TRUE)
attack_gps_2009_2015<-as.data.table(attack_gps_2009_2015)

#ESCOGIENDO LA VARIABLES QUE NOS INTERESAN

setnames(attack_gps_2009_2015,"P.x","P")
setnames(attack_gps_2009_2015,"D.x","D")
setnames(attack_gps_2009_2015,"L.x","L")
setnames(attack_gps_2009_2015,"V.x","V")
attack_gps_2009_2015<-as.data.table(attack_data_2009_2015)
attack_gps_2009_2015<-attack_gps_2009_2015[,c("UNICODE","P","D","L","V","P_TRIAT","I_TRIAT","CICLO","FECHA","LATITUDE","LONGITUDE")]

#encontrando casas in gps 
mising<-attack_gps_2009_2015[is.na(LATITUDE)]
DUPLICADOS <-duplicated(attack_gps_2009_2015)
UNICOS<-unique(attack_gps_2009_2015)

##imprimendo las casas in gps
write.csv(mising,"nogps.csv",row.names =FALSE )
write.csv(attack_gps_2009_2015,"attack_gps_2009_2015.csv",row.names =FALSE )



#leyendo el consolidado del rociado UNION ROCIADOS QUE TIENE LOS ROCIADOS DESDE 2006 HASTA
#HASTA UNA PARTE DEL 2009 .
attack_2006_2009<-read.csv("~/claudia codigos r/UNION_ROCIADOS_2012-01-31.csv")
attack_2006_2009$UNICODE<-paste(attack_2006_2009$P, attack_2006_2009$D, attack_2006_2009$L, attack_2006_2009$V, sep = ".")

#CAMBIANDO DE NOMBRE A LAS VARIABLES PARA QUE COINCIDAN CON LOS NOMBRES DE LA TABLA ANTERIOR  
setnames(attack_2006_2009,"P.cap.T","P_TRIAT")
setnames(attack_2006_2009,"I.cap.T","I_TRIAT")
setnames(attack_2006_2009,"Residual.T","TRATADA")
setnames(attack_2006_2009,"Residual.R","RENUENTE")
setnames(attack_2006_2009,"Residual.C","CERRADA")
setnames(attack_2006_2009,"Residual.D","DESHABITADA")






#drop unwanted columns
varstokeep <- c("UNICODE","P","D","L","V", "P_TRIAT", "I_TRIAT", "FECHA", "CICLO")
attack_2006_2009 <- attack_2006_2009[, varstokeep]

#conviertiendo los null in P_TRIAT Y I_TIRAT DE NULL A NA 
attack_2006_2009<-as.data.table(attack_2006_2009)
attack_2006_2009[P_TRIAT == "NULL", P_TRIAT := NA]
attack_2006_2009[I_TRIAT == "NULL", I_TRIAT := NA]
#ESCOGIENDO SOLO  AREQUIPA 
attack_2006_2009<-attack_2006_2009[P==1]
#ESCOGIENDO LOS D=7,8,9,13,18,23,24,25.
attack_2006_2009<-attack_2006_2009[D==7|D==8|D==9|D==13|D==18|D==23|D==24|D==25]

#leyendo la data de gps  de las adicionales para 
added_2006_2009<-read.csv("~/claudia codigos r/added_aqpII.csv")
#JOIN BASE ATTACK 2006-2009 WITH GPS  
gps_aqp_total<-rbind(gps_aqp_total,added_2006_2009)

attack_gps_2006_2009 <- merge(attack_2006_2009, gps_aqp_total, by="UNICODE",all.x =TRUE)
setnames(attack_gps_2006_2009,"P.x","P")
setnames(attack_gps_2006_2009,"D.x","D")
setnames(attack_gps_2006_2009,"L.x","L")
setnames(attack_gps_2006_2009,"V.x","V")
attack_gps_2009_2015<-as.data.table(attack_gps_2006_2009)
attack_gps_2006_2009<-attack_gps_2006_2009[,c("UNICODE","P","D","L","V","P_TRIAT","I_TRIAT","CICLO","FECHA","LATITUDE","LONGITUDE")]

#encontrando casas in gps 
mising1<-attack_gps_2006_2009[is.na(LATITUDE)]

##imprimendo las casas sin gps
write.csv(mising1,"noblock2.csv",row.names =FALSE)


write.csv(attack_gps_2006_2009,"attack_gps_2006_2009.csv",row.names =FALSE )


