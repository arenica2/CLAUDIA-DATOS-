setwd ("~/CLAUDIA-DATOS-/claudia codigos r") 
#limpiando las bases de datos de los consolidados de rociado (attack data 2009 2015,
# y union rociados 2006-2009) .

library(dplyr)
library(data.table)

#Leyendo el consolidado del rociado UNION ROCIADOS QUE TIENE LOS ROCIADOS DESDE 2006 HASTA
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
varstokeep <- c("UNICODE","P","D","L","V", "P_TRIAT", "I_TRIAT", "FECHA", "CICLO","TRATADA","RENUENTE","CERRADA","DESHABITADA","Residual.Rec")
attack_2006_2009 <- attack_2006_2009[,varstokeep]
attack_2006_2009<-as.data.table(attack_2006_2009)
#Convirtiendo de factor a character o numero a caracter
attack_2006_2009$TRATADA <- as.character(attack_2006_2009$TRATADA)
attack_2006_2009$Residual.Rec<-as.character(attack_2006_2009$Residual.Rec)
#cambiando valores de NULL A 0 
attack_2006_2009[P_TRIAT == "NULL", P_TRIAT := "0"]
attack_2006_2009[I_TRIAT == "NULL", I_TRIAT := "0"]
attack_2006_2009[TRATADA == "NULL", TRATADA := "0"]
attack_2006_2009[Residual.Rec == "NULL", Residual.Rec := "0"]
attack_2006_2009[RENUENTE == "NULL", RENUENTE := "0"]
attack_2006_2009[DESHABITADA == "NULL", DESHABITADA := "0"]
attack_2006_2009[CERRADA == "NULL", CERRADA := "0"]
#PONIENDO TODAS LAS CASAS QUE FUERON RECUPARADAS COMO TRATADAS 
attack_2006_2009[Residual.Rec == "1", TRATADA := "1"]


#ELIMINADO LA ULTIMA FILA QUE NO NOS SIRVE 
attack_2006_2009<-attack_2006_2009[,names(attack_2006_2009)[c(-14)],with=FALSE]
#ESCOGIENDO SOLO  AREQUIPA 
attack_2006_2009<-attack_2006_2009[P==1]
#ESCOGIENDO LOS D=7,8,9,13,18,23,24,25.
attack_2006_2009<-attack_2006_2009[D==7|D==8|D==9|D==13|D==18|D==23|D==24|D==25]
#escribiendo la tabla limpia   
write.csv(attack_2006_2009,"~/CLAUDIA-DATOS-/claudia codigos r/ATTACK_2006_2015/attack_2006_2009.csv",row.names =FALSE )


#tabla attack 2009 -2015
attack_data_2009_2015<-read.csv("~/claudia codigos r/cons_rociado_2009_2015.csv")

#drop unwanted columns
varstokeep <- c("UNICODE","P","D","L","V", "P_TRIAT", "I_TRIAT", "FECHA", "CICLO")
attack_data_2009_2015 <- attack_data_2009_2015[, varstokeep]
#Seleccionando 
attack_data_2009_2015<-as.data.table(attack_data_2009_2015)
#convirtiendo los esapcios vacios a 0
attack_data_2009_2015<-set_to(attack_data_2009_2015)

attack_data_2009_2015$P_TRIAT<-as.character(attack_data_2009_2015$P_TRIAT)
attack_data_2009_2015$I_TRIAT<-as.character(attack_data_2009_2015$I_TRIAT)
attack_data_2009_2015[I_TRIAT == "", I_TRIAT := "0"]
attack_data_2009_2015<-set_to(attack_data_2009_2015)





#seleccionando solo arequipa 
attack_data_2009_2015<-attack_data_2009_2015[P==1]
#Seleccionando solo D= 1,3,4,5,10,11,12,13,28 que estan dentro de nuestro estudio
attack_data_2009_2015<-attack_data_2009_2015[D==1|D==3|D==4|D==5|D==10|D==11|D==12|D==13|D==28]

write.csv(attack_data_2009_2015,"~/CLAUDIA-DATOS-/claudia codigos r/ATTACK_2006_2015/attack_2009_2015.csv",row.names =FALSE )





