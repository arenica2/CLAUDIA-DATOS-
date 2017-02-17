 formato2<-read.csv("C:/Users/Rodrigo/Downloads/formato2_lectura_triatominos - formato2_LecturaDeTriatominos.csv")
 library(data.table)
 formato2$unicode<-as.character(formato2$unicode)
 formato2$fec_ingreso<-as.character(formato2$fec_ingreso)
 formato2<-as.data.table(formato2)
 formato2<-formato2[vivienda==915 |vivienda==893|vivienda==907|vivienda==910|vivienda==914|vivienda==911|vivienda==912|vivienda==898|vivienda==896|vivienda==897]
 formato2<-formato2[,c("unicode","fec_captura","cap_total","exa_total","pos_total")]                   
 setwd("~/CLAUDIA-DATOS-/claudia codigos r/")
 write.csv(formato2,"chiris_mariano.csv",row.names = FALSE)
 