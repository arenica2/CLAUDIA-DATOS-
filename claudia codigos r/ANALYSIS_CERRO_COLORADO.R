nos ## analisis cerro colorado primer y segundo rociado 
## 

library(data.table)
cerro<- read.csv("~/Participation/merge_participacion/Cerro Colorado 2016/merge_cc_II/resultados/CICLO_I_CICLO_II_CERRO_COLORADO.csv",stringsAsFactors = TRUE)
merge_cerro<-cerro[,c("UNICODE","T_Cuantis","ESTRATEGIA","CLUSTER","LATITUDE","LONGITUDE","T_Cuantis_IICC")]
merge_cerro<-as.data.table(merge_cerro)
merge_cerro[is.na(merge_cerro)]<-0
setnames(merge_cerro,"ESTRATEGIA","TRATAMIENTO")

merge_cerro[,FIRST_SPRAY:= ifelse(T_Cuantis==1,1,0)]
merge_cerro[,SECOND_SPRAY:=ifelse(T_Cuantis_IICC==1,1,0)]
merge_cerro[,BOTH_SPRAY:=ifelse(T_Cuantis==1 & T_Cuantis_IICC==1,1,0)]
merge_cerro[,NONE:=ifelse(T_Cuantis==0 & T_Cuantis_IICC==0,1,0)]





