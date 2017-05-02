 minsa<-read.csv("~/Participation/bd_minsa/bd/bases_minsa_feb_2017/generalRociadoPA_Javier.csv.")
 library(data.table)
 minsa<-as.data.table(minsa)
 minsa<-minsa[P==1]
 #ESCOGIENDO LOS D=4
 minsa<-minsa[D==4]
 #ESCOGIENDO LAS LOCALIDADES DE INTERES :(14,15,16,17,20,21,24,37,40,41,43,45,47,51,52)
 minsa<-minsa[L==14|L==15|L==16|L==17|L==20|L==21|L==24|L==37|L==40|L==41|L==43|L==45|L==47|L==51|L==52]
 #escogiendo CICLO II
 minsa<-minsa[CICLO_ROCIADO==1]
 ROCIADO_MINSA_I_CC<-minsa
 write.csv(ROCIADO_MINSA_I_CC,"~/CLAUDIA-DATOS-/ROCIADO_MINSA_I_CC.csv",row.names = FALSE)
 minsa<-read.csv("~/Participation/bd_minsa/bd/bases_minsa_feb_2017/generalRociadoPA_Javier.csv.")
 library(data.table)
 minsa<-as.data.table(minsa)
 minsa<-minsa[P==1]
 #ESCOGIENDO LOS D=4
 minsa<-minsa[D==4]
 #ESCOGIENDO LAS LOCALIDADES DE INTERES :(14,15,16,17,20,21,24,37,40,41,43,45,47,51,52)
 minsa<-minsa[L==14|L==15|L==16|L==17|L==20|L==21|L==24|L==37|L==40|L==41|L==43|L==45|L==47|L==51|L==52]
 #escogiendo CICLO II
 minsa<-minsa[CICLO_ROCIADO==2]
 ROCIADO_MINSA_II_CC<-minsa
 write.csv(ROCIADO_MINSA_II_CC,"~/CLAUDIA-DATOS-/ROCIADO_MINSA_II_CC.csv",row.names = FALSE)
 