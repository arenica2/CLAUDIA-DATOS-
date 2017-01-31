#seteando las rutas 
ruta_1<-"/Users/Rodrigo/Documents/GitHub/Participation/bases_commcare/resultados"
ruta_2<-"/Users/Rodrigo/Links/Descargas.lnk"


########LEYENDO LOS ARCHIVOS QUE VAMOS A USAR PARA GHACER EL MERGE CON ENCUESTAS MINSA ##############
bases_comcare<-read.csv("C:/Users/Rodrigo/Documents/GitHub/Participation/bases_commcare/resultados/COMMCARE_BICHOS_27oct2015.csv",stringsAsFactors = FALSE) 
encuestas_minsa<- read.csv("C:/Users/Rodrigo/Downloads/asa_eed.csv - asa_eed.csv.csv") 

#Filtrando de las encuestas MINSA LAS LOCALIDADES DE PARTIIPACION   
filtro<-function(dat,dentro)
{
  filtrado<-dat[is.element(dat$L,dentro)==TRUE,]
  return(filtrado)
}
dentro<-c(7,8,13,14,22,23,25,26,27,29,34,36,39,42,44,45,71,77,78,81,82,83)
#data_1
encuestas_minsa<-filtro(encuestas_minsa,dentro)
# ELIGIENDO LAS VARIABLES QUE NOS INTERESAN 
varstokeep <- c("unicode","LOCALIDAD","D_FE","M_FE","A_FE","NUM_AMB","MAT_PARED","MAT_TECHO","MAT_PISO","A_TRI_I","N_TRI_I","T_TRI_I","OBS_INTRA","PER_TECHO","CUY_T","CON_T","OVE_T","PER_T","AVE_T","GAT_T","OTRO_ANI_T","CARAC_T","PER_PATIO","CUY_P","CON_P","OV_P","PER_P","AVE_P","GAT_P","OTRO_ANI_P","CARAC_P","A_TRI_P","N_TRI_P","T_TRI_P","OBS_PERI","EXUVIAS","HUEVOS","M_FEC","M_FEC_SEC","ENC","CER","REN","DES","LVA","LPU")
encuestas_minsa<- encuestas_minsa[,varstokeep]


#MERGEANDO LAS BASES DE COMCARE DE BICHOS CON LA BASE DE ENCUESTAS MINSA PARA TENER BACKGROUND 
comcare_data_MINSA<-merge(bases_comcare,encuestas_minsa,by="unicode", all.x=TRUE)
comcare_data_MINSA<- comcare_data_MINSA[,-2]

setwd(ruta_1) 


write.csv(comcare_data_MINSA,"COMCARE_MINSA.csv",row.names = FALSE)



