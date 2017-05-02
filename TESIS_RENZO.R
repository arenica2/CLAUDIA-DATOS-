library(data.table)
formato_2<-read.csv("/Users/Rodrigo/Downloads/formato2_lectura_triatominos - formato2_LecturaDeTriatominos.csv")
formato_2<-as.data.table(formato_2)
formato_2<-formato_2[distrito == 1 & procedencia== "EP"]
setnames(formato_2,"unicode","UNICODE")
cuartos_1<-read.csv("/Users/Rodrigo/Downloads/CUARTOS_APURIMAC_encuestas2013.csv",sep = ";")
cuartos_2<-read.csv("/Users/Rodrigo/Downloads/CUARTOS_INDEPENDENCIA_encuestas2013.csv",sep = ";")
corrales_1<-read.csv("/Users/Rodrigo/Downloads/CORRALES_INDEPENDENCIA_encuestas2013.csv",sep = ";")
corrales<-read.csv("/Users/Rodrigo/Downloads/CORRALES_APURIMAC_encuesta2013.csv",sep = ";")

#juntando las bases de cuartos y corrales (apurimac e independencia)
cuartos<-rbind(cuartos_2,cuartos_1)

corrales<-rbind(corrales_1,corrales)


#separando el unicode en P,D,L,V ,para poder hacer el subset 
cuartos<-as.data.table(cuartos)
cuartos<-cuartos[,c("P","D","L","V") := tstrsplit(UNICODE,".", fixed=TRUE)]
corrales<-as.data.table(corrales)
corrales<-corrales[,c("P","D","L","V") := tstrsplit(UNICODE,".", fixed=TRUE)]

#scribiendo las bases 
write.csv(cuartos,"~/CLAUDIA-DATOS-/cuartos_formato2.csv",row.names = FALSE)
write.csv(corrales,"~/CLAUDIA-DATOS-/corrales_formato2.csv",row.names = FALSE)



cuartos<-read.csv("~/CLAUDIA-DATOS-/cuartos_formato2.csv")
cuartos<-as.data.table(cuartos)
#Seleccionando las localidades que me interesa
cuartos<-cuartos[L ==34|L==38|L==39|L==78|L==81|L==82|L==82]
formato_2<-as.data.table(formato_2)
f2_cuartos<-formato_2[intra_peri=="INTRA"]
f2_corrales<-formato_2[intra_peri=="PERI"]

# mergeando cuartos con formato 2
cuartos_formato_2<-merge(cuartos,f2_cuartos,by = .EACHI , all.x = TRUE,allow.cartesian = TRUE)
cuartos_formato_2<-as.data.table(cuartos_formato_2)

#cuartos_formato_2<-cuartos_formato_2[intra_peri=="INTRA"]
write.csv(cuartos_formato_2,"~/CLAUDIA-DATOS-/cuartos_formato_2.csv",row.names = FALSE)

#mergeando corrales
corrales<-read.csv("~/CLAUDIA-DATOS-/corrales_formato2.csv")
corrales<-as.data.table(corrales)
#Seleccionando las localidades que me interesa
corrales<-corrales[L ==34|L==38|L==39|L==78|L==81|L==82|L==82]

# mergeando corrales con formato 2
corrales_formato_2<-merge(corrales,f2_corrales,by = .EACHI , all.x = TRUE,allow.cartesian = TRUE)

write.csv(corrales_formato_2,"~/CLAUDIA-DATOS-/corrales_formato2.csv",row.names = FALSE)



