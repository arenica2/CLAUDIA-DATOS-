library(data.table)

#Definiendo las rutas para trabajar 
ruta1<-("~/PETM-shiny/spray_I_II_2009_2011/")
ruta2<-("~/PETM-shiny/Static_Data_formodel/")
#Seteando las rutas necesarias  
setwd(ruta1)


#leyendo los archivos necesario para obtener la data de lo ocho distritos
#Hunter,JLyB Rivero ,La Joya ,Paucarpata ,Sachaca,Uchumayo,Tiabaya,Socabaya.
byhoue<-fread("byHouse_fullEID.csv")
byhoue[,c("block","distrito"):=tstrsplit(id_manz,"-",fixed=TRUE)]
byhouse_manzanas<-byhoue[,c("UNICODE","P","D","L","V","lon","lat","Ispray","1_date","IIspray","2_date","Iinfested","IIinfested","id_manz","block")]

setwd(ruta2)
write.table(byhouse_manzanas,"byhouse_manzanas.csv",row.names = FALSE)
for