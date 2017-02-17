# comparando las bases de puntos gogle earth con las bases corregidas 
library(data.table)

setwd("~/CLAUDIA-DATOS-/")
asa_points<-read.csv("~/CLAUDIA-DATOS-/ASA_points_corregido.csv")
ASA_POINTS_1<-read.csv("~/CLAUDIA-DATOS-/claudia codigos r/AREQUIPA_GPS_GOOGLE.csv")


SOCABAYA_POINTS<-read.csv("~/CLAUDIA-DATOS-/Socabaya_points_corregido.csv",sep = ",")
adedd<-read.csv("~/CLAUDIA-DATOS-/claudia codigos r/added_aqp_2006_2015.csv")
adedd<-as.data.table(adedd)
adedd<-adedd[D== 25]

SOCABAYA_POINTS<-as.data.table(SOCABAYA_POINTS)

x[!duplicated(x)]

diff1<-setdiff(adedd$UNICODE,SOCABAYA_POINTS$UNICODE) # 10 viviendas
diff2<-setdiff(SOCABAYA_POINTS$UNICODE,adedd$UNICODE) # 5055
#Interseccion
interseption<-intersect(SOCABAYA_POINTS$UNICODE, adedd$UNICODE)

#Merge
mmelgar_gps_rociado <- merge(aqp_gps_block,attack_mm, all= TRUE, by = "UNICODE")
#Comprobando
aux1 <- SOCABAYA_POINTS[SOCABAYA_POINTS$UNICODE%in%diff1,]
aux2 <- mmelgar_gps_rociado[mmelgar_gps_rociado$UNICODE%in%diff1,]
aux3 <- mmelgar_gps_rociado[mmelgar_gps_rociado$UNICODE%in%diff2,]

asa_points<-as.data.table(asa_points)
ASA_POINTS_1<-as.data.table(ASA_POINTS_1)

ASA_POINTS_1<-ASA_POINTS_1[P==1]
#ESCOGIENDO LOS D=7,8,9,13,18,23,24,25.
ASA_POINTS_1<-ASA_POINTS_1[D==25]


asa_p<-as.data.table(asa_points)
#añadiendo las varables para el file final 
asa_points<-asa_points[, c("P", "D","L","V") := tstrsplit(ident, ".", fixed=TRUE)]
setnames(asa_points,"ident","UNICODE")
setnames(asa_points,"lat","LATITUDE")
setnames(asa_points,"long","LONGITUDE")

SOCABAYA_POINTS<-SOCABAYA_POINTS[, c("P","D","L","V") := tstrsplit(ident, ".", fixed=TRUE)]
setnames(SOCABAYA_POINTS,"ident","UNICODE")
setnames(SOCABAYA_POINTS,"lat","LATITUDE")
setnames(SOCABAYA_POINTS,"long","LONGITUDE")
SOCABAYA_POINTS<-as.data.table(SOCABAYA_POINTS)
SOCABAYA_POINTS<-SOCABAYA_POINTS[,c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")]
write.csv(SOCABAYA_POINTS,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/puntos_corregidos/SOCABAYA_POINTS.csv",row.names = FALSE)

HUNTER_POINTS<-read.csv("~/CLAUDIA-DATOS-/Hunter_points_06FEB2017.csv")
HUNTER_POINTS<-as.data.table(HUNTER_POINTS)
HUNTER_POINTS<-HUNTER_POINTS[, c("P","D","L","V") := tstrsplit(ident, ".", fixed=TRUE)]
setnames(HUNTER_POINTS,"ident","UNICODE")
setnames(HUNTER_POINTS,"lat","LATITUDE")
setnames(HUNTER_POINTS,"long","LONGITUDE")
HUNTER_POINTS<-as.data.table(SOCABAYA_POINTS)
HUNTER_POINTS<-HUNTER_POINTS[,c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")]
write.csv(HUNTER_POINTS,"~/CLAUDIA-DATOS-/claudia codigos r/puntos_corregidos/HUNTER_POINTS.csv",row.names = FALSE)




JLB_POINTS<-read.csv("~/CLAUDIA-DATOS-/JLByRivero_points_corregido.csv",sep = ",")
JLB_POINTS<-JLB_POINTS[, c("P","D","L","V") := tstrsplit(ident, ".", fixed=TRUE)]
JLB_POINTS<-as.data.table(JLB_POINTS)
setnames(JLB_POINTS,"ident","UNICODE")
setnames(JLB_POINTS,"lat","LATITUDE")
setnames(JLB_POINTS,"long","LONGITUDE")

JLB_POINTS<-JLB_POINTS[,c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")]
write.csv(JLB_POINTS,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/puntos_corregidos/JLB_POINTS.csv",row.names = FALSE)



LAJOYA_POINTS<-read.csv("~/CLAUDIA-DATOS-/La_Joya_points_corregido.csv")
LAJOYA_POINTS<-as.data.table(LAJOYA_POINTS)
LAJOYA_POINTS<-LAJOYA_POINTS[, c("P","D","L","V") := tstrsplit(ident, ".", fixed=TRUE)]
setnames(LAJOYA_POINTS,"ident","UNICODE")
setnames(LAJOYA_POINTS,"lat","LATITUDE")
setnames(LAJOYA_POINTS,"long","LONGITUDE")

LAJOYA_POINTS<-LAJOYA_POINTS[,c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")]
write.csv(LAJOYA_POINTS,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/puntos_corregidos/LAJOYA_POINTS.csv",row.names = FALSE)


SACHACA_POINTS<-read.csv("~/CLAUDIA-DATOS-/Sachaca_points_02FEB2017.csv")
SACHACA_POINTS<-as.data.table(SACHACA_POINTS)
SACHACA_POINTS<-SACHACA_POINTS[, c("P","D","L","V") := tstrsplit(ident, ".", fixed=TRUE)]
setnames(SACHACA_POINTS,"ident","UNICODE")
setnames(SACHACA_POINTS,"lat","LATITUDE")
setnames(SACHACA_POINTS,"long","LONGITUDE")
SOCABAYA_POINTS<-as.data.table(SOCABAYA_POINTS)
SACHACA_POINTS<-SACHACA_POINTS[,c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")]
write.csv(SACHACA_POINTS,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/puntos_corregidos/SACHACA_POINTS.csv",row.names = FALSE)


UCHUMAYO_POINTS<-read.csv("~/CLAUDIA-DATOS-/Uchumayo_points_07FEB2017.csv")
UCHUMAYO_POINTS<-as.data.table(UCHUMAYO_POINTS)
UCHUMAYO_POINTS<-UCHUMAYO_POINTS[, c("P","D","L","V") := tstrsplit(ident, ".", fixed=TRUE)]
setnames(UCHUMAYO_POINTS,"ident","UNICODE")
setnames(UCHUMAYO_POINTS,"lat","LATITUDE")
setnames(UCHUMAYO_POINTS,"long","LONGITUDE")
UCHUMAYO_POINTS<-UCHUMAYO_POINTS[,c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")]
write.csv(UCHUMAYO_POINTS,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/puntos_corregidos/UCHUMAYO_POINTS.csv",row.names = FALSE)

TIABAYA_POINTS<-read.csv("~/CLAUDIA-DATOS-/Tiabaya_points_corregido.csv")
TIABAYA_POINTS<-as.data.table(TIABAYA_POINTS)
TIABAYA_POINTS<-TIABAYA_POINTS[, c("P","D","L","V") := tstrsplit(ident, ".", fixed=TRUE)]
setnames(TIABAYA_POINTS,"ident","UNICODE")
setnames(TIABAYA_POINTS,"lat","LATITUDE")
setnames(TIABAYA_POINTS,"long","LONGITUDE")

TIABAYA_POINTS<-TIABAYA_POINTS[,c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")]
write.csv(TIABAYA_POINTS,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/puntos_corregidos/TIABAYA_POINTS.csv",row.names = FALSE)

PAUCARPATA_POINTS<-read.csv("~/CLAUDIA-DATOS-/Paucarpata_points_07FEB2017.csv")
PAUCARPATA_POINTS<-as.data.table(PAUCARPATA_POINTS)
PAUCARPATA_POINTS<-PAUCARPATA_POINTS[, c("P","D","L","V") := tstrsplit(ident, ".", fixed=TRUE)]
setnames(PAUCARPATA_POINTS,"ident","UNICODE")
setnames(PAUCARPATA_POINTS,"lat","LATITUDE")
setnames(PAUCARPATA_POINTS,"long","LONGITUDE")

PAUCARPATA_POINTS<-PAUCARPATA_POINTS[,c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")]
write.csv(PAUCARPATA_POINTS,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/puntos_corregidos/PAUCARPATA_POINTS.csv",row.names = FALSE)


CAYMA_POINTS<-read.csv("~/CLAUDIA-DATOS-/Cayma_points_corregido.csv")
CAYMA_POINTS<-as.data.table(CAYMA_POINTS)
CAYMA_POINTS<-CAYMA_POINTS[, c("P","D","L","V") := tstrsplit(ident, ".", fixed=TRUE)]
setnames(CAYMA_POINTS,"ident","UNICODE")
setnames(CAYMA_POINTS,"lat","LATITUDE")
setnames(CAYMA_POINTS,"long","LONGITUDE")

CAYMA_POINTS<-CAYMA_POINTS[,c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")]
write.csv(CAYMA_POINTS,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/puntos_corregidos/CAYMA_POINTS.csv",row.names = FALSE)


ASA_POINTS<-read.csv("~/CLAUDIA-DATOS-/ASA_points_corregido.csv")
ASA_POINTS<-as.data.table(ASA_POINTS)
ASA_POINTS<-ASA_POINTS[, c("P","D","L","V") := tstrsplit(ident, ".", fixed=TRUE)]
setnames(ASA_POINTS,"ident","UNICODE")
setnames(ASA_POINTS,"lat","LATITUDE")
setnames(ASA_POINTS,"long","LONGITUDE")

ASA_POINTS<-ASA_POINTS[,c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")]
write.csv(ASA_POINTS,"~/CLAUDIA-DATOS-/claudia codigos r/MERGES_BLOCKS_GPS_ROCIADO/puntos_corregidos/ASA_POINTS.csv",row.names = FALSE)


MIRAFLORES_POINTS<-read.csv("~/CLAUDIA-DATOS-/MIRAFLORES_points_corregido.csv",)
MIRAFLORES_POINTS<-as.data.table(MIRAFLORES_POINTS)
MIRAFLORES_POINTS<-MIRAFLORES_POINTS[, c("P","D","L","V") := tstrsplit(ident, ".", fixed=TRUE)]
setnames(MIRAFLORES_POINTS,"ident","UNICODE")
setnames(MIRAFLORES_POINTS,"lat","LATITUDE")
setnames(MIRAFLORES_POINTS,"long","LONGITUDE")

MIRAFLORES_POINTS<-MIRAFLORES_POINTS[,c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")]
write.csv(MIRAFLORES_POINTS,"~/CLAUDIA-DATOS-/claudia codigos r/puntos_corregidos/MIRAFLORES_POINTS.csv",row.names = FALSE)



CHARACATO_POINTS<-read.csv("~/CLAUDIA-DATOS-/Characato_points_03FEB2017.csv")
CHARACATO_POINTS<-as.data.table(CHARACATO_POINTS)
CHARACATO_POINTS<-CHARACATO_POINTS[, c("P","D","L","V") := tstrsplit(ident, ".", fixed=TRUE)]
setnames(CHARACATO_POINTS,"ident","UNICODE")
setnames(CHARACATO_POINTS,"lat","LATITUDE")
setnames(CHARACATO_POINTS,"long","LONGITUDE")

CHARACATO_POINTS<-CHARACATO_POINTS[,c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")]
write.csv(CHARACATO_POINTS,"~/CLAUDIA-DATOS-/claudia codigos r/puntos_corregidos/CHARACATO_POINTS.csv",row.names = FALSE)

YARABAMBA_POINTS<-read.csv("~/CLAUDIA-DATOS-/Yarabamba_points_corregido.csv")
YARABAMBA_POINTS<-as.data.table(YARABAMBA_POINTS)
YARABAMBA_POINTS<-YARABAMBA_POINTS[, c("P","D","L","V") := tstrsplit(ident, ".", fixed=TRUE)]
setnames(YARABAMBA_POINTS,"ident","UNICODE")
setnames(YARABAMBA_POINTS,"lat","LATITUDE")
setnames(YARABAMBA_POINTS,"long","LONGITUDE")

YARABAMBA_POINTS<-YARABAMBA_POINTS[,c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")]
write.csv(YARABAMBA_POINTS,"~/CLAUDIA-DATOS-/claudia codigos r/puntos_corregidos/YARABAMBA_POINTS.csv",row.names = FALSE)




asa_points<-asa_points[, c("P", "D","L","V") := tstrsplit(ident, ".", fixed=TRUE)]
setnames(asa_points,"ident","UNICODE")
setnames(asa_points,"lat","LATITUDE")
setnames(asa_points,"long","LONGITUDE")






asa_points<-asa_points[,c("UNICODE","P","D","L","V","LATITUDE","LONGITUDE")]
#seleccionando las variables  que interesan 
n_row<-nrow(asa_points)
asa_points$LATITUDE<-as.character(asa_points$LATITUDE)
asa_points$LONGITUDE<-as.character(asa_points$LONGITUDE)
asa_points$UNICODE<-as.character(asa_points$UNICODE)
asa_points$P<-as.character(asa_points$P)
asa_points$D<-as.character(asa_points$D)
asa_points$L<-as.character(asa_points$L)
asa_points$V<-as.character(asa_points$V)


asa_points_1$LATITUDE<-as.character(asa_points_1$LATITUDE)
asa_points_1$LONGITUDE<-as.character(asa_points_1$LONGITUDE)
asa_points_1$UNICODE<-as.character(asa_points_1$UNICODE)
asa_points_1$P<-as.character(asa_points_1$P)
asa_points_1$D<-as.character(asa_points_1$D)
asa_points_1$L<-as.character(asa_points_1$L)
asa_points_1$V<-as.character(asa_points_1$V)


AUX1<-setdiff(ASA_POINTS_1,asa_points)
AU1<-setdiff(ASA_POINTS_1$UNICODE,asa_points$UNICODE)
AUX<-fsetdiff(asa_points,asa_points_1, all=TRUE)
AUX2<-as.data.table(AUX)

AU1<-as.data.table(AU1)
ASA<-intersect(asa_points, asa_points_1)










write.csv(asa_points,"~/CLAUDIA-DATOS-/new_points_aqp//asa_points_05feb.csv",row.names = FALSE)



