######### Mariano Melgar data sets for Doctor Jeun #########
############################################################
library(data.table)
library(dplyr)
library(geosphere)
library(rgdal) # este es el paquete que no puedo instalar en MAC
library(sp)
library(sf)
library(tidyverse)

#### file with blocks ##########
block<-fread("~/PETM-shiny/PRUEBAS_MODEL/data_complete")
block<-block[D=="10"]
#Verificar si hay duplicados
block<-block %>% distinct(UNICODE, .keep_all = TRUE)
block<-as.data.frame(block)
## getting the no information data ###
noinf<- block[which(block$no_information==TRUE),]


noinf<-as.data.table(noinf)
block<-as.data.table(block)
block<-rbind(noinf,block)


#################### reading data sets from surveys ##################
gps<-fread("~/PETM-shiny/unicode_numberMzn/AREQUIPA_GPS_GOOGLE/AQP_GPS_GOOGLE_EARTH_PUNTOS_30_abril_2019.csv")
gps<-gps[,c("UNICODE","LATITUDE","LONGITUDE")]


##### rociado ######
rociadomm<-fread("~/PETM-shiny/PRUEBAS_MODEL/data_roc.csv")
rociadomm<-rociadomm[D=="10"]
# rociadomm<-rociadomm [L=="2"|L=="9"|L=="10"|L=="12"|L=="13"|L=="13A"|L=="14"]
rociadomm<-rociadomm[,-c(1,15)]
rociadomm<-rociadomm %>% distinct(UNICODE, .keep_all = TRUE)
sapply(rociadomm, class)
rociadomm$block<-as.character(rociadomm$block)
### getting prevalences by block in rociado ##### 

alllocalities<-sort(unique(rociadomm$L),decreasing = FALSE)
p <- data.frame(matrix(ncol=3, nrow=0))

for (i in seq_along(alllocalities)) {
  rociadomm.i<-rociadomm[which(rociadomm$L==alllocalities[i]),]
  allblock<-sort(unique(rociadomm.i$block),decreasing = FALSE)
  
  for (j in seq_along(allblock)) {

    rociadomm.j<-rociadomm.i[which(rociadomm.i$block==allblock[j]),]
    p.j<-length(unique(which(rociadomm.j$pos_sprayed==1)))/length(unique(rociadomm.j$UNICODE))*100
    p.j<-cbind(alllocalities[i],allblock[j], p.j)
    p<-rbind(p, p.j)
  }
}

colnames(p)[colnames(p)=="V1"] <- "Localidad"
colnames(p)[colnames(p)=="V2"] <- "Block"
colnames(p)[colnames(p)=="p.j"] <- "P_spray"

p$block<-paste("1.10",p$Localidad,sep = ".")
p$block<-paste(p$block,p$Block,sep = "-")
p<-p[,c("block","P_spray")]


write.csv(p,"~/PETM-shiny/PRUEBAS_MODEL/prev_spray_block.csv")
######## encuesta ############
encuestasmm<-fread("~/Downloads/BASE_MARIANOMELGAR_CORREGIDA24_7_9.csv")
# encuestasmm<-encuestasmm [L=="2"|L=="9"|L=="10"|L=="12"|L=="13"|L=="13A"|L=="14"]
encuestasmm<-merge(encuestasmm,gps,by="UNICODE", all.x = TRUE)
encuestasmm<-encuestasmm[,c("UNICODE","(+)","Fecha","P","D","L","V","LATITUDE","LONGITUDE")]
setnames(encuestasmm,"(+)","pos_enc")
setnames(encuestasmm,"Fecha","FECHA")
table(encuestasmm$L)
encuestasmm<-encuestasmm %>% distinct(UNICODE, .keep_all = TRUE)
encuestasmm<-encuestasmm[,-c(8,9)]

###uniendo encuestas con el numero de cuadras() ######
block <-block[,c("UNICODE","block","LONGITUDE","LATITUDE")]
enc_block<-merge( encuestasmm,block,by="UNICODE",all.x=TRUE)
enc_block<-enc_block%>%distinct(UNICODE,.keep_all = TRUE)
enc_block<-enc_block[pos_enc==9,pos_enc:=0]

#### encuestas
alllocalities<-sort(unique((enc_block$L)),decreasing = FALSE)
q <- data.frame(matrix(ncol=3, nrow=0))

for (i in seq_along(alllocalities)) {

  enc.i<-enc_block[which(enc_block$L==alllocalities[i]),]
  allblock<-sort(unique(enc.i$block),decreasing = FALSE)
  
  for (j in seq_along(allblock)) {

    enc.j<-enc.i[which(enc.i$block==allblock[j]),]
    q.j<-length(unique(which(enc.j$pos_enc==1)))/length(unique(enc.j$UNICODE))*100
    q.j<-cbind(alllocalities[i],allblock[j], q.j)
    q<-rbind(q, q.j)
  }
}

colnames(q)[colnames(q)=="V1"] <- "Localidad"
colnames(q)[colnames(q)=="V2"] <- "Block"
colnames(q)[colnames(q)=="q.j"] <- "P_enc"

q$block<-paste("1.10",q$Localidad,sep = ".")
q$block<-paste(q$block,q$Block,sep = "-")
q<-q[,c("block","P_enc")]
write.csv(q,"~/PETM-shiny/PRUEBAS_MODEL/prev_survey_block.csv")


