
library(data.table)

asa<-read.csv("~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/ASA_gps_rociado.csv")
cayma<-read.csv("~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/CAYMA_gps_rociado.csv")
melgar<-read.csv("~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/mmelgar_gps_rociado.csv")
socabaya<-read.csv("~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/SOCABAYA_gps_rociado.csv")
jlbr<-read.csv("~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/JLB_RIVERO_gps_rociado.csv")
characato<-read.csv("~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/CHARACATO_gps_rociado.csv")
hunter<-read.csv("~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/HUNTER_gps_rociado.csv")
lajoya<-read.csv("~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/LA_JOYA_gps_rociado.csv")
miraflores<-read.csv("~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/MIRAFLORES_rociado.csv")
paucarpata<-read.csv("~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/PAUCARPATA_gps_rociado.csv")

tiabaya<-read.csv("~/PETM-shiny/Static_Data_formodel/MERGES_BLOCKS_GPS_ROCIADO/TIABAYA_gps_rociado.csv")


socabaya<-as.data.table(socabaya)
s<-socabaya[L == 45]
melgar<-as.data.table(melgar)
m<-melgar[L=="14A" ]
miraflores<-as.data.table(miraflores)
mi<-miraflores[L==60]
tiabaya<-as.data.table(tiabaya)
t<-tiabaya[L==14 ]
miraflores<-as.data.table(miraflores)
mi<-miraflores[L== 55]




inpecciones<-read.csv("~/PETM-shiny/Static_Data_formodel/inspections_AQP_ALL_DISTRICTS.csv")

which(is.na(t$P_TRIAT))
