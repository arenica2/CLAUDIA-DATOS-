library(data.table)

# fill in latitude and longitude information
setwd(lat_long_direc)
latLong <- fread("~/Downloads/Puntos_ASA_01AUGUST2018_actualizados_con_adicionada.csv")
latLong <- data.frame(lapply(latLong, as.character), stringsAsFactors=FALSE)
addLatLong <- function(df) {
  df$lat <- NA
  df$long <- NA
  for (z in 1:nrow(df)) {
    print(z)
    toCompare <- df$UNICODE[z]
    rowNum <- which(latLong$UNICODE == toCompare)
    if (length(rowNum != 0)) {
      df$lat[z] <- latLong$lat[rowNum]
      df$long[z] <- latLong$long[rowNum]
    }
  }
  return(df)
}
setwd(lat_long_direc)
latLong <- read.csv("AQP_GPS_GOOGLE_EARTH_PUNTOS_05_jun_2017.csv")
latLong <- data.frame(lapply(latLong, as.character), stringsAsFactors=FALSE)
latLong <- filterDatabase(latLong)
df<-CASAS_ASA
for (z in 1:nrow(df)) {

  rowNum <- which(latLong$UNICODE == df$UNICODE[z])#[1]
  if (length(rowNum) == 0) {
    rowNum <- which(latLong$UNICODE == gsub("[A-z]", "", df$UNICODE[z]))#gsub("A", "", gsub("B", "", df$UNICODE[z])))
  }
  #Yo eliminaria esto, por que nunca va a entrar aqui
  #if (length(rowNum) > 1) {
  #  print(paste0("error in ", df$UNICODE[z]))
  #}
  #Elimino el IF por que no es necesario (llegue a esta conclusion despues de analizar todas las opciones)
  #if (length(rowNum != 0)) {
  df$LATITUDE[z] <- latLong$lat[rowNum]
  df$LONGITUDE[z] <- latLong$long[rowNum]
  #}
}

CASAS_ASA<-fread('~/Rabies/Optimized_tents/demand_points/houses_ASA2016_en_09Oct18.csv')
CASAS_ASA<-CASAS_ASA[!is.na(CASAS_ASA$LATITUDE),]
CASAS_ASA<-rbind(CASAS_ASA,df)
CASAS_ASA<-merge(CASAS_ASA,latLong, by='UNICODE', all.x=TRUE)
CASAS_ASA<-addLatLong(CASAS_ASA)
df_denuncias1_date <- df_denuncias1[!is.na(df_denuncias1$den_YEAR),]


write.csv(CASAS_ASA,"~/Rabies/Optimized_tents/demand_points/houses_ASA2016_en_09Oct18.csv",row.names = FALSE)







