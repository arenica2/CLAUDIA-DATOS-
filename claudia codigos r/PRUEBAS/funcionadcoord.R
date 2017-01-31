AdCoord <- function (data) {
  
  # quitando la letra adicionada de las casas del campo UNICODE
  sinAdicion <- as.data.frame(gsub("([A-Z])", "", data$UNICODE))
  
  # cambiando nombre de columna
  names(sinAdicion)[1] <- "UNICODE"
  
  # adicionando una columna como indice para juntar las tablas
  sinAdicion$indice <- seq(1:nrow(data))
  data$indice <- seq(1:nrow(data))
  
  # juntando las bases
  union <- merge(sinAdicion,data, by = "UNICODE", all.x=T)
  
  # extrayendo los campos que nos interesan
  union <- subset(union,UNICODE=1,select = c(indice.x,LATITUDE,LONGITUDE))
  union2 <- merge(data,union,by.x = "indice", by.y = "indice.x")
  
  # quitando columnas que no nos interesa y renombrando otras
  union2 <- union2[,-c(1,12,13)]
  names(union2)[11] <- "LATITUDE"
  names(union2)[12] <- "LONGITUDE" 
  return(union2) 
}
  
  
  
  
  
