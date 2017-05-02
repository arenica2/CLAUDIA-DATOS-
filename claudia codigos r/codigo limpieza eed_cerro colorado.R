
## codigo limpieza duplicado de base encuesta cerro colorado 
library(data.table)
eed_CERROCOLORADO<-read.csv("/Users/Rodrigo/Downloads/EED_MINSA CERRO COLORADO enero2017.xlsx - Hoja1.csv", stringsAsFactors = FALSE)
eed_CERROCOLORADO<-as.data.table(eed_CERROCOLORADO)

DUPLICADOS<-eed_CERROCOLORADO[duplicated(eed_CERROCOLORADO$UNICODE)]


# imprimiendo los duplicados 
write.csv(DUPLICADOS,"/Users/Rodrigo/Downloads/Dupicados_MINSA_EEDCERRO.csv",row.names = FALSE)
#### eliminando duplicados 
eed_CERROCOLORADO<-eed_CERROCOLORADO[!duplicated(eed_CERROCOLORADO$UNICODE,fromLast = FALSE), ]



write.csv(eed_CERROCOLORADO,"/Users/Rodrigo/Downloads/EED_MINSA CERRO COLORADO enero2017.xlsx - Hoja1.csv",row.names = FALSE)
