






#Leer mis archivos
puntos<- read.csv("~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-03-02.csv",sep = " ")
linea_carlos<-read.csv("/Users/Rodrigo/Downloads/Carlos/Carlos_26APRIL2017_GPS7.csv",sep=",")
poligonos<-read.csv("~/CLAUDIA-DATOS-/claudia codigos r/Manzanas _Arequipa/Mariano Melgar/MARIANO MELGAR.csv",sep = ",")

lineas_caminando <- read.csv("ruta_caminando.csv",sep=";")
lineas_movi <- read.csv("ruta_movilidad.csv",sep=";")
poligonos<-read.csv("~/CLAUDIA-DATOS-/claudia codigos r/Manzanas _Arequipa/Mariano Melgar/MARIANO MELGAR.csv", sep=",")
library(data.table)
poligonos<-as.data.table(poligonos)
poligonos[poligonos==" "]<- NA
poligonos[poligonos==""]<- NA
poligonos[poligonos==" "]<- 'NA'
poligonos[poligonos==""]<- 'NA'



c

linea_carlos<-as.data.table(linea_carlos)
linea_carlos[lineas_carlos==" "]<-"NA"
#Almacenando los campos "LONGITUD" y"LATITUD"
puntos <- puntos[, c("LONGITUDE","LATITUDE")]
lineas_caminando <- lineas_caminando[, c("ident","long","lat")]
lineas_movi <- lineas_movi[, c("ident","long","lat")]
#rio <- rio[, c("ident","long","lat")]
poligonos <- poligonos[, c("ident","long","lat")]
#puntos_torrenteras <- puntos_torrenteras[, c("long","lat")]






x11()
plot(puntos, col = "red", pch=1.5, cex = 0.5, cex.main=0.5, main = "MODEL_RESULT")
#puntos vacunacion
#plot(puntos$long,puntos$lat, col = "red", pch=19, cex = 1, cex.main=0.8, main = "Puntos vacunacion")
#with(puntos,(plot(puntos$long,puntos$lat,xlab="longitud",ylab = "latitud", main = "puntos de vacunacion")))
quartz()
#poligonos
plot.new()
x<-NULL
y<-NULL
n_row <- nrow(poligonos)+1
for (i in 2:n_row) {
  if (!is.na(poligonos[i,3])) {
    x<-c(x,poligonos[i,4])
    y<-c(y,poligonos[i,3])
  }
  else{
    polygon (x,y, border = "blue", lwd = 1)
    #a<-mean(x)
    #b<-mean(y)
    labels(x,y)
    x<-NULL
    y<-NULL
  }
}

#ruta caminando
x<-NULL
y<-NULL
n_row <- nrow(linea_carlos)+1
for (i in 1:n_row) {
  if (!is.na(linea_carlos[i,3])) {
    x<-c(x,linea_carlos[i,3])
    y<-c(y,linea_carlos[i,4])
  }
  else{
    lines(x,y,col="black",pch=19, lwd=3)
    x<-NULL
    y<-NULL
  }
}

#ruta movilidad
x<-NULL
y<-NULL
n_row <- nrow(lineas_movi)+1
for (i in 2:n_row) {
  if (!is.na(lineas_movi[i,2])) {
    x<-c(x,lineas_movi[i,2])
    y<-c(y,lineas_movi[i,3])
  }
  else{
    lines(x,y,col="blue",pch=19, lwd=3)
    x<-NULL
    y<-NULL
  }
}



dev.print(device = pdf,"Vacunacion_Rabia_2016.pdf")

summary(poligonos)
summary(lineas_caminando)
summary(lineas_movi)

points(points, col = "darkgreen", pch=2, cex = 1)


#Leyenda
#En la parte inferior izquierda  
#legend(x=-71.605, y=-16.44, legend = "Torrenteras", col = "blue", lty=1, bty="n")
#legend(x=-71.596, y=-16.447, bty="n",c("Puentes","Casos de rabia"),col=c("green","red"),pch=c(2,19))
#En la parte superior derecha
legend(x=-71.50, y=-16.305, legend = "Torrenteras", col = "blue", lty=1, bty="n")
legend(x=-71.491, y=-16.311, bty="n",c("Puentes","Casos de rabia"),col=c("darkgreen","red"),pch=c(2,19))
