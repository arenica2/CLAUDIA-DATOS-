library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(data.table)




CARLOS<-read.csv('~/PETM-shiny/PRUEBAS_MODEL/DATA_APP_PILOT/Carlos/CARLOS_02MAY2017_GPS5.csv')
CARLOS<-CARLOS[,c('Latitude','Longitude',"Date")]
MODEL_RESULT<- read.csv("~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-03-02.csv",sep = " ")
poligonos<-read.csv("~/PETM-shiny/PRUEBAS_MODEL/Manzanas _Arequipa/Mariano Melgar/MARIANO MELGAR.csv",sep = ",")
poligonos<-as.data.table(poligonos)
poligonos<-poligonos [lat!='NA']
poligonos<-poligonos[, c('CODELOC','BLOCK') := tstrsplit(ident, "-", fixed=TRUE)]
poligonos<-poligonos[CODELOC=='1.10.38']
poligonos <- poligonos[order(poligonos$BLOCK),]
inspections_app<-read.csv('~/PETM-shiny/BackUp_App_Inspections/Backup_Inspecctions_9_may_2017.csv',sep = ';')

library(data.table)
inspections_app<-as.data.table(inspections_app)
inspections_app<-inspections_app[TEST_DATA==0 & USER_NAME=='CCP_1V']
inspections_app<-inspections_app[PREDICTED_COLOR!='']
inspections_app<-inspections_app[, c('fecha','hora') := tstrsplit(DATETIME, " ", fixed=TRUE)]
inspections_app<-inspections_app[fecha=='2017-05-02']
setnames(inspections_app,'UNI_CODE','UNICODE')

inspections_app<-merge(inspections_app,MODEL_RESULT,by='UNICODE',all.x = TRUE)

#carlos<-as.matrix(table(inspections_app$USER_NAME))
#inspections<-as.matrix(table(inspections_app$STATUS_INSPECCION))








    
    
  shape = 21, colour = "black", fill = "white", size = 5 , stroke = 5
  x11()
  plot.new()
  ggplot()+
    geom_point(data = MODEL_RESULT, aes(x = MODEL_RESULT$LONGITUDE, y = MODEL_RESULT$LATITUDE), col=colfunc,cex=1,pch=19) +
    #point(MODEL_RESULT$LONGITUDE,MODEL_RESULT$LATITUDE)+
    coord_fixed(xlim = c(-71.505, -71.5000),  ylim = c(-16.405, -16.407), ratio = 1.3)+
  
  
    ggplot()+
    geom_polygon(data = poligonos, aes(x=poligonos$long, y = poligonos$lat, group = ident),size=0.2, fill = NA, color = "black")+ 
  
  
   ggplot()+
    geom_path(data = CARLOS, aes(x=CARLOS$Longitude,y=CARLOS$Latitude,group='Date'),col='blue', size = 0.3, lineend = "round")+ 
    scale_color_gradientn(colours = rainbow(7), breaks = seq(25, 200, by = 25))

    geom_point(data = inspections_app, mapping = aes(x = inspections_app$LONGITUDE, y = inspections_app$LATITUDE,group=STATUS_INSPECCION),shape='.', fill = NA)+
    geom_text(data = inspections_app, aes(x=inspections_app$LONGITUDE,y=inspections_app$LATITUDE,label = paste("  ", as.character(STATUS_INSPECCION), sep="")), angle = 0, hjust = 0, color = "black")
    
    
    
library(zoom)   
    
 zm
 
    
    
    
    
    
    
    
    
    
