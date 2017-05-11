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



usa <- map_data("usa") # we already did this, but we can do it again
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group))+
  points(MODEL_RESULT$LONGITUDE,MODEL_RESULT$LATITUDE)+
  

plot.new()
x11()
ggplot()+
  geom_point(data = MODEL_RESULT, aes(x = MODEL_RESULT$LONGITUDE, y = MODEL_RESULT$LATITUDE), col=colfunc,cex=1,pch=19) +
  points(MODEL_RESULT$LONGITUDE,MODEL_RESULT$LATITUDE)+
  coord_fixed(xlim = c(-123, -121.0),  ylim = c(36, 38), ratio = 1.3)
ggplot()+
 geom_polygon(data = poligonos, aes(x=poligonos$long, y = poligonos$lat, group = 'ident'),size=0.3, fill = NA, color = "green") 
 geom_path(data = CARLOS, aes(x=CARLOS$Longitude, y = CARLOS$Latitude, group = 'Date'), size = 0.5, lineend = "round") + 
  scale_color_gradientn(colours = rainbow(7), breaks = seq(25, 200, by = 25))

  bike <- read.csv("data/bike-ride.csv")
  head(bike)
  #>         lon      lat elevation                 time
  #> 1 -122.0646 36.95144      15.8 2011-12-08T19:37:56Z
  #> 2 -122.0646 36.95191      15.5 2011-12-08T19:37:59Z
  #> 3 -122.0645 36.95201      15.4 2011-12-08T19:38:04Z
  #> 4 -122.0645 36.95218      15.5 2011-12-08T19:38:07Z
  #> 5 -122.0643 36.95224      15.7 2011-12-08T19:38:10Z
  #> 6 -122.0642 36.95233      15.8 2011-12-08T19:38:13Z
  
  
  bikemap1 <- get_map(location = c(-122.080954, 36.971709), maptype = "terrain", source = "google", zoom = 14)
  #> Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=36.971709,-122.080954&zoom=14&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
  ggmap(bikemap1) + 
    geom_path(data = bike, aes(color = elevation), size = 3, lineend = "round") + 
    scale_color_gradientn(colours = rainbow(7), breaks = seq(25, 200, by = 25))

  bikemap1 <- get_map(location = c(-16.42563,-71.52599), maptype = "n", source = "osm", zoom = 14)
  #> Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=36.971709,-122.080954&zoom=14&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
  ggmap(bikemap1) + 
    
   
    
    
    
    
    
    
    
    
    
  
  
  plot.new()
  ggplot()+
    geom_point(data = MODEL_RESULT, aes(x = MODEL_RESULT$LONGITUDE, y = MODEL_RESULT$LATITUDE), col=colfunc,cex=1,pch=19) +
    points(MODEL_RESULT$LONGITUDE,MODEL_RESULT$LATITUDE)+
    coord_fixed(xlim = c(-71.5150, -71.5000),  ylim = c(-16.405, -16.411), ratio = 1.3)+
  
  
  
  
  
   ggplot()+
    geom_path(data = CARLOS, aes(x=CARLOS$Longitude,y=CARLOS$Latitude,group='Date'), size = 1, lineend = "round")+  
    scale_color_gradientn(colours = rainbow(7), breaks = seq(25, 200, by = 25))

   coord_fixed(xlim = c(-71.515, -71.500),  ylim = c(-16.40, -16.41), ratio = 1.3)
