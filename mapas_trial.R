library(dplyr)
library(leaflet)
sachaca<-fread('~/PETM-shiny/unicode_numberMzn/Manzanas _Arequipa/Sachacha_correcciones/Sachaca_points_15FEB2017.csv')
sachaca <- data.table(sachaca, Loc = paste(sachaca$P, sachaca$D,sachaca$L, sep = "."))

tiabbaya<-fread('~/PETM-shiny/unicode_numberMzn/Manzanas _Arequipa/Tiabaya_correcciones/Tiabaya_points_15FEB2017.csv')
tiabbaya <- data.table(tiabbaya, Loc = paste(tiabbaya$P, tiabbaya$D,tiabbaya$L, sep = "."))


todo<-rbind(tiabbaya,sachaca)
polygon_sach<-fread('~/Downloads/SEARCH_ZONE_SACHACA.csv')
polygon_sach <- polygon_sach[order(POLYGON_TO$ident),]

polygon_TIA<-fread('~/Downloads/SEARCH_ZONE_TIABAYA.csv')
polygon_TIA <- polygon_TIA[order(POLYGON_TO$ident),]




factpal <- colorFactor(topo.colors(61), todo$Loc)


zonas_trial<-leaflet(todo) %>% addTiles()%>% setView(-71.54, -16.42, zoom = 12)  %>% 
  addCircles(stroke = FALSE, fillOpacity = 1,
             color = ~factpal(Loc))%>%
  addPolygons(data = polygon_sach, lng = polygon_sach$long, lat = polygon_sach$lat, group = 'ident'
  )%>%
  addPolygons(data = polygon_TIA, lng = polygon_TIA$long, lat = polygon_TIA$lat, group = 'ident',fillOpacity = 0.0)
              
mapshot(zonas_trial, file = paste0(getwd(), "/maps_trial.png"))

  


library(difR)





