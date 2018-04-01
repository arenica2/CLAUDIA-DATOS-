# This code explore formato 2 of the chiris project
# Call packages
library(ggplot2)
library(data.table)
library(raster)
#library(car)
library(dplyr)
library(plyr)
#install.packages("tmap")
library(tmap)
library(rgdal)
library(ggmap)
setwd("~/CLAUDIA-DATOS-/")
library(shape)
library(tmap)
library(memisc)
### CALL SHAPEFILES ###
## DISTRICT AND BEYOND...
tdcero <- tempdir() # save the directory
#peru0 <- getData('GADM', country=c('PER'), level=0, path=tdcero)
peru1 <- getData('GADM', country=c('PER'), level=1, path=tdcero)
peru2 <- getData('GADM', country=c('PER'), level=2, path=tdcero)
peru3 <- getData('GADM', country=c('PER'), level=3, path=tdcero)
#peru4 <- getData('GADM', country=c('PER'), level=4, path=tdcero)
# no level 4

# Plot shapefiles
#plot(peru0)
plot(shapefile)
plot(peru2)
plot(peru3)

# get df
peru1_df <- peru_1@data
peru2_df <- peru_2@data
peru3_df <- peru_3@data

## check variables in gadm2 - provinces
names(peru_2@data)
# [1] "OBJECTID"  "ID_0"      "ISO"       "NAME_0"    "ID_1"      "NAME_1"   
# [7] "ID_2"      "NAME_2"    "HASC_2"    "CCN_2"     "CCA_2"     "TYPE_2"   
# [13] "ENGTYPE_2" "NL_NAME_2" "VARNAME_2"
head(peru_2@data, 3)
# OBJECTID ID_0 ISO NAME_0 ID_1   NAME_1 ID_2      NAME_2   HASC_2 CCN_2 CCA_2
# 1        1  178 PER   Peru    1 Amazonas    1       Bagua PE.AM.BG    NA      
# 2        2  178 PER   Peru    1 Amazonas    2     Bongará PE.AM.BN    NA      
# 3        3  178 PER   Peru    1 Amazonas    3 Chachapoyas PE.AM.CP    NA      
# TYPE_2 ENGTYPE_2 NL_NAME_2 VARNAME_2
# 1 Provincia  Province                    
# 2 Provincia  Province             Bongart
# 3 Provincia  Province  

# Keep Arequipa only. It has Arequipa region with its 8 provinces
AQP_prov <-  peru_2[peru_2$ID_1==4,]
AQP_prov_df <- AQP_prov@data
plot(AQP_prov) # OK
writeOGR(AQP_prov, dsn=".", layer="claudia codigos r/PRUEBAS/AQP_region_provinces", 
         driver="ESRI Shapefile")


## check variables in gadm3 - district levels
names(peru_3@data)
head(peru_3@data, 3)
# OBJECTID ID_0 ISO NAME_0 ID_1   NAME_1 ID_2 NAME_2 ID_3   NAME_3 CCN_3 CCA_3
# 1        1  178 PER   Peru    1 Amazonas    1  Bagua    1 Aramango    NA      
# 2        2  178 PER   Peru    1 Amazonas    1  Bagua    2 Copallin    NA      
# 3        3  178 PER   Peru    1 Amazonas    1  Bagua    3 El Parco    NA      
# TYPE_3 ENGTYPE_3 NL_NAME_3 VARNAME_3
# 1 Distrito  District                    
# 2 Distrito  District                    
# 3 Distrito  District   

# Keep ONLY districts in Arequipa province
AQP_districts <-  peru_3[peru_3$ID_1==4,]
AQP_AQP_districts <- AQP_districts[AQP_districts$ID_2==35,]
plot(AQP_AQP_districts) # OK
AQP_AQP_districts_df <- AQP_AQP_districts@data
str(AQP_AQP_districts_df)
AQP_AQP_districts_df$fNAME_3 <- factor(AQP_AQP_districts_df$NAME_3)
levels(AQP_AQP_districts_df$fNAME_3)
# [1] "Alto Selva Alegre"             "Arequipa"                     
# [3] "Cayma"                         "Cerro Colorado"               
# [5] "Characato"                     "Chiguata"                     
# [7] "Jacobo Hunter"                 "Jose Luis Bustamante Y Rivero"
# [9] "La Joya"                       "Laguna Loriscota"             
# [11] "Mariano Melgar"                "Miraflores"                   
# [13] "Mollebaya"                     "Paucarpata"                   
# [15] "Pocsi"                         "Polobaya"                     
# [17] "Quequeña"                      "Sabandia"                     
# [19] "Sachaca"                       "San Juan de Siguas"           
# [21] "San Juan de Tarucani"          "Santa Isabel de Siguas"       
# [23] "Santa Rita de Siguas"          "Socabaya"                     
# [25] "Tiabaya"                       "Uchumayo"                     
# [27] "Vitor"                         "Yanahuara"                    
# [29] "Yarabamba"                     "Yura"  
writeOGR(AQP_AQP_districts, dsn=".", layer="claudia codigos r/PRUEBAS/AQP_AQP_province_districts", 
         driver="ESRI Shapefile")

# Plotear
plot(AQP_AQP_districts, col="ivory2")
plot(AQP_prov, add=TRUE, lwd=2)
#plot(peru0, add=TRUE, lwd=3)
box()

# Plot with districts to check
qtm(AQP_AQP_districts, fill="white") + 
  tm_text("NAME_3")
# OK

## Explore more the shapefile that we need
names(AQP_AQP_districts_df)
# [1] "OBJECTID"  "ID_0"      "ISO"       "NAME_0"    "ID_1"      "NAME_1"   
# [7] "ID_2"      "NAME_2"    "ID_3"      "NAME_3"    "CCN_3"     "CCA_3"    
# [13] "TYPE_3"    "ENGTYPE_3" "NL_NAME_3" "VARNAME_3" "fNAME_3"  
str(AQP_AQP_districts_df) 
# $ ID_3     : int  330 331 332 333 334 335 336 337 338 339 ...
# $ NAME_3   : chr  "Alto Selva Alegre" "Arequipa" "Cayma" "Cerro Colorado" ...

unique(sort(AQP_AQP_districts_df$ID_3))
# [1] 330 331 332 333 334 335 336 337 338 339 340 341 342 343 344 345 346 347 348 349
# [21] 350 351 352 353 354 355 356 357 358 359
levels(as.factor(AQP_AQP_districts_df$NAME_3))

# Recode names
AQP_AQP_districts$NAME_3 <- recode(AQP_AQP_districts$NAME_3,  
                                   `Alto Selva Alegre` = "A.S.A.", 
                                   `Cayma` = "CAYMA", 
                                   `Cerro Colorado` = "C. COLORADO", 
                                   `Jacobo Hunter` = "HUNTER", 
                                   `Jose Luis Bustamante Y Rivero` = "J. L. B Y R", 
                                   `Mariano Melgar` = "M. MELGAR", 
                                   `Miraflores` = "MIRAFLORES", 
                                   `Paucarpata` = "PAUCARPATA",
                                   `Sachaca` = "SACHACA", 
                                   `Socabaya` = "SOCABAYA",
                                   `Uchumayo` = "UCHUMAYO",
                                   `Yura` = "YURA",
                                   `Arequipa` = "AREQUIPA",
                                   `Characato` = "CHARACATO",
                                   `Chiguata` = "CHIGUATA",
                                   `La Joya` = "LA JOYA",
                                   `Laguna Loriscota` = "L. LORISCOTA",
                                   `Mollebaya` = "MOLLEBAYA",
                                   `Pocsi` = "POCSI",
                                   `Polobaya` = "POLOBAYA",
                                   `Quequeña` = "QUEQUEÑA",
                                   `Sabandia` = "SABANDIA",
                                   `San Juan de Siguas` = "S.J. Siguas",
                                   `San Juan de Tarucani` = "S.J. Tarucani",
                                   `Santa Isabel de Siguas` = "S.I. de Siguas",
                                   `Santa Rita de Siguas` = "S.R. de Siguas",
                                   `Tiabaya` = "TIABAYA",
                                   `Vitor` = "VITOR",
                                   `Yanahuara` = "YANAHUARA",
                                   `Yarabamba` = "YARABAMBA")
AQP_AQP_districts_df <- AQP_AQP_districts@data

# Plot again
qtm(AQP_AQP_districts, fill="white") + 
  tm_text("NAME_3")
# OK


### Create shapefiles of districts that we like

districts.i <- c("A.S.A.", "CAYMA", "C. COLORADO", "CHARACATO", "HUNTER",
                 "J. L. B Y R", "LA JOYA", "M. MELGAR", "MIRAFLORES",
                 "MOLLEBAYA", "PAUCARPATA", "SACHACA", "UCHUMAYO",
                 "TIABAYA", "SOCABAYA")

# for(i in districts.i){
#   print(i)
#   AQP_AQP_districts.i <- AQP_AQP_districts[AQP_AQP_districts$NAME_3 == i,]
#   AQP_AQP_districts.i_df <- AQP_AQP_districts.i@data
#   plot(AQP_AQP_districts.i)
#   writeOGR(AQP_AQP_districts.i, dsn=".", 
#            layer=paste("output/Casos_INS/Scripts/outputs/", i, "_district", sep=""), 
#            driver="ESRI Shapefile")
#   }
#   
####tarea estadistica y demografia#######

# mapas arequipa indicadores demograficos 2015-2020
# tasa de fecundidad ,tasa global de fecundidad y esperanza de vida al nacer

indicadores<-fread("~/CLAUDIA-DATOS-/cap03030.csv")


map <- get_map(location = c(lon =-71, lat = -16),
               zoom = 5, # 5 to set the map a country.
               maptype = "terrain",     # setting easy to visualise
               source = "google")       # the source to load the map from
plotmap <- ggmap(map) + xlab("Longitude") + ylab("Latitude")

library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(ggmaps)    # for fortifying shapefiles

# First read in the shapefile, using the path to the shapefile and the shapefile name minus the
# extension as arguments
peru_1<- shapefile("~/Spatial-surveillance/MAP_CODES/PERU_SHAPES/PER_adm0.shp")
peru_2<-shapefile("~/Spatial-surveillance/MAP_CODES/PERU_SHAPES/PER_adm1.shp")
peru_3<-shapefile("~/Spatial-surveillance/MAP_CODES/PERU_SHAPES/PER_adm3.shp")


# Next the shapefile has to be converted to a dataframe for use in ggplot2
shapefile_df <- fortify(peru_1)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
map <- ggplot() +
  geom_path(data = shapefile_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray', fill = 'white', size = .2)

print(map) 

# Using the ggplot2 function coord_map will make things look better and it will also let you change
# the projection. But sometimes with large shapefiles it makes everything blow up.
map_projected <- map +
  coord_map()

print(map_projected)


