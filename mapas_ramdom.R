### January##
library(sf)
library(data.table)
library("raster")
library("rgdal")
library("RQGIS")
library(ggplot2)
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
library(maptools)
library(cowplot)
library(googleway)
library(ggrepel)
library(ggspatial)
library("liblwgeom")
library("rworldmap")
library("rworldxtra")


ger <- getData(name = "GADM", country = "DEU", level = 1)
set_env()


peru0 <- getData('GADM', country=c('PER'), level=0)
peru1 <- getData('GADM', country=c('PER'), level=1)
peru2 <- getData('GADM', country=c('PER'), level=2)
peru3 <- getData('GADM', country=c('PER'), level=3)
peru4 <- getData('GADM', country=c('PER'), level=4)


# Plot shapefiles
plot(peru0)
plot(peru1)
plot(peru2)
plot(peru3)
plot(out$geometry, pch = 21, add = TRUE, bg = "lightblue", col = "black")
# get df
peru1_df <- peru1@data
peru2_df <- peru2@data
peru3_df <- peru3@data

AQP_districts <-  peru3[peru3$ID_1==4&peru3$NAME_3=="Alto Selva Alegre",]
AQP_AQP_districts <- AQP_districts[AQP_districts$ID_2==35,]
plot(AQP_AQP_districts) # OK

#Llamando los poligonos de cada inspector
insp<-read.csv("Inspectores_ASA.csv", sep = ";")
#AQP_LOCALIDADES<-read.csv("poligonos_localidadesAQP_03jun2015.csv", sep = ";")
ASA_LOC<-fread("ASA_Distrito.csv")
ASA_LOC<- ASA_LOC[!is.na(ASA_LOC$long),]
centroid<-aggregate(cbind(long,lat) ~ ident, data=insp, FUN=mean)
plot.new()
ggplot() + 
  geom_polygon(data=ASA_LOC ,aes(x=ASA_LOC$long, y=ASA_LOC$lat, group=ident), size=0.2, fill=NA, color="black")

geom_polygon(data = insp,fill="white",col="black",mapping = aes(x=long, y=lat)) + 
  geom_text(data = centroid, mapping = aes(x=long, y=lat, label=ident)) 


#########################################################
################
# this is to create grids in a shapefile
shp <- getData( country = 'aut', level = 0)
shp <- st_as_sf(shp)

grid <- shp %>% 
  st_make_grid(cellsize = 0.1, what = "centers") %>% # grid of points
  st_intersection(shp) 
ggplot() + 
   geom_sf(data = shp) + 
   geom_sf(data = grid)
shp <- getData(country = "FRA", level = 0)
shp <- spTransform(shp, CRSobj = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(shp)

shp <- spTransform(SST_start1, CRSobj = "+proj=utm +zone=51 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(shp)
cs <- c(3.28084, 3.28084)*6000
grdpts <- makegrid(shp, cellsize = cs)
spgrd <- SpatialPoints(grdpts, proj4string = CRS(proj4string(shp)))
spgrdWithin <- SpatialPixels(spgrd[shp,])
plot(spgrdWithin, add = T)
#############
grid <- SST_start1 %>% 
  st_make_grid(cellsize = 0.1, what = "centers") %>% # grid of points
  st_intersection(SST_start1) 
ggplot() + 
  geom_sf(data = SST_start1) + 
  geom_sf(data = grid)

library(raster)
setwd('~/Desktop/')
SST_start1 = readOGR("miraflores_localities_to_divide.kml") 
SST_start1 <- st_as_sf(SST_start1
shp <- getData(country = "FRA", level = 0)

SST_start1 <- spTransform(SST_start1, CRSobj = "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(SST_start1)

cs <- c(3.28084, 3.28084)*6000
grdpts <- makegrid(SST_start1, cellsize = cs)

spgrd <- SpatialPoints(grdpts, proj4string = CRS(proj4string(SST_start1)))

spgrdWithin <- SpatialPixels(spgrd[SST_start1,])
plot(spgrd, add = T)
plot(spgrd)
N=7
chop <- function(xy,N){
  Npts = nrow(xy)
  Nyh = round(sqrt(Npts/N))
  Nxh = Nyh
  yp = seq(0,1,len=Nyh+1)
  ybreaks = quantile(xy$y,yp)
  ybins = cut(xy$y, ybreaks, include.lowest=TRUE, labels=FALSE)
  xy$ybin = ybins
  xy$ymin = ybreaks[ybins]
  xy$ymax = ybreaks[ybins+1]
  for(ybin in ybins){
    inbin = ybins==ybin
    xyb = xy[inbin,"x"]
    xp = seq(0,1,len=Nxh+1)
    xbreaks = quantile(xyb,xp)
    xbins = cut(xyb, xbreaks, include.lowest=TRUE, labels=FALSE)
    xy$xmin[inbin]=xbreaks[xbins]
    xy$xmax[inbin]=xbreaks[xbins+1]
    xy$xbin[inbin] = xbins
  }
  xy$bin = paste0(xy$xbin,",",xy$ybin)
  xy
}

# helper to get cell coordinates
cells <- function(chopped){
  xy = chopped[!duplicated(chopped$bin),c("xmin","ymin","xmax","ymax")]
  xy
}
 set.seed(123)
xy = data.frame(x=runif(123, 100,200), y=runif(123,200,400))

plot(xy$x, xy$y, asp=1)
grid = chop(xy, 10)
head(grid)

table(grid$bin)
p = cells(grid)

rect(p$xmin,p$ymin,p$xmax,p$ymax)

####################################################################
# dibujar mapas conggplot

plot(peru1)


theme_set(theme_bw())


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world) +
  geom_sf()
ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))

ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")
# Ploteando PERU
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-83.14, -65.12), ylim = c(1.42, -20.42), expand = FALSE)

# SETING ARROW AND SCALE
ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-83.14, -65.12), ylim = c(1.42, -20.42),expand = FALSE)

### poniendo nombres

world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

ggplot(data = world) +
  geom_sf() +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", 
           fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(-83.14, -65.12), ylim = c(1.42, -20.42),expand = FALSE)

ggplot(data = world) + 
geom_sf(fill= "antiquewhite") +
geom_text(data= world_points,aes(x=X, y=Y, label=name), 
color = "darkblue", fontface = "bold", check_overlap = FALSE) + 
annotate(geom = "text", x = -90, y = 26, label = "Peru", 
fontface = "italic", color = "grey22", size = 6) + 
annotation_scale(location = "bl", width_hint = 0.5) + 
annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.75, "in"),
pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
  coord_sf(xlim = c(-83.14, -65.12), ylim = c(1.42, -20.42),expand = FALSE) +
xlab("Longitude") + ylab("Latitude") + 
ggtitle("Peru") + theme(panel.grid.major = element_line(color = gray(.5),
linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))

##GUARDANDO
ggsave("map.pdf")
ggsave("map_web.png", width = 6, height = 6, dpi = "screen")


world <- getMap(resolution = "high")
class(world)
world <- st_as_sf(world)
peru<-st_as_sf(peru1)

# DIVIDIENDO LAS VIVIENDAS POR ZONAS
# Periurbano
houses_p<-houses1[houses1$TYPE_L=="periurban",] #212

# Urbano
houses_u<-houses1[houses1$TYPE_L=="urban",] #673

ggplot(data = world) +
  geom_sf() +
  geom_point(data = houses_u, aes(x = LONGITUDE, y = LATITUDE), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-83.14, -65.12), ylim = c(1.42, -20.42), expand = FALSE)

houses_u<-houses_u[!is.na(houses_u$LATITUDE),]
(sites <- st_as_sf(houses_u, coords = c("LONGITUDE", "LATITUDE"), 
                   crs = 4326, agr = "constant"))
ggplot(data = world) +
  geom_sf() +
  # geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-83.14, -65.12), ylim = c(1.42, -20.42), expand = FALSE)
  coord_sf(xlim = c(-70.59, -70,29), ylim = c(-16.10, -16.16), expand = FALSE)

# First, simplify REGION for the legend:
  
  levels(world$REGION)[7] <- "South America"

# Prepare the subplots, #1 world map:

  (gworld <- ggplot(data = world) +
      geom_sf(aes(fill = REGION)) +
      geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
                fill = NA, colour = "black", size = 1.5) +
      scale_fill_viridis_d(option = "plasma") +
      theme(panel.background = element_rect(fill = "azure"),
            panel.border = element_rect(fill = NA)))
   #2 Gulf map 
  (ggulf <- ggplot(data = world) +
      geom_sf(aes(fill = REGION)) +
      annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", 
               fontface = "italic", color = "grey22", size = 6) +
      coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) +
      scale_fill_viridis_d(option = "plasma") +
      theme(legend.position = "none", axis.title.x = element_blank(), 
            axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
            panel.border = element_rect(fill = NA)))
  









