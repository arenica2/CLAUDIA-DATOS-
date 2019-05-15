### January##
gps<- fread('~/PETM-shiny/autoModel/model/input/AQP_GPS_GOOGLE_EARTH_PUNTOS_05_jun_2017.csv')

library(data.table)
plot(data)





library(sf)
poligon<-fread('~/CLAUDIA-DATOS-/claudia codigos r/Manzanas _Arequipa/Miraflores/Miraflores_mz.csv')
poligon<-poligon[!is.na(lat)]
poly_sf<-st_polygon(x =poligon )
grid<-st_make_grid(poly_sf, cellsize = .1)
plot(poly_sf, col="red")
plot(grid, add=T)
library(raster)
x <- extent(c(40.97453103, 41.06321504, -92.47427103, -92.36617044))
plot(x)
x <- cbind(c(40.97453103, 41.06321504, 41.06321504, 40.97453103, 40.97453103),
           c(-92.47427103, -92.47427103, -92.36617044, -92.36617044, -92.47427103))

x <- st_sf(st_sfc(st_polygon(list(x))))

grid <- st_make_grid(x, cellsize = c(0.01,0.01))

par(mar = c(1,1,1,1))
plot(x)
plot(grid, add = T)

poly_sf<-st_polygon(list(rbind(c(0,0), c(0.5, 0.2),c(1,0), c(1,1), c(0,0))))
grid<-st_make_grid(poly_sf, cellsize = .1, square = TRUE)
plot(poly_sf, col="red")
plot(grid, add=T)

poligon<-fread('~/Downloads//')
poligon<-poligon[!is.na(lat)]
poligon<-poligon[,c('lat','long')]
poligon<-as.matrix(poligon)
poligon<-poligon[,LISTA:=list(c(long,lat),1)]

poligon<-st_sf(st_sfc(st_polygon(list(poligon))))
poly_sf_1<-st_sf(st_sfc(st_polygon(list(poligon))))



install.packages("RQGIS")
library("raster")
library("rgdal")
library("RQGIS")
ger <- getData(name = "GADM", country = "DEU", level = 1)
set_env()

find_algorithms(search_term = "([Pp]olygon)(centroid)")
get_usage(alg = "qgis:polygoncentroids")
params <- get_args_man(alg = "qgis:polygoncentroids")
params$INPUT_LAYER  <- poligon
params$OUTPUT_LAYER <- file.path('~/CLAUDIA-DATOS-/claudia codigos r/PRUEBAS/', "arequipa_coords.shp")
out <- run_qgis(alg = "qgis:polygoncentroids",
                params = params,
                load_output = TRUE)
$OUTPUT_LAYER
[1] "/Users/jennip/CLAUDIA-DATOS-/claudia codigos r/PRUEBAS//PERU_coords.shp"






peru0 <- getData('GADM', country=c('PER'), level=0)
peru1 <- getData('GADM', country=c('PER'), level=1)
peru2 <- getData('GADM', country=c('PER'), level=2)
peru3 <- getData('GADM', country=c('PER'), level=3)
peru4 <- getData('GADM', country=c('PER'), level=4,)



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





library(data.table)


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

### read shapefile


setwd('~/Desktop//')
library("rgdal")
shp <- readOGR("miraflores_localities_to_divide.kml")
plot(shp)
proj4string(shp)
poi <- data.frame(x=c(919500, 959500, 1019500, 1049500, 1029500, 989500),
                  y=c(130600, 150600, 180600, 198000, 248000, 218000),
                  id="A", stringsAsFactors=F)

coordinates(poligon) <- ~ long + lat

proj4string(poligon) <- proj4string(shp)

bb <- bbox(shp)
cs <- c(3.28084, 3.28084)*6000
cc <- bb[, 1] + (cs/2)
cd <- ceiling(diff(t(bb))/cs)
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
grd

sp_grd <- SpatialGridDataFrame(grd,
                               data=data.frame(id=1:prod(cd)),
                               proj4string=CRS(proj4string(shp)))
summary(sp_grd)
over(poligon, sp_grd)
library("lattice")
spplot(sp_grd, "id",
       panel = function(...) {
         panel.gridplot(..., border="black")
         sp.polygons(shp)
         sp.points(poligon, cex=1.5)
         panel.text(...)
       })


plot(sp_grd)

plot(shp)

################
library(sf)
library(raster)
library(ggplot2)
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

setwd('~/Desktop/')
SST_start1 = readOGR("miraflores_localities_to_divide.kml") 
SST_start1 <- st_as_sf(SST_start1)

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

data(mtcars)
mtcars = apply_labels(mtcars,
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      hp = "Gross horsepower",
                      drat = "Rear axle ratio",
                      wt = "Weight (1000 lbs)",
                      qsec = "1/4 mile time",
                      vs = "Engine",
                      vs = c("V-engine" = 0,
                             "Straight engine" = 1),
                      am = "Transmission",
                      am = c("Automatic" = 0,
                             "Manual"=1),
                      gear = "Number of forward gears",
                      carb = "Number of carburetors"
)
cro(mtcars$am, mtcars$vs)

cro_cpct(mtcars$cyl, list(total(), mtcars$am, mtcars$vs))



data<-fread("~/PETM-shiny/BackUp_App_Inspections/APP_INSPECTIONS_feb13_2019.csv")
setnames(data,old = 'UNI_CODE',new = 'UNICODE')
data$USER_NAME <- toupper(data$USER_NAME)
data<-data[STATUS_INSPECCION == "inspeccion",STATUS_INSPECCION:="I"]
data<-data[STATUS_INSPECCION == "entrevista",STATUS_INSPECCION:="E"]
data<-data[TEST_DATA==0]
data<-data[FECHA >='2019-02-11']
head(data,3)
####Changing weird Color for the color  predicted from the APP###
data<-data[PREDICTED_COLOR=="#E31A1C",PREDICTED_COLOR:="#BD0026"]

#



table(data$USER_NAME)
JAP LLA MAR MIA MPC OCA RQA ZAC 
45  44  49  52  49  45  38  39 
JAP<-data[USER_NAME=='JAP']
LLA<-data[USER_NAME=='LLA']
MAR<-data[USER_NAME=='MAR']
MIA<-data[USER_NAME=='MIA']
MPC<-data[USER_NAME=='MPC']
OCA<-data[USER_NAME=='OCA']
RQA<-data[USER_NAME=='RQA']
ZAC<-data[USER_NAME=='ZAC']

status_visitas(JAP)
GROUP_NAME USER_NAME STATUS_INSPECCION NUMBER_HOUSES TOTAL_VISITS_BY_ARM PERCENTAGE
1  SIN_GRUPO       JAP                 C            23                  45       51.1
2  SIN_GRUPO       JAP                 I            12                  45       26.7
3  SIN_GRUPO       JAP                 R             6                  45       13.3
4  SIN_GRUPO       JAP                 V             4                  45        8.9
status_visitas(LLA)
GROUP_NAME USER_NAME STATUS_INSPECCION NUMBER_HOUSES TOTAL_VISITS_BY_ARM PERCENTAGE
1  SIN_GRUPO       LLA                 C            16                  44       36.4
2  SIN_GRUPO       LLA                 I            15                  44       34.1
3  SIN_GRUPO       LLA                 R             3                  44        6.8
4  SIN_GRUPO       LLA                 V            10                  44       22.7
status_visitas(MAR)
GROUP_NAME USER_NAME STATUS_INSPECCION NUMBER_HOUSES TOTAL_VISITS_BY_ARM PERCENTAGE
1  SIN_GRUPO       MAR                 C            26                  49         53
2  SIN_GRUPO       MAR                 I            16                  49         33
3  SIN_GRUPO       MAR                 R             7                  49         14
status_visitas(MIA)
GROUP_NAME USER_NAME STATUS_INSPECCION NUMBER_HOUSES TOTAL_VISITS_BY_ARM PERCENTAGE
1  SIN_GRUPO       MIA                 C            25                  52         48
2  SIN_GRUPO       MIA                 I            17                  52         33
3  SIN_GRUPO       MIA                 V            10                  52         19
status_visitas(OCA)
GROUP_NAME USER_NAME STATUS_INSPECCION NUMBER_HOUSES TOTAL_VISITS_BY_ARM PERCENTAGE
1  SIN_GRUPO       OCA                 C            15                  45         33
2  SIN_GRUPO       OCA                 I            30                  45         67
status_visitas(RQA)
GROUP_NAME USER_NAME STATUS_INSPECCION NUMBER_HOUSES TOTAL_VISITS_BY_ARM PERCENTAGE
1  SIN_GRUPO       RQA                 C            20                  38       52.6
2  SIN_GRUPO       RQA                 I            17                  38       44.7
3  SIN_GRUPO       RQA                 V             1                  38        2.6
status_visitas(ZAC)
GROUP_NAME USER_NAME STATUS_INSPECCION NUMBER_HOUSES TOTAL_VISITS_BY_ARM PERCENTAGE
1  SIN_GRUPO       ZAC                 C            20                  39       51.3
2  SIN_GRUPO       ZAC                 I            17                  39       43.6
3  SIN_GRUPO       ZAC                 R             2                  39        5.1
status_visitas(MPC) 
GROUP_NAME USER_NAME STATUS_INSPECCION NUMBER_HOUSES TOTAL_VISITS_BY_ARM PERCENTAGE
1  SIN_GRUPO       MPC                 C            16                  49         33
2  SIN_GRUPO       MPC                 I            17                  49         35
3  SIN_GRUPO       MPC                 R            15                  49         31
4  SIN_GRUPO       MPC                 V             1                  49          2


p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
summary(p)

with(p, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")

summary(m1 <- glm(triangulation.n ~ arm + inspector, family="poisson", data=triangles))
plot(triangulation.n ~ inspector, 
     data=triangles, 
     pch=16,
     xlab = "Calories", 
     ylab = "Sodium")

abline(model,
       col = "blue",
       lwd = 2)
  
######### getting data for barstack and productivity figures##########  
######=============NUMBER OF RISK HOUSES BY INSPECTOR=============####
#=====================================================================

#======FUNCIONES TO GET RISK HOUSES AND OUTCOME VISITS==============####

numero_riesgo<-function(data){ 
  
  #Esta funcion retorna una base de datos agregada por color de riesgo y visitas
  
  #anadiendo un campo para contar  
  
  #agregando por color de riesgo
  
  
  data_1<-aggregate(COUNT~GROUP_NAME,data =data,FUN = sum)
  data<-merge(data,data_1,by="GROUP_NAME",all.x=TRUE)
  
  setnames(data,"COUNT.x","NUMBER_HOUSES")
  setnames(data,"COUNT.y","TOTAL_VISITS_BY_ARM")
  
  data$PERCENTAGE<- (data$NUMBER_HOUSES/data$TOTAL_VISITS_BY_ARM)*100
  #options(digits=2)
  return(data)
  
}

#######=============status of visits by inspector=============####
#==============================================================####
status_visitas<-function(data){ 
  
  #Esta funcion retorna una base de datos agregada por EL STATUS DE LAS VISISTAS
  
  #anadiendo un campo para contar  
  data$COUNT<-unlist(1)
  
  #agregando por STATUS DE LA VISITA Y POR ARM
  data<-aggregate(COUNT~USER_NAME+STATUS_INSPECCION+GROUP_NAME,data = data,FUN = sum)
  
  data_2<-aggregate(COUNT~GROUP_NAME,data =data,FUN = sum)
  data<-merge(data,data_2,by="GROUP_NAME",all.x=TRUE)
  
  setnames(data,"COUNT.x","NUMBER_HOUSES")
  setnames(data,"COUNT.y","TOTAL_VISITS_BY_ARM")
  
  data$PERCENTAGE<- (data$NUMBER_HOUSES/data$TOTAL_VISITS_BY_ARM)*100
  options(digits=2)
  return(data)
  
}
#=============OBTAINING ROUGH DATA FOR EACH INSPECTOR BY ARM=============###
##========================================================================##

V7_RISK<-numero_riesgo(CLM)
V7_STATUS<-status_visitas(CLM)

V6_RISK<-numero_riesgo(MAR)
V6_STATUS<-status_visitas(MAR)

V8_RISK<-numero_riesgo(MLS)
V8_STATUS<-status_visitas(MLS)

V9_RISK<-numero_riesgo(RQA)
V9_STATUS<-status_visitas(RQA)

V13_RISK<-numero_riesgo(MIA)
V13_STATUS<-status_visitas(MIA)

V5_RISK<-numero_riesgo(MPC)
V5_STATUS<-status_visitas(MPC)


##===========PUTTING ALL IN A CSV BY VISITS AND BY RISK COLOR===========###
#========================================================================##

#binding all the tables by risk
data_risk<-do.call(rbind,list(V7_RISK,V6_RISK,V8_RISK,V9_RISK,V13_RISK,V5_RISK))
data_risk<-as.data.table(data_risk)

setwd('~/PETM-shiny/autoModel/')
write.csv(data_risk,'~/PETM-shiny/autoModel/experiments/stats_and_results/SocabayaI/data/DATA_RISK_BY_INSPECTOR_BY_ARM.csv',row.names = FALSE)


#binding all tehh tables by visists
data_status<-do.call(rbind,list(V7_STATUS,V6_STATUS,V8_STATUS,V9_STATUS,V13_STATUS,V5_STATUS))
data_status<-as.data.table(data_status)
write.csv(data_status,'~/PETM-shiny/autoModel/experiments/stats_and_results/SocabayaI/data/DATA_STATUS_BY_INSPECTOR_BY_ARM.csv',row.names = FALSE)


data_inspections<-data_status[STATUS_INSPECCION=='I']

write.csv(data_inspections,'~/PETM-shiny/autoModel/experiments/stats_and_results/SocabayaI/data/tabla_inspecciones.csv',row.names = FALSE)

####============== working tables for results===========#####
#############################################################

data_risk = apply_labels(data_risk,
                         GROUP_NAME = "Arms",
                         PREDICTED_COLOR = "Risk",
                         USER_NAME = "Participant",
                         NUMBER_HOUSES = "Number of visited houses per Risk level",
                         TOTAL_VISITS_BY_ARM = "Total Visits per Arm",
                         PERCENTAGE = "Percentage"
                         
)

cro(data_risk$GROUP_NAME, data_risk$PREDICTED_COLOR)

cro(data_risk$USER_NAME, list(total()))

cro_cpct(mtcars$cyl, list(total(), mtcars$am, mtcars$vs))


cro(data_risk$USER_NAME,data_risk$NUMBER_HOUSES,data_risk$GROUP_NAME)










#============OBTAINING NUMBER OF HOUSES IN THE BIG TRIANGLES================####
#==============================================================================#
##=============THIS HAS TO BE DONE WITH THE MODEL RESULTS CSV==================#
#INSPECTOR A
# Soc_Z13/D/CLM
# Soc_Z18/C/CLM
# Soc_Z3/A/CLM
# Soc_Z14/B/CLM

#ARM A
setwd("~/PETM-shiny/MODEL_RESULTS/")
INSP_A_ARM_A<-fread('MODEL_RESULTS_Soc_Z3_2018-04-06_13.53.01.csv')
INSP_A_ARM_A_INSP<-findInspecciones(INSP_A_ARM_A)
atriangle_a<-displayNumber_houses_uninpsected(INSP_A_ARM_A, INSP_A_ARM_A_INSP)
# max(df$nHouseUninspect)
# 1                      23
#ARM B
INSP_A_ARM_B<-fread('MODEL_RESULTS_Soc_Z14_2018-04-13_15.03.52.csv')
INSP_A_ARM_B_INSP<-findInspecciones(INSP_A_ARM_B)
atriangle_b<-displayNumber_houses_uninpsected(INSP_A_ARM_B, INSP_A_ARM_B_INSP)
# max(df$nHouseUninspect)
# 1                      13

#ARM C
INSP_A_ARM_C<-fread('MODEL_RESULTS_Soc_Z18_2018-03-28_15.07.21.csv')
INSP_A_ARM_C_INSP<-findInspecciones(INSP_A_ARM_C)
atriangle_c<-displayNumber_houses_uninpsected(INSP_A_ARM_C, INSP_A_ARM_C_INSP)
# max(df$nHouseUninspect)
# 1                      45

#ARM D
INSP_A_ARM_D<-fread('MODEL_RESULTS_Soc_z13_2018-03-28_17.12.59.csv')
INSP_A_ARM_D_INSP<-findInspecciones(INSP_A_ARM_D)
atriangle_d<-displayNumber_houses_uninpsected(INSP_A_ARM_D, INSP_A_ARM_D_INSP)
# max(df$nHouseUninspect)
# 1                      83

#getting the table with triangles and percentages of triangles for each zone 
houses_triangleA<- do.call(cbind,list(atriangle_a,atriangle_b,atriangle_c,atriangle_d))
houses_triangleA$USER_NAME<-unlist('A')
houses_triangleA<-as.data.table(houses_triangleA)

#INSPECTOR B
# Soc_Z4/D/MAR
# Soc_Z24/C/MAR
# Soc_Z1/B/MAR
# Soc_Z12/A/MAR
#ARM A
INSP_B_ARM_A<-fread('MODEL_RESULTS_Soc_Z12_2018-04-13_15.14.20.csv')
INSP_B_ARM_A_INSP<-findInspecciones(INSP_B_ARM_A)
btriangle_a<-displayNumber_houses_uninpsected(INSP_B_ARM_A, INSP_B_ARM_A_INSP)
#max(df$nHouseUninspect)
#1                       9
#ARM B
INSP_B_ARM_B<-fread('MODEL_RESULTS_Soc_Z1_2018-04-06_13.37.08.csv')
INSP_B_ARM_B_INSP<-findInspecciones(INSP_B_ARM_B)
btriangle_b<-displayNumber_houses_uninpsected(INSP_B_ARM_B, INSP_B_ARM_B_INSP)
#   max(df$nHouseUninspect)
# 1                      24
#ARM C
INSP_B_ARM_C<-fread('MODEL_RESULTS_Soc_Z24_2018-03-28_15.14.04.csv')
INSP_B_ARM_C_INSP<-findInspecciones(INSP_B_ARM_C)
btriangle_c<-displayNumber_houses_uninpsected(INSP_B_ARM_C, INSP_B_ARM_C_INSP)
#max(df$nHouseUninspect)
#1                      67
#ARM D
INSP_B_ARM_D<-fread('MODEL_RESULTS_Soc_Z4_2018-03-20_12.59.07.csv')
INSP_B_ARM_D_INSP<-findInspecciones(INSP_B_ARM_D)
btriangle_d<-displayNumber_houses_uninpsected(INSP_B_ARM_D, INSP_B_ARM_D_INSP)
#max(df$nHouseUninspect)
#1                      35
houses_triangleB<- do.call(cbind,list(btriangle_a,btriangle_b,btriangle_c,btriangle_d))
houses_triangleB$USER_NAME<-unlist('B')
houses_triangleB<-as.data.table(houses_triangleB)

#INSPECTOR C
# Soc_Z5/A/RQA
# Soc_Z17/D/RQA
# Soc_Z10/C/RQA
# Soc_Z7/B/RQA
#ARM A
setwd("~/PETM-shiny/MODEL_RESULTS/")
INSP_C_ARM_A<-fread('MODEL_RESULTS_Soc_Z5_2018-03-20_13.02.17.csv')
INSP_C_ARM_A_INSP<-findInspecciones(INSP_C_ARM_A)
ctriangle_a<-displayNumber_houses_uninpsected(INSP_C_ARM_A, INSP_C_ARM_A_INSP)
# max(df$nHouseUninspect)
# 1                      15
#ARM B
INSP_C_ARM_B<-fread('MODEL_RESULTS_Soc_Z7_2018-04-16_13.48.20.csv')
INSP_C_ARM_B_INSP<-findInspecciones(INSP_C_ARM_B)
ctriangle_b<-displayNumber_houses_uninpsected(INSP_C_ARM_B, INSP_C_ARM_B_INSP)
# max(df$nHouseUninspect)
# 1                       8
#ARM C
INSP_C_ARM_C<-fread('MODEL_RESULTS_Soc_Z10_2018-04-09_14.17.21.csv')
INSP_C_ARM_C_INSP<-findInspecciones(INSP_C_ARM_C)
ctriangle_c<-displayNumber_houses_uninpsected(INSP_C_ARM_C, INSP_C_ARM_C_INSP)
# max(df$nHouseUninspect)
# 1                      43
#ARM D
INSP_C_ARM_D<-fread('MODEL_RESULTS_Soc_Z17_2018-04-02_14.20.09.csv')
INSP_C_ARM_D_INSP<-findInspecciones(INSP_C_ARM_D)
ctriangle_d<-displayNumber_houses_uninpsected(INSP_C_ARM_D, INSP_C_ARM_D_INSP)
# max(df$nHouseUninspect)
# 1                      50

houses_triangleC<- do.call(cbind,list(ctriangle_a,ctriangle_b,ctriangle_c,ctriangle_d))
houses_triangleC$USER_NAME<-unlist('C')
houses_triangleC<-as.data.table(houses_triangleC)


#INSPECTOR D
# Soc_Z8/A/MIA
# Soc_Z9/B/MIA
# Soc_Z23/C/MIA
# Soc_Z22/D/MIA
#ARM A
INSP_D_ARM_A<-fread('MODEL_RESULTS_Soc_Z8_2018-03-21_14.09.13.csv')
INSP_D_ARM_A_INSP<-findInspecciones(INSP_D_ARM_A)
dtriangle_a<-displayNumber_houses_uninpsected(INSP_D_ARM_A, INSP_D_ARM_A_INSP)
# max(df$nHouseUninspect)
# 1                      22
#ARM B
INSP_D_ARM_B<-fread('MODEL_RESULTS_Soc_Z9_2018-04-02_14.19.01.csv')
INSP_D_ARM_B_INSP<-findInspecciones(INSP_D_ARM_B)
dtriangle_b<-displayNumber_houses_uninpsected(INSP_D_ARM_B, INSP_D_ARM_B_INSP)
# max(df$nHouseUninspect)
# 1                      16
#ARM C
INSP_D_ARM_C<-fread('MODEL_RESULTS_Soc_Z23_2018-04-09_15.42.45.csv')
INSP_D_ARM_C_INSP<-findInspecciones(INSP_D_ARM_C)
dtriangle_c<-displayNumber_houses_uninpsected(INSP_D_ARM_C, INSP_D_ARM_C_INSP)
# max(df$nHouseUninspect)
# 1                      34
#ARM D
INSP_D_ARM_D<-fread('MODEL_RESULTS_Soc_Z22_2018-04-16_13.56.16.csv')
INSP_D_ARM_D_INSP<-findInspecciones(INSP_D_ARM_D)
dtriangle_d<-displayNumber_houses_uninpsected(INSP_D_ARM_D, INSP_D_ARM_D_INSP)
# max(df$nHouseUninspect)
# 1                      48
houses_triangleD<- do.call(cbind,list(dtriangle_a,dtriangle_b,dtriangle_c,dtriangle_d))
houses_triangleD$USER_NAME<-unlist('D')
houses_triangleD<-as.data.table(houses_triangleD)

#INSPECTOR E
# Soc_Z19/D/MLS
# Soc_Z15/C/MLS
# Soc_Z11/A/MLS
# Soc_Z20/B/MLS
#ARM A
INSP_E_ARM_A<-fread('MODEL_RESULTS_Soc_Z11_2018-04-06_14.57.08.csv')
INSP_E_ARM_A_INSP<-findInspecciones(INSP_E_ARM_A)
etriangle_a<-displayNumber_houses_uninpsected(INSP_E_ARM_A, INSP_E_ARM_A_INSP)
#max(df$nHouseUninspect)
#1                       9
#ARM B
INSP_E_ARM_B<-fread('MODEL_RESULTS_Soc_Z20_2018-04-13_15.21.26.csv')
INSP_E_ARM_B_INSP<-findInspecciones(INSP_E_ARM_B)
etriangle_b<-displayNumber_houses_uninpsected(INSP_E_ARM_B, INSP_E_ARM_B_INSP)
# max(df$nHouseUninspect)
# 1                      12
#ARM C
INSP_E_ARM_C<-fread('MODEL_RESULTS_Soc_Z15_2018-03-28_15.19.09.csv')
INSP_E_ARM_C_INSP<-findInspecciones(INSP_E_ARM_C)
etriangle_c<-displayNumber_houses_uninpsected(INSP_E_ARM_C, INSP_E_ARM_C_INSP)
# max(df$nHouseUninspect)
# 1                      51
#ARM D
INSP_E_ARM_D<-fread('MODEL_RESULTS_Soc_Z19_2018-03-20_13.11.38.csv')
INSP_E_ARM_D_INSP<-findInspecciones(INSP_E_ARM_D)
etriangle_d<-displayNumber_houses_uninpsected(INSP_E_ARM_D, INSP_E_ARM_D_INSP)
# max(df$nHouseUninspect)
# 1                      42
houses_triangleE<- do.call(cbind,list(etriangle_a,etriangle_b,etriangle_c,etriangle_d))
houses_triangleE$USER_NAME<-unlist('E')
houses_triangleE<-as.data.table(houses_triangleE)


#INSPECTOR F
# Soc_Z16 /A/MPC
# Soc_Z21/D/MPC
# Soc_Z2/B/MPC
# Soc_Z6/C/MPC
#ARM A
INSP_F_ARM_A<-fread('MODEL_RESULTS_Soc_Z16_2018-03-20_13.20.00.csv')
INSP_F_ARM_A_INSP<-findInspecciones(INSP_F_ARM_A)
ftriangle_a<-displayNumber_houses_uninpsected(INSP_F_ARM_A, INSP_F_ARM_A_INSP)
# max(df$nHouseUninspect)
# 1                      26
#ARM B
INSP_F_ARM_B<-fread('MODEL_RESULTS_Soc_Z2_2018-04-09_14.58.00.csv')
INSP_F_ARM_B_INSP<-findInspecciones(INSP_F_ARM_B)
ftriangle_b<-displayNumber_houses_uninpsected(INSP_F_ARM_B, INSP_F_ARM_B_INSP)
# max(df$nHouseUninspect)
# 1                      16
#ARM C
INSP_F_ARM_C<-fread('MODEL_RESULTS_Soc_Z6_2018-04-16_13.50.47.csv')
INSP_F_ARM_C_INSP<-findInspecciones(INSP_F_ARM_C)
ftriangle_c<-displayNumber_houses_uninpsected(INSP_F_ARM_C, INSP_F_ARM_C_INSP)
# max(df$nHouseUninspect)
# 1                      70
#ARM D
INSP_F_ARM_D<-fread('MODEL_RESULTS_Soc_Z21_2018-04-02_14.37.59.csv')
INSP_F_ARM_D_INSP<-findInspecciones(INSP_F_ARM_D)
ftriangle_d<-displayNumber_houses_uninpsected(INSP_F_ARM_D, INSP_F_ARM_D_INSP)
# max(df$nHouseUninspect)
# 1                      49
houses_triangleF<- do.call(cbind,list(ftriangle_a,ftriangle_b,ftriangle_c,ftriangle_d))
houses_triangleF$USER_NAME<-unlist('F')
houses_triangleF<-as.data.table(houses_triangleF)
# # juntando todas las tablas de los trianguklos por brazo 
# triangles_data<-do.call(rbind,list(houses_triangleA,houses_triangleB,houses_triangleC,houses_triangleD,houses_triangleE,houses_triangleF))
# cols <- names(triangles_data) == "max(df$nHouseUninspect)"
# #names(triangles_data)[cols] <- paste('ARM',c('A','B','C','D'),sep = '')
# names(triangles_data)[cols]<-c("App Risk Tri Inc 4:4",'App Risk Tri Inc 6:2','No Inc','Inc per Bug')
# triangles_data<-triangles_data%>%dplyr::select(USER_NAME,`App Risk Tri Inc 4:4`,`App Risk Tri Inc 6:2`,`No Inc`,`Inc per Bug`)

## GOAL FOR THIS WEEK , SHOW TO THE PEOPLE OUR RESULTS FROM THE EXPRIMENTS AND TRY TO DO SOME ANNALISYS 
## ABOUT SOCABAYA ,Paucarpata and Cayma 
## show maps,show the 

library(MASS)
######=============NUMBER OF RISK HOUSES BY INSPECTOR=============####
#=====================================================================

#======FUNCIONES TO GET RISK HOUSES AND OUTCOME VISITS==============####

numero_riesgo<-function(data){ 
  
  #Esta funcion retorna una base de datos agregada por color de riesgo y visitas
  
  #anadiendo un campo para contar  
  data$COUNT<-unlist(1)
  
  #agregando por color de riesgo
  data<-aggregate(COUNT ~ PREDICTED_COLOR+USER_NAME+GROUP_NAME, data=data, FUN=sum)
  
  data_1<-aggregate(COUNT~GROUP_NAME,data =data,FUN = sum)
  data<-merge(data,data_1,by="GROUP_NAME",all.x=TRUE)
  
  setnames(data,"COUNT.x","NUMBER_HOUSES")
  setnames(data,"COUNT.y","TOTAL_VISITS_BY_ARM")
  
  data$PERCENTAGE<- (data$NUMBER_HOUSES/data$TOTAL_VISITS_BY_ARM)*100
  #options(digits=2)
  return(data)
  
}
V4<-numero_riesgo(LLA)
V5<-numero_riesgo(MPC)
V6<-numero_riesgo(MAR)
V7<-numero_riesgo(CLM)
V9<-numero_riesgo(RQA)
V13<-numero_riesgo(MIA)

#######=============status of visits by inspector=============####
#==============================================================####
status_visitas<-function(data){ 
  
  #Esta funcion retorna una base de datos agregada por EL STATUS DE LAS VISISTAS
  
  #anadiendo un campo para contar  
  data$COUNT<-unlist(1)
  
  #agregando por STATUS DE LA VISITA Y POR ARM
  data<-aggregate(COUNT~USER_NAME+STATUS_INSPECCION+GROUP_NAME,data = data,FUN = sum)
  
  data_2<-aggregate(COUNT~GROUP_NAME,data =data,FUN = sum)
  data<-merge(data,data_2,by="GROUP_NAME",all.x=TRUE)
  
  setnames(data,"COUNT.x","NUMBER_HOUSES")
  setnames(data,"COUNT.y","TOTAL_VISITS_BY_ARM")
  
  data$PERCENTAGE<- (data$NUMBER_HOUSES/data$TOTAL_VISITS_BY_ARM)*100
  options(digits=2)
  return(data)
  
}

V4_sta<-status_visitas(LLA)
V5_sta<-status_visitas(MPC)
V6_sta<-status_visitas(MAR)
V7_sta<-status_visitas(CLM)
V9_sta<-status_visitas(RQA)
V13_sta<-status_visitas(MIA)






#====================Reading data from back app ========================



setwd("~/PETM-shiny/autoModel/searchZones/")
fread('Z7CAY/modelResults/MODEL_RESULTS_Z7CAY_2018-05-10_14.47.33.csv')
INSP_A_ARM_A_INSP<-findInspecciones(INSP_A_ARM_A)
displayNumber_houses_uninpsected(INSP_A_ARM_A, INSP_A_ARM_A_INSP)


Z7CAY 
V4_A<-fread('Z7CAY/modelResults/MODEL_RESULTS_Z7CAY_2018-05-10_14.47.33.csv')
V4_A_INSP<-findInspecciones(V4_A)
displayNumber_houses_uninpsected(V4_A, V4_A_INSP)
# max(df$nHouseUninspect)
# 1                      44
Z2CAY V4-B
V4_B<-fread('Z2CAY/modelResults/MODEL_RESULTS_Z2CAY_2018-05-17_16.24.56.csv')
V4_B_INSP<-findInspecciones(V4_B)
displayNumber_houses_uninpsected(V4_B, V4_B_INSP)
# max(df$nHouseUninspect)
# 1                      22

Z12CAY V5-A
V5_A<-fread('Z12CAY/modelResults/MODEL_RESULTS_Z12CAY_2018-05-21_15.45.11.csv')
V5_A_INSP<-findInspecciones(V5_A)
displayNumber_houses_uninpsected(V5_A, V5_A_INSP)
# max(df$nHouseUninspect)
# 1                      12
Z9CAY V5-B
V5_B<-fread('Z9CAY/modelResults/MODEL_RESULTS_Z9CAY_2018-05-14_15.12.01.csv')
V5_B_INSP<-findInspecciones(V5_B)
displayNumber_houses_uninpsected(V5_B, V5_B_INSP)
# max(df$nHouseUninspect)
# 1                      13



Z10CAY 
V6_A<-fread('Z10CAY/modelResults/MODEL_RESULTS_Z10CAY_2018-05-10_14.49.34.csv')
V6_A_INSP<-findInspecciones(V6_A)
displayNumber_houses_uninpsected(V6_A, V6_A_INSP)
# max(df$nHouseUninspect)
# 1                      10
Z1CAY V6-B
V6_B<-fread('Z1CAY/modelResults/MODEL_RESULTS_Z1CAY_2018-05-17_16.09.53.csv')
V6_B_INSP<-findInspecciones(V6_B)
displayNumber_houses_uninpsected(V6_B, V6_B_INSP)
# max(df$nHouseUninspect)
# 1                      31

Z4CAY 
V7_A<-fread('Z4CAY/modelResults/MODEL_RESULTS_Z4CAY_2018-05-10_15.03.37.csv')
V7_A_INSP<-findInspecciones(V7_A)
displayNumber_houses_uninpsected(V7_A,V7_A_INSP)
# max(df$nHouseUninspect)
# 1                      10


Z14CAY V7-B
V7_B<-fread('Z14CAY/modelResults/MODEL_RESULTS_Z14CAY_2018-05-17_16.14.26.csv')
V7_B_INSP<-findInspecciones(V7_B)
displayNumber_houses_uninpsected(V7_B,V7_B_INSP)
# max(df$nHouseUninspect)
# 1                      11




Z6CAY 
V9_A<-fread('Z6CAY/modelResults/MODEL_RESULTS_Z6CAY_2018-05-10_14.39.24.csv')
V9_A_INSP<-findInspecciones(V9_A)
displayNumber_houses_uninpsected(V9_A,V9_A_INSP)
# max(df$nHouseUninspect)
# 1                       9
Z13CAY 
V9_B<-fread('Z13CAY/modelResults/MODEL_RESULTS_Z13CAY_2018-05-17_16.05.26.csv')
V9_B_INSP<-findInspecciones(V9_B)
displayNumber_houses_uninpsected(V9_B,V9_B_INSP)
# max(df$nHouseUninspect)
# 1                       8


Z8CAY 
V13_A<-fread('Z8CAY/modelResults/MODEL_RESULTS_Z8CAY_2018-05-17_16.19.24.csv')
V13_A_INSP<-findInspecciones(V13_A)
displayNumber_houses_uninpsected(V13_A,V13_A_INSP)
# max(df$nHouseUninspect)
# 1                      27
Z11CAY 
V13_B<-fread('Z11CAY/modelResults/MODEL_RESULTS_Z11CAY_2018-05-10_15.04.33.csv')
V13_B_INSP<-findInspecciones(V13_B)
displayNumber_houses_uninpsected(V13_B,V13_B_INSP)
# max(df$nHouseUninspect)
# 1                      22


library("ggpubr")
ggboxplot(triangles, x = "arm", y = "productivity_inspect", 
          color = "arm", palette = c("#00AFBB", "#E7B800"),
          order = c("triang_app_no_incentives", "triang_app_four_four"),
          ylab = "productivity_inspect", xlab = "arm")




