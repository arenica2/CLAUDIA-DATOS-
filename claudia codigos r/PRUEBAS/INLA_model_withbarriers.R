setwd("~/PETM-shiny/Static_Data_formodel")
#load libraries
library(INLA)
library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)



######################################
############read in NEW data#############
######################################
############work in progress#############
######################################

#all spray data 2009 - 2015
#rociado <- read.csv("~/PETM-shiny/spray_I_II_2009_2011/cons_rociado_2009_2015.csv")

#drop unwanted columns
# varstokeep <- c("UNICODE", "P_TRIAT", "I_TRIAT", "FECHA", "CICLO")
# rociado <- rociado[, varstokeep]
# 
# #all gps data (most have blocks)
# #gps <- read.csv("~/PETM-shiny/unicode_numberMzn/AREQUIPA_GPS_GOOGLE/AREQUIPA_GPS_GOOGLE.csv")
# 
# #merge gps and spray data
# rociado.gps <- merge(rociado, gps, by="UNICODE",all=TRUE)
# 
# #find which houses are missing from either dataset
# #which gps coordinates are missing
# length(which(is.na(rociado.gps$LATITUDE)))     #5865 observations with no coordinates
# missing.gps <- which(is.na(rociado.gps$LATITUDE))   
# houses.missing.gps <- rociado.gps$UNICODE[missing.gps]
# 
# #look at rociado data over all houses
# par(mfrow=c(1,1))
# par(mar=c(1,1,1,1))
# only.rociado.gps <- merge(rociado, gps, by="UNICODE",all.x=TRUE)
# plot(gps$LATITUDE, gps$LONGITUDE, pch=".")
# points(only.rociado.gps$LATITUDE, only.rociado.gps$LONGITUDE, pch=".",col="red")
# 

#####################################################################
#####################################################################
#####################################################################

######################################
###merge block indicators with data###
######################################

setwd("~/PETM-shiny/Static_Data_formodel")
#bymanz <- read.csv("merged_encuesta_2011.csv")

#find study unicodes
# pueblo <- "1.10.2."
# rosa <- "1.10.38."
# pueblo.houses <- ifelse(grepl(pueblo, bymanz$unicode,fixed=TRUE),1,0)
# rosa.houses <- ifelse(grepl(rosa, bymanz$unicode,fixed=TRUE),1,0)
# 
# #if want to include only rosa
# rosa.unicodes <- bymanz$unicode[which(rosa.houses==1)]
# 
# #including only pueblo and santa rosa
# #rosa.unicodes <- bymanz$unicode[which(pueblo.houses==1 | rosa.houses==1)]
# 
# #subset data to pueblo unicodes
# bymanz <- bymanz[which(bymanz$unicode %in% rosa.unicodes),]
# 
# #drop extra variables
# varstokeep <- c("unicode", "intra_adul", "intra_ninfas", "peri_adul", "peri_ninfas", "fecha")
# bymanz <- bymanz[,varstokeep]
# 
# #create outcome variable
# bymanz$TOTAL <- bymanz$intra_adul + bymanz$intra_ninfas + bymanz$peri_adul + bymanz$peri_ninfas
# bymanz$INSP_POSITIVA <- ifelse(bymanz$TOTAL>0, 1, 0)
# bymanz$source <- 2

#read.in <- data.frame(c(unicode X Y date ))

gpsdata <- read.csv("mmelgar_gps_rociado.csv")

gpsdata$I_TRIAT <- ifelse(is.na(gpsdata$I_TRIAT), 0, gpsdata$I_TRIAT)
gpsdata$P_TRIAT <- ifelse(is.na(gpsdata$P_TRIAT), 0, gpsdata$P_TRIAT)

#create positive column
gpsdata$POSITIVE_at_SPRAY <- NULL
POSITIVE_at_SPRAY <- ifelse(gpsdata$I_TRIAT + gpsdata$P_TRIAT > 0, 1, 0)
gpsdata <- cbind(gpsdata, POSITIVE_at_SPRAY)
pos_sprayed <- ifelse(gpsdata$POSITIVE_at_SPRAY==1 & (gpsdata$CICLO==1 | gpsdata$CICLO==2), 1, 0)
not_sprayed <- ifelse(is.na(gpsdata$CICLO), 1, 0)
gpsdata <- cbind(gpsdata, pos_sprayed, not_sprayed)

gpsdata.rosa <- gpsdata[which(gpsdata$L==38),]


dim(gpsdata.rosa)[1] #2733 houses
gpsadata.rosa <- gpsdata.rosa[,c("UNICODE", "P", "D", "L", "V","LATITUDE", "LONGITUDE", "block", "pos_sprayed", "not_sprayed")]

#merge datasets
#df <- merge(df, bymanz, by="unicode",all.x=TRUE)

plot(gpsdata.rosa$LATITUDE, gpsdata.rosa$LONGITUDE)

###########################################
###read in inspections and ministry data###
###########################################

insp <- read.csv("inspections_AQP_ALL_DISTRICTS.csv")
minsa <- read.csv("TODO LOS DISTRITOS ENCUESTAS MINSA.csv")

#rename one column so column names identical
names(minsa)[names(minsa)=="ENCUESTA_ANT"] <- "PROCEDENCIA"

#check
identical(names(insp), names(minsa))


#subset to rosa
insp <- insp[which(insp$L==38),]
minsa <-minsa[which(minsa$L==38),]

minsa$INSP_POSITIVA <- ifelse(minsa$INSP_COMPLETA==1 & is.na(minsa$INSP_POSITIVA), 0, minsa$INSP_POSITIVA)
insp$INSP_POSITIVA <- ifelse(insp$INSP_COMPLETA==1 & is.na(insp$INSP_POSITIVA), 0, insp$INSP_POSITIVA)
#combine minsa and insp
newdata <- rbind(insp, minsa)

#rename unicode variable for merge later
names(newdata)[names(newdata)=="UNICODE."] <- "UNICODE"

#create indicator of dataset for merge
newdata$source <- c(rep(1, dim(insp)[1]), rep(0, dim(minsa)[1]))


#how many unique observations
length(unique(newdata$UNICODE))

#merge new data with old data & gps information
df <- newdata
#df <- merge(bymanz, newdata, by=c("unicode", "source", "INSP_POSITIVA"),all=TRUE)

dim(df)

#add in gps data
data <- merge(gpsdata.rosa, df, by=c("UNICODE", "P", "D", "L", "V"), all=TRUE)

#remove houses with missing locations
missinghouses <- data$INSP_POSITIVA[which(is.na(data$LATITUDE))]
#NOTE: 578 missing

#for now remove the missing houses
data <- data[!is.na(data$LATITUDE),]

################################
###prepare data for analyzing###
################################

#number of positives
sum(data$INSP_POSITIVA[which(data$source==1)], na.rm=TRUE)  #586 positives
sum(data$INSP_POSITIVA[which(data$source==0)], na.rm=TRUE)  #2 positives



#keep most recent observation
#if more than one observation for a house, pick most recent

#identify unique unicodes
unicode<-as.character(data$UNICODE)
unique.unicodes <- unique(unicode)


#find repeated unicodes
repeated.unicodes <- unicode[which(duplicated(unicode) == TRUE)]
#setting an empty vector for the for loop
unique.dates <- c(1:length(unique.unicodes)*NA)

date <- function(y,m,d){
  #Convert it into one string
  right.date <- paste(y,m,d,sep = "/", collapse = NULL)
  #Read it as a date in the right format for R
  
  new.dates <- as.Date(right.date, "%Y/%m/%d")
  
  return(new.dates)
}

#add date column
data$date <- NULL
data$date <- date(data$ANIO,data$MES,data$DIA)

#add in date for 2009 inspections
#data$date <- ifelse(is.na(data$date), as.Date(data$fecha, '%Y-%m-%d'), date(data$ANIO,data$MES,data$DIA))

missingdates <- which(is.na(data$date)&!is.na(data$INSP_POSITIVA))
#length(missingdates) #4109 houses with missing dates
#data[missingdates,]
#sum(data$source[missingdates]) #all from insp data (sum=4109)

missing.insp.dates <- data$INSP_POSITIVA[which(is.na(data$date)&!is.na(data$INSP_POSITIVA))]
missing.insp.dates #none

#remove houses
#data <- data[!is.na(data$date),]

#summary of current data
table(data$INSP_POSITIVA) #4 positives, 871 negatives

#create time indicator variable for analysis
#time since last inspection
break.date <- as.Date("2017-01-05")
data$insptime <- 4
data$insptime <- ifelse(break.date-data$date<(365*1),3,data$insptime)
data$insptime <- ifelse((break.date-data$date>=(365*1)&break.date-data$date<(365*3)),2,data$insptime)
data$insptime <- ifelse((break.date-data$date>=(365*3)&break.date-data$date<(365*10)),1,data$insptime)

#add column for most recent inspection

newdata <- data[order(data$UNICODE, data$date),] 

#find most recent date for each observation
for (i in unique.unicodes){
  #if more than one observation
  temp <- newdata[which(newdata$UNICODE==i),]
  
  if(dim(temp)[1]==1){
    newdata$recentinsp[which(newdata$UNICODE==i)] <- temp$date
  }else{
    maxdate <- max(temp$date,na.rm=TRUE)
    newdata$recentinsp[which(newdata$UNICODE==i)] <- maxdate
  }
}


newdata$recentinsp <- ifelse(is.na(newdata$recentinsp), NA, as.Date(newdata$recentinsp,origin="1970-01-01"))

newdata$recentinsp <- as.Date(newdata$recentinsp,origin="1970-01-01")

data <- newdata

table(data$insptime, data$INSP_POSITIVA)



#remove unwanted columns
varstokeep <- c("UNICODE", "LONGITUDE", "LATITUDE", "block", "INSP_POSITIVA", "insptime", "recentinsp", "pos_sprayed", "not_sprayed")

data.test <- data[,varstokeep]

#remove duplicate rows
data.test <- data.test[!duplicated(data.test[,c("UNICODE", "insptime", "LONGITUDE", "LATITUDE")],fromLast=TRUE),]
table(data.test$insptime, data.test$INSP_POSITIVA)
data.test <- data.test[!duplicated(data.test),]
df <- data.test

myvars <- c("UNICODE", "LONGITUDE", "LATITUDE", "block", "recentinsp", "pos_sprayed", "not_sprayed")
temp <- df[,myvars]
temp <- temp[!duplicated(temp),]
N <- length(unique(df$UNICODE))
melgar.fourtimes <- rbind(temp,temp,temp,temp)
melgar.fourtimes$insptime <- c(rep(1, N), rep(2, N), rep(3, N), rep(4, N))
data.merge <- merge(data.test, melgar.fourtimes,by=c("UNICODE","insptime", "LATITUDE", "LONGITUDE", "block","recentinsp", "pos_sprayed", "not_sprayed"),all.y=TRUE)


#create copy column
data.merge$INSP_POSITIVA_copy <- data.merge$INSP_POSITIVA

#third time point
pos3 <- data.merge$UNICODE[which(data.merge$INSP_POSITIVA==1&data.merge$insptime==3)]

#make fourth time point for these houses postive
data.merge$INSP_POSITIVA_copy[which(data.merge$UNICODE %in% pos3&data.merge$insptime==4)] <- 1

#second time point
pos2 <- data.merge$UNICODE[which(data.merge$INSP_POSITIVA==1&data.merge$insptime==2)]

#make fourth time point for these houses postive
data.merge$INSP_POSITIVA_copy[which(data.merge$UNICODE %in% pos2&data.merge$insptime==3)] <- 1

#first time point
pos1 <- data.merge$UNICODE[which(data.merge$INSP_POSITIVA==1&data.merge$insptime==1)]

#make fourth time point for these houses postive
data.merge$INSP_POSITIVA_copy[which(data.merge$UNICODE %in% pos1&data.merge$insptime==2)] <- 1

plot(data.merge$LONGITUDE, data.merge$LATITUDE,cex=0.05)
points(data.merge$LONGITUDE[which(data.merge$insptime==1)], data.merge$LATITUDE[which(data.merge$insptime==1)], cex=data.merge$INSP_POSITIVA_copy[which(data.merge$insptime==1)],col="red",lwd=2)
points(data.merge$LONGITUDE[which(data.merge$insptime==1)], data.merge$LATITUDE[which(data.merge$insptime==1)], cex=data.merge$INSP_POSITIVA_copy[which(data.merge$insptime==2)]*0.6,col="orange",lwd=2,pch=2)
points(data.merge$LONGITUDE[which(data.merge$insptime==1)], data.merge$LATITUDE[which(data.merge$insptime==1)], cex=data.merge$INSP_POSITIVA_copy[which(data.merge$insptime==3)]*0.4,col="gold",lwd=2,pch=3)
points(data.merge$LONGITUDE[which(data.merge$insptime==1)], data.merge$LATITUDE[which(data.merge$insptime==1)], cex=data.merge$INSP_POSITIVA_copy[which(data.merge$insptime==4)]*0.3,col="forestgreen",lwd=2,pch=4)
#points(data.merge$LONGITUDE, data.merge$LATITUDE, cex=1-data.merge$INSP_POSITIVA_copy,col="blue",lwd=2,pch=4)


###############################################
#########spatial model in INLA#################
###############################################
data <- data.merge

##########################################################
##############adding streets as barriers##################
##########################################################

#################################
##create origin for every block##
#################################

N.block <- length(unique(data$block)) #573 blocks

data$originX <- NA
data$originY <- NA

for(i in unique(data$block)){
  
  data$originX[which(data$block==i)] <- median(data$LONGITUDE[which(data$block==i)])
  data$originY[which(data$block==i)] <- median(data$LATITUDE[which(data$block==i)]) 
}

plot(data$originX, data$originY,pch=18)


###################################################
##create x,y matrix for each house on block level##
###################################################

data$LONGITUDEdiff <- NA
data$LATITUDEdiff <- NA

data$LONGITUDEdiff <- data$LONGITUDE - data$originX
data$LATITUDEdiff <- data$LATITUDE - data$originY

################################
##scale x,y of origin matrix##
###############################

scale.dim <- function(S){
  
  Xscale <- data$originX*S+data$LONGITUDEdiff
  Yscale <- data$originY*S+data$LATITUDEdiff
  
  #look at block origins
  plot(Xscale-data$LONGITUDE, Yscale-data$LATITUDE,pch=18,cex=0.08)
  points(data$LONGITUDE, data$LATITUDE,pch=18,cex=0.1,col="red")
  
  
  #data scaled plotted together
  plot(Xscale/S, Yscale/S,pch=18,cex=0.2)
  points(data$LONGITUDE, data$LATITUDE,pch=18,cex=0.2,col="red")
  
  return(list(Xscale, Yscale))
  
}

#################################
###Add scale factor to map#######
#Scale set to 1.5
#################################

S <- 1.5
new.coords <- scale.dim(S)
data$LONGITUDEscale <- new.coords[[1]]*100000
data$LATITUDEscale <- new.coords[[2]]*100000

#create dataset of observations
dataforfit <- data[which(data$INSP_POSITIVA_copy==1 | data$INSP_POSITIVA_copy==0),]
#dataforfit <- dataforfit[1:5000,]
datapred <- data[which(data$insptime==4),]

#define time dimension
k <- length(unique(data$insptime))


#define coordinate matrix
coords1<- cbind(dataforfit$LONGITUDEscale, dataforfit$LATITUDEscale)
coords <- cbind(datapred$LONGITUDEscale, datapred$LATITUDEscale)

#create mesh
mesh1 <- inla.mesh.2d(coords1, max.edge=c(50*S,50*S),cutoff=0.2)
plot(mesh1)
points(data$LONGITUDEscale,data$LATITUDEscale,pch=18,cex=0.2,col="blue")
points(dataforfit$LONGITUDEscale, dataforfit$LATITUDEscale,col="red",pch=18,cex=0.2)

A.est <- inla.spde.make.A(mesh=mesh1,
                          loc=coords1,
                          group=dataforfit$insptime,n.group=k)

A.prd <- inla.spde.make.A(mesh=mesh1,loc=coords,group=datapred$insptime)

spde <- inla.spde2.matern(mesh1, alpha=2)

mesh.index <- inla.spde.make.index(name='spatial', n.spde=spde$n.spde,n.group=k)

stack.est =
  inla.stack(data=list(y=dataforfit$INSP_POSITIVA_copy),
             A=list(A.est,1),
             effects=
               list(c(mesh.index,list(Intercept=1)),
                    list(pos_sprayed=dataforfit$pos_sprayed,
                         not_sprayed=dataforfit$not_sprayed)),
             tag="est")

stack.pred =
  inla.stack(data=list(y=datapred$INSP_POSITIVA_copy),
             A=list(A.prd,1),
             effects=
               list(c(mesh.index,list(Intercept=1)),
                    list(pos_sprayed=datapred$pos_sprayed,
                         not_sprayed=datapred$not_sprayed)),
             tag="pred")


stack = inla.stack(stack.est, stack.pred)

formula <- y ~ -1 + Intercept + pos_sprayed + not_sprayed + f(spatial, model=spde, group=spatial.group, control.group=list(model='ar1'))


result <- inla(formula,
               data = inla.stack.data(stack, spde=spde),
               family = c("binomial"),verbose=TRUE,
               control.predictor = list(A=inla.stack.A(stack), compute=TRUE,link=1)
               ,control.inla = list(reordering = "metis")
               ,control.fixed = list(mean = c(-5,3.07, 1.17), prec = c(1,1,1))
               )

result.f <- inla.spde2.result(result, "spatial", spde, do.transf=TRUE)

par(mfrow=c(2,3))
par(mar=c(5,3,1,1))
plot(result$marginals.fix[[1]], type="l", xlab="Intercept",
     ylab="Density")
plot(result$marginals.hy[[1]], type="l", ylab="Density",
     xlab=expression(phi))
plot.default(result.f$marginals.variance.nominal[[1]], type="l",
             xlab=expression(sigma[x]^2), ylab="Density")
plot.default(result.f$marginals.range.nominal[[1]], type="l",
             xlab="Practical range", ylab="Density")
plot(result$marginals.hy[[2]], type="l", ylab="Density",
     xlab=names(result$marginals.hy)[2])


id.prd <- inla.stack.index(stack,"pred")$data
uncertainty <- probability <- matrix(NA, dim(coords)[1], dim(coords)[1])


probability <- result$summary.fitted.values$mean[id.prd]
uncertainty <- result$summary.fitted.values$sd[id.prd]

output <- datapred

varstokeep <- c("unicode", "X", "Y", "recentinsp")

output <- output[,varstokeep]
output <- cbind(output, probability)
output <- output[order(-output$probability),]

#names(output)[names(output)=="UNICODE."] <- "UNICODE"

#write output to read into app
write.csv(output, "Model Results Melgar.csv")

#record data results with todays date
write.csv(output, paste("Model Results ",Sys.Date(), ".csv", sep=""))

#plot results from today
pdf(file=paste("Model Results ",today, "pdf", sep=""),width=20, height=15)
output <- output[order(output$probability, decreasing=TRUE),]
colfunc <- heat.colors(length(output$probability))
layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
plot(output$LONGITUDE,output$LATITUDE,col=colfunc,cex=0.14)
points(data$LONGITUDE,data$LATITUDE,cex=data$INSP_POSITIVA_copy)

legend_image <- as.raster(matrix(colfunc, ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = paste(Sys.Date()))
text(x=1.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
rasterImage(colfunc, 0, 0, 1,1)

dev.off()


#Plot the results
# 
# 
# projgrid <- inla.mesh.projector(mesh1)
# xmean <- list()
# for (j in 1:k){
#   xmean[[j]] <- inla.mesh.project(
#     projgrid, result$summary.random$spatial$mean[which(mesh.index$spatial.group==j)])
# }
# 
# require(gridExtra)
# do.call(function(...) grid.arrange(..., nrow=4),
#         lapply(xmean, levelplot, xlab='', ylab='',
#                col.regions=topo.colors(16), scale=list(draw=FALSE)))
# 
# 
# resultstoplot <- cbind(datapred, probability, uncertainty)
# 
# 
# ggplot(resultstoplot) +
#   geom_point(aes(x=resultstoplot$X, y=resultstoplot$Y, col=resultstoplot$probability),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7))
# 
# 
# ggplot(resultstoplot) +
#   geom_point(aes(x=resultstoplot$X, y=resultstoplot$Y, col=resultstoplot$uncertainty),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7))
# 
# par(mfrow=c(1,1))
# par(mar=c(1,1,1,1))
# plot(data$LONGITUDE, data$LATITUDE, cex=0.2,xlab="",ylab="")
# points(data$LONGITUDE, data$LATITUDE, cex=(1-data$INSP_POSITIVA)*0.5,col="forestgreen")
# points(data$LONGITUDE[which(data$insptime==1)], data$LATITUDE[which(data$insptime==1)], cex=log(data$TOTAL_TCAP[which(data$insptime==1)]+1),col="red",lwd=2)
# points(data$LONGITUDE[which(data$insptime==2)], data$LATITUDE[which(data$insptime==2)], cex=log(data$TOTAL_TCAP[which(data$insptime==2)]+1),col="gold",lwd=2)
# points(data$LONGITUDE[which(data$insptime==3)], data$LATITUDE[which(data$insptime==3)], cex=log(data$TOTAL_TCAP[which(data$insptime==3)]+1),col="blue",lwd=2)
# 
# legend("topright",c("Inspected, no bugs", "Inspected, found bugs"),pch=c(1,1),
#        col=c("forestgreen","red"))
# 
# 
# par(mfrow=c(1,1))
# par(mar=c(1,1,1,1))
# plot(data$LONGITUDE, data$LATITUDE, cex=0.2,xlab="",ylab="")
# points(data$LONGITUDE, data$LATITUDE, cex=(1-data$INSP_POSITIVA)*0.5,col="forestgreen")
# points(data$LONGITUDE, data$LATITUDE, cex=log(data$TOTAL_TCAP+1),col=data$insptime+1,lwd=2)
# legend("topright",c("Inspected, no bugs", "Inspected, found bugs"),pch=c(1,1),
#        col=c("forestgreen","red"))
# 
# par(mfrow=c(1,1))
# par(mar=c(1,1,1,1))
# plot(m.prd1[which(data$insptime==3&data$INSP_POSITIVA<2)], sd.prd1[which(data$insptime==3&data$INSP_POSITIVA<2)],col="blue",pch=18,cex=0.3,ylim=c(0,1),xlim=c(0,1))
# points(m.prd1[which(data$insptime==1)], sd.prd1[which(data$insptime==1)],pch=18,cex=0.3)
# points(m.prd1[which(data$insptime==2)], sd.prd1[which(data$insptime==2)],col="red",pch=18,cex=0.3)
# points(m.prd1[which(data$insptime==3&is.na(data$INSP_POSITIVA))], sd.prd1[which(data$insptime==3&is.na(data$INSP_POSITIVA))],col="purple",pch=18,cex=0.3)
# points(m.prd1[which(data$insptime==3&data$INSP_POSITIVA<2)], sd.prd1[which(data$insptime==3&data$INSP_POSITIVA<2)],col="blue",pch=18,cex=0.3)
# 
# plot(m.prd1, sd.prd1)
# points(m.prd[which(!is.na(data$INSP_POSITIVA))], sd.prd[which(!is.na(data$INSP_POSITIVA))],col="red")
# 
# par(mfrow=c(1,1))
# par(mar=c(4,1,1,1))
# plot(data$LONGITUDE, data$LATITUDE, cex=0.2,xlab="",ylab="")
# points(dataforfit$LONGITUDE[which(dataforfit$insptime==3)],dataforfit$LATITUDE[which(dataforfit$insptime==3)],col="red",cex=0.2)
# 
# par(mfrow=c(2,1))
# par(mar=c(4,1,1,1))
# hist(m.prd1,breaks=20,col="skyblue",main="Mean Pred. Prob.")
# 
# par(mar=c(4,1,1,1))
# hist(sd.prd1,breaks=20,col="skyblue",main="SD")
# 
# par(mfrow=c(1,1))
# par(mar=c(5,5,1,1))
# plot(data$LONGITUDE, data$LATITUDE, cex=0.2,xlab="",ylab="",col=data$insptime+1)
# 
# 
# ##########################################################
# ##############adding streets as barriers##################
# ##########################################################
# 
# #################################
# ##create origin for every block##
# #################################
# 
# N.block <- length(unique(data$id_manz)) #573 blocks
# 
# data$originX <- NA
# data$originY <- NA
# 
# for(i in unique(data$id_manz)){
#   
#   data$originX[which(data$id_manz==i)] <- median(data$LONGITUDE[which(data$id_manz==i)])
#   data$originY[which(data$id_manz==i)] <- median(data$LATITUDE[which(data$id_manz==i)]) 
# }
# 
# plot(data$originX, data$originY,pch=18)
# 
# 
# ###################################################
# ##create x,y matrix for each house on block level##
# ###################################################
# 
# data$LONGITUDEdiff <- NA
# data$LATITUDEdiff <- NA
# 
# data$LONGITUDEdiff <- data$LONGITUDE - data$originX
# data$LATITUDEdiff <- data$LATITUDE - data$originY
# 
# ################################
# ##scale x,y of origin matrix##
# ###############################
# 
# scale.dim.sim <- function(S){
#   
#   Xscale <- data$originX*S+data$LONGITUDEdiff
#   Yscale <- data$originY*S+data$LATITUDEdiff
#   
#   #look at block origins
#   plot(Xscale-data$LONGITUDE, Yscale-data$LATITUDE,pch=18,cex=0.08)
#   points(data$LONGITUDE, data$LATITUDE,pch=18,cex=0.1,col="red")
#   
#   
#   #data scaled plotted together
#   plot(Xscale/S, Yscale/S,pch=18,cex=0.2)
#   points(data$LONGITUDE, data$LATITUDE,pch=18,cex=0.2,col="red")
#   
#   return(list(Xscale, Yscale))
#   
# }
# 
# 
# #################################
# #########recover values##########
# #################################
# 
# predictions.sim <- data
# sim.result <- NULL
# 
# for(j in seq(1,4,by=0.2)){
#   
#   new.coords <- scale.dim.sim(j)
#   data$LONGITUDEscale <- new.coords[[1]]
#   data$LATITUDEscale <- new.coords[[2]]
#   
#   #create dataset of observations
#   dataforfit <- data[which(data$INSP_POSITIVA_copy==1 | data$INSP_POSITIVA_copy==0),]
#   datapred <- data[which(data$insptime==4),]
#   
#   #define time dimension
#   k <- length(unique(data$insptime))
#   
#   
#   #define coordinate matrix
#   coords1<- cbind(dataforfit$LONGITUDEscale, dataforfit$LATITUDEscale)
#   coords <- cbind(datapred$LONGITUDEscale, datapred$LATITUDEscale)
#   
#   #create mesh for Socabaya
#   mesh1 <- inla.mesh.2d(coords1, max.edge=c(200*j,200*j),cutoff=0.2)
#   
#   plot(mesh1)
#   points(data$LONGITUDE,data$LATITUDE,pch=18,cex=0.2,col="blue")
#   points(dataforfit$LONGITUDE, dataforfit$LATITUDE,col="red",pch=18,cex=0.2)
#   
#   A.est <- inla.spde.make.A(mesh=mesh1,
#                             loc=coords1,
#                             group=dataforfit$insptime,n.group=k)
#   
#   A.prd <- inla.spde.make.A(mesh=mesh1,loc=coords,group=datapred$insptime)
#   
#   spde <- inla.spde2.matern(mesh1, alpha=1.5)
#   
#   #TO DO: specify spatial distribution
#   #right now using a matern spatial covariance
#   
#   mesh.index <- inla.spde.make.index(name='spatial', n.spde=spde$n.spde,n.group=k)
#   
#   stack.est =
#     inla.stack(data=list(y=dataforfit$INSP_POSITIVA_copy),
#                A=list(A.est,1,1),
#                effects=
#                  list(c(mesh.index,list(Intercept=1)),
#                       list(infFAstatus = dataforfit$infFAstatus.formodel),
#                       list(rocFAstatus = dataforfit$rocFAstatus.formodel)),
#                tag="est")
#   
#   stack.pred =
#     inla.stack(data=list(y=NA),
#                A=list(A.prd,1,1),
#                effects=
#                  list(c(mesh.index,list(Intercept=1)),
#                       list(infFAstatus = datapred$infFAstatus.formodel),
#                       list(rocFAstatus = datapred$rocFAstatus.formodel)),
#                tag="pred")
#   
#   
#   stack = inla.stack(stack.est, stack.pred)
#   
#   formula <- y ~ -1 + Intercept + infFAstatus +
#     rocFAstatus +
#     f(spatial, model=spde, group=spatial.group,
#       control.group=list(model='ar1'))
#   
#   
#   result.sim <- inla(formula,
#                  data = inla.stack.data(stack, spde=spde),
#                  family = c("binomial"),verbose=TRUE,
#                  control.predictor = list(A=inla.stack.A(stack), compute=TRUE,link=1)
#                  ,control.inla = list(reordering = "metis")
#                  ,control.fixed = list(mean = c(0,3.07, 1.17), prec = c(0.01,1,1)))
#   
#   theta.mean <- summary(result.sim)$hyperpar$mean
#   theta.sd <- summary(result.sim)$hyperpar$sd
#   intercept.mean <- summary(result.sim)$fixed[1,1]
#   intercept.sd <- summary(result.sim)$fixed[1,2]
#   cov1.mean <- summary(result.sim)$fixed[2,1]
#   cov1.sd <- summary(result.sim)$fixed[2,2]
#   cov2.mean <- summary(result.sim)$fixed[3,1]
#   cov2.sd <- summary(result.sim)$fixed[3,2]
#   lik <- summary(result.sim)$mlik[2]
#   sim.result.row <- cbind(theta.mean[1], theta.mean[2],theta.mean[3],
#                           theta.sd[1], theta.sd[2],theta.sd[3],
#                           intercept.mean, intercept.sd, 
#                           cov1.mean, cov1.sd,
#                           cov2.mean, cov2.sd,
#                           lik, j)
#   sim.result <- rbind(sim.result, sim.result.row)
#   
#   
#   id.prd <- inla.stack.index(stack,"pred")$data
#   sd.prd <- m.prd <- matrix(NA, dim(coords)[1], dim(coords)[1])
#   
#   m.prd <- result.sim$summary.fitted.values$mean[id.prd]
#   sd.prd <- result.sim$summary.fitted.values$sd[id.prd]
#   predictions.sim <- cbind(predictions.sim,m.prd, sd.prd)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #############################
# #########Simulation##########
# #############################
# 
# resultstoplot.sim <- resultstoplot
# 
# #length of simulation
# total.days <- 6
# sim.insp.neg <- matrix(NA, nrow=10,ncol=total.days)
# m.prd.sim <- sd.prd.sim <- matrix(NA, nrow=dim(datapred)[1],ncol=total.days)
# sim.insp.mat <- NULL
# data.temp <- data
# meanresults <- matrix(NA, nrow=total.days, ncol=6)
# m.prd.sim[,1] <- probability
# sd.prd.sim[,1] <- uncertainty
# 
# for(day in 2:(total.days)){
#   #create temporary matrix
#   temp.mat <- data.frame(datapred$UNICODE., m.prd.sim[,(day-1)])
#   ####say we inspect 10 houses with highest median probability each day and all are negative####
#   sim.insp.neg <- temp.mat[order(temp.mat[,2],decreasing=TRUE)[1:10],1]
#   resultstoplottemp <- resultstoplot
#   resultstoplottemp$sim.insp <- ifelse(resultstoplottemp$UNICODE. %in% sim.insp.neg, 1,0)
#   sim.insp.mat <- cbind(sim.insp.mat, resultstoplottemp$sim.insp)
# 
#   #plot which houses were inspecting
#   pdata(file=paste("Inspections Day ", day, sep=""))
#   par(mfrow=c(1,1))
#   par(mar=c(1,1,4,1))
#   plot(data$LONGITUDE, data$LATITUDE, cex=0.2,xlab="",ylab="",main=paste("Day", day, sep=" "))
#   #points(data$LONGITUDE, data$LATITUDE, cex=probability,col="blue",lwd=2)
#   points(resultstoplottemp$X, resultstoplottemp$Y, cex=resultstoplottemp$sim.insp,col="red",lwd=2,pch=18)
#   dev.off()
#   
#   #define houses as negative
#   data.temp$INSP_POSITIVA_copy[which(data.temp$UNICODE. %in% sim.insp.neg & data.temp$insptime==4)] <- 0
#   
#   #create dataset of observations
#   dataforfit.temp <- data.temp[which(data.temp$INSP_POSITIVA_copy==1 | data.temp$INSP_POSITIVA_copy==0),]
#   datapred.temp <- data.temp[which(data.temp$insptime==4),]
#   
#   #define time dimension
#   k <- length(unique(data.temp$insptime))
#   
#   
#   #define coordinate matrix
#   coords1<- cbind(dataforfit.temp$X, dataforfit.temp$Y)
#   coords <- cbind(datapred.temp$X, datapred.temp$Y)
#   
#   #create mesh for Socabaya
#   mesh1 <- inla.mesh.2d(coords1, max.edge=c(200,200),cutoff=0.2)
#   
#   
#   A.est <- inla.spde.make.A(mesh=mesh1,
#                             loc=coords1,
#                             group=dataforfit.temp$insptime,n.group=k)
#   
#   A.prd <- inla.spde.make.A(mesh=mesh1,loc=coords,group=datapred.temp$insptime)
#   
#   spde <- inla.spde2.matern(mesh1, alpha=1.5)
#   
#   mesh.index <- inla.spde.make.index(name='spatial', n.spde=spde$n.spde,n.group=k)
#   
#   stack.est =
#     inla.stack(data=list(y=dataforfit.temp$INSP_POSITIVA_copy),
#                A=list(A.est,1,1),
#                effects=
#                  list(c(mesh.index,list(Intercept=1)),
#                       list(infFAstatus = dataforfit.temp$infFAstatus.formodel),
#                       list(rocFAstatus = dataforfit.temp$rocFAstatus.formodel)),
#                tag="est")
#   
#   stack.pred =
#     inla.stack(data=list(y=datapred.temp$INSP_POSITIVA_copy),
#                A=list(A.prd,1,1),
#                effects=
#                  list(c(mesh.index,list(Intercept=1)),
#                       list(infFAstatus = datapred.temp$infFAstatus.formodel),
#                       list(rocFAstatus = datapred.temp$rocFAstatus.formodel)),
#                tag="pred")
#   
#   
#   stack = inla.stack(stack.est, stack.pred)
#   
#   formula <- y ~ -1 + Intercept + infFAstatus +
#     rocFAstatus +
#     f(spatial, model=spde, group=spatial.group,
#       control.group=list(model='ar1'))
#   
#   
#   result <- inla(formula,
#                  data = inla.stack.data(stack, spde=spde),
#                  family = c("binomial"),verbose=TRUE,
#                  control.predictor = list(A=inla.stack.A(stack), compute=TRUE,link=1)
#                  ,control.inla = list(reordering = "metis")
#                  ,control.fixed = list(mean = c(0,3.07, 1.17), prec = c(0.01,1,1)))
#   
#   result.f <- inla.spde2.result(result, "spatial", spde, do.transf=TRUE)
#   
#   #id.prd <- inla.stack.index(stk.all, "prd")$data
#   id.prd <- inla.stack.index(stack, "pred")$data
#   
#   m.prd.sim[,day] <- result$summary.fitted.values$mean[id.prd]
#   sd.prd.sim[,day] <- result$summary.fitted.values$sd[id.prd]
#   
#   meansimfixed <- summary(result)$fixed[,1]
#   meansimrandom <- summary(result)$hyperpar[,1]
#   
#   meanresults[day,] <- cbind(t(as.vector(meansimfixed)), t(as.vector(meansimrandom)))
#   
# 
#   print(day)
# }
# 
# save.image("~/Bandit/Socabaya/Simulation1.RData")
# ####plot results
# 
# colnames(m.prd.sim) <- c("m.prd1", "m.prd2", "m.prd3", "m.prd4", "m.prd5", "m.prd6")
# colnames(sd.prd.sim) <- c("sd.prd1", "sd.prd2", "sd.prd3", "sd.prd4", "sd.prd5", "sd.prd6")
# resultstoplot.sim <- cbind(data, m.prd.sim, sd.prd.sim)
# 
# ####plot original results
# pdata(file="Plot0.pdata", height=5, width=7)
# plot0 <- output[order(output$probability, decreasing=TRUE),]
# colfunc <- heat.colors(length(plot0$probability))
# layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
# plot(plot0$X,plot0$Y,col=colfunc,cex=0.14,pch=16,xlab="",ylab="")
# points(resultstoplottemp$X, resultstoplottemp$Y, cex=sim.insp.mat[,1]*0.3,col="black",pch=18)
# 
# legend_image <- as.raster(matrix(colfunc, ncol=1))
# plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Day 0')
# text(x=1.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
# rasterImage(colfunc, 0, 0, 1,1)
# dev.off()
# 
# 
# ##after 1 round of inspections
# pdata(file="Simulated Day 1.pdata",width=20, height=15)
# resultstoplot.sim <- cbind(datapred,m.prd.sim, sd.prd.sim)
# plot1 <- resultstoplot.sim[order(resultstoplot.sim$m.prd1, decreasing=TRUE),]
# colfunc <- heat.colors(length(plot1$m.prd1))
# layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
# plot(plot1$X,plot1$Y,col=colfunc,cex=0.14)
# points(resultstoplottemp$X, resultstoplottemp$Y, cex=sim.insp.mat[,1]*0.8,col="black",pch=1)
# 
# legend_image <- as.raster(matrix(colfunc, ncol=1))
# plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Day 1')
# text(x=1.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
# rasterImage(colfunc, 0, 0, 1,1)
# dev.off()
# 
# ##day 2
# pdata(file="Simulated Day 2.pdata",width=20, height=15)
# plot2 <- resultstoplot.sim[order(resultstoplot.sim$m.prd2, decreasing=TRUE),]
# colfunc <- heat.colors(length(plot2$m.prd2))
# layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
# plot(plot2$X,plot2$Y,col=colfunc,cex=0.14)
# points(resultstoplottemp$X, resultstoplottemp$Y, cex=sim.insp.mat[,2]*0.8,col="black",pch=1)
# 
# legend_image <- as.raster(matrix(colfunc, ncol=1))
# plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Day 2')
# text(x=1.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
# rasterImage(colfunc, 0, 0, 1,1)
# dev.off()
# 
# #day 3
# pdata(file="Simulated Day 3.pdata",width=20, height=15)
# plot3 <- resultstoplot.sim[order(resultstoplot.sim$m.prd3, decreasing=TRUE),]
# colfunc <- heat.colors(length(plot3$m.prd3))
# layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
# plot(plot3$X,plot3$Y,col=colfunc,cex=0.14)
# points(resultstoplottemp$X, resultstoplottemp$Y, cex=sim.insp.mat[,3]*0.8,col="black",pch=1)
# 
# legend_image <- as.raster(matrix(colfunc, ncol=1))
# plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Day 3')
# text(x=1.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
# rasterImage(colfunc, 0, 0, 1,1)
# dev.off()
# 
# #day 4
# pdata(file="Simulated Day 4.pdata",width=20, height=15)
# plot4 <- resultstoplot.sim[order(resultstoplot.sim$m.prd4, decreasing=TRUE),]
# colfunc <- heat.colors(length(plot4$m.prd4))
# layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
# plot(plot4$X,plot4$Y,col=colfunc,cex=0.14)
# points(resultstoplottemp$X, resultstoplottemp$Y, cex=sim.insp.mat[,4]*0.8,col="black",pch=1)
# 
# legend_image <- as.raster(matrix(colfunc, ncol=1))
# plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Day 4')
# text(x=1.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
# rasterImage(colfunc, 0, 0, 1,1)
# dev.off()
# 
# #day 5
# pdata(file="Simulated Day 5.pdata",width=20, height=15)
# plot5 <- resultstoplot.sim[order(resultstoplot.sim$m.prd5, decreasing=TRUE),]
# colfunc <- heat.colors(length(plot5$m.prd5))
# layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
# plot(plot5$X,plot5$Y,col=colfunc,cex=0.14)
# points(resultstoplottemp$X, resultstoplottemp$Y, cex=sim.insp.mat[,5]*0.8,col="black",pch=1)
# 
# legend_image <- as.raster(matrix(colfunc, ncol=1))
# plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Day 5')
# text(x=1.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
# rasterImage(colfunc, 0, 0, 1,1)
# dev.off()
# 
# #day 6
# pdata(file="Simulated Day 6.pdata",width=20, height=15)
# plot6 <- resultstoplot.sim[order(resultstoplot.sim$m.prd6, decreasing=TRUE),]
# colfunc <- heat.colors(length(plot6$m.prd6))
# layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
# plot(plot6$X,plot6$Y,col=colfunc,cex=0.14)
# 
# legend_image <- as.raster(matrix(colfunc, ncol=1))
# plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Day 6')
# text(x=1.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
# rasterImage(colfunc, 0, 0, 1,1)
# dev.off()
# 
# p.m <- ggplot(resultstoplot.sim) +
#   geom_point(aes(x=X, y=Y, col="1"),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7),limits=c(0,0.4))
# 
# p.m1 <- ggplot(resultstoplot.sim) +
#   geom_point(aes(x=X, y=Y, col=m.prd1),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7),limits=c(0,0.4))
# 
# p.m2 <- ggplot(resultstoplot.sim) +
#   geom_point(aes(x=X, y=Y, col=m.prd.sim[,2]),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7),limits=c(0,0.4))
# 
# p.m3 <- ggplot(resultstoplot.sim) +
#   geom_point(aes(x=X, y=Y, col=m.prd.sim[,3]),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7),limits=c(0,0.4))
# 
# p.m4 <- ggplot(resultstoplot.sim) +
#   geom_point(aes(x=X, y=Y, col=m.prd.sim[,4]),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7),limits=c(0,0.4))
# 
# p.m5 <- ggplot(resultstoplot.sim) +
#   geom_point(aes(x=X, y=Y, col=m.prd.sim[,5]),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7),limits=c(0,0.4))
# 
# 
# pdata("MDNplot3.pdata",width=20,height=10)
# grid.arrange(p.m,p.m1,p.m2,p.m3,p.m4,p.m5,p.m6,p.m7,p.m8,ncol=3,nrow=3)
# dev.off()
# 
# 
# p.sd <- ggplot(resultstoplot.sim) +
#   geom_point(aes(x=X, y=Y, col=sd.prd),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7),limits=c(0,0.3),title="sd.prd[,0]")
# 
# p.sd1 <- ggplot(resultstoplot.sim) +
#   geom_point(aes(x=X, y=Y, col=sd.prd.sim[,1]),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7),limits=c(0,0.3))
# 
# p.sd2 <- ggplot(resultstoplot.sim) +
#   geom_point(aes(x=X, y=Y, col=sd.prd.sim[,2]),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7),limits=c(0,0.3))
# 
# p.sd3 <- ggplot(resultstoplot.sim) +
#   geom_point(aes(x=X, y=Y, col=sd.prd.sim[,3]),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7),limits=c(0,0.3))
# 
# p.sd4 <- ggplot(resultstoplot.sim) +
#   geom_point(aes(x=X, y=Y, col=sd.prd.sim[,4]),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7),limits=c(0,0.3))
# 
# p.sd5 <- ggplot(resultstoplot.sim) +
#   geom_point(aes(x=X, y=Y, col=sd.prd.sim[,5]),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7),limits=c(0,0.3))
# 
# p.sd6 <- ggplot(resultstoplot.sim) +
#   geom_point(aes(x=X, y=Y, col=sd.prd.sim[,6]),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7),limits=c(0,0.3))
# 
# p.sd7 <- ggplot(resultstoplot.sim) +
#   geom_point(aes(x=X, y=Y, col=sd.prd.sim[,7]),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7),limits=c(0,0.3))
# 
# p.sd8 <- ggplot(resultstoplot.sim) +
#   geom_point(aes(x=X, y=Y, col=sd.prd.sim[,8]),size=0.1) +
#   scale_color_gradientn(colours=rainbow(7),limits=c(0,0.3))
# 
# pdata("SDplot3.pdata",width=20,height=10)
# grid.arrange(p.sd,p.sd1,p.sd2,p.sd3,p.sd4,p.sd5,p.sd6,p.sd7,p.sd8,ncol=3,nrow=3)
# dev.off()
# 
# par(mfrow=c(1,1))
# par(mar=c(4,4,1,1))
# plot(m.prd,pch=16,xlab="House",ylab="Median Probability")
# points(m.prd.sim[,1],col="gray20",cex=0.4)
# points(m.prd.sim[,2],col="gray30",cex=0.4)
# points(m.prd.sim[,3],col="gray40",cex=0.4)
# points(m.prd.sim[,4],col="gray50",cex=0.4)
# points(m.prd.sim[,5],col="gray60",cex=0.4)
# points(m.prd.sim[,6],col="gray70",cex=0.4)
# points(m.prd.sim[,7],col="gray80",cex=0.4)
# points(m.prd.sim[,8],col="gray90",cex=0.4)
# 
# y <- m.prd
# for (i in 1:8){
#   y <- cbind(y, m.prd.sim[,i])
# }
# 
# plot(c(1:9),y[1,],type="o",col=i,ylim=c(0,0.4),ylab="Mdn Prob")
# for(i in 2:9614){
#   points(c(1:9),y[i,],type="o",col=i)  
# }
# 
# 
# y <- sd.prd
# for (i in 1:8){
#   y <- cbind(y, sd.prd.sim[,i])
# }
# 
# plot(c(1:9),y[1,],type="o",col=i,ylim=c(0,0.05),ylab="Std Dev")
# for(i in 2:9614){
#   points(c(1:9),y[i,],type="o",col=i)  
# }
# 
# 
# 
# 
# par(mfrow=c(1,1))
# par(mar=c(4,4,1,1))
# plot(sd.prd,pch=16,xlab="House",ylab="Std. Dev.")
# points(sd.prd.sim[,1],col="gray20",cex=0.4)
# points(sd.prd.sim[,2],col="gray30",cex=0.4)
# points(sd.prd.sim[,3],col="gray40",cex=0.4)
# points(sd.prd.sim[,4],col="gray50",cex=0.4)
# points(sd.prd.sim[,5],col="gray60",cex=0.4)
# points(sd.prd.sim[,6],col="gray70",cex=0.4)
# points(sd.prd.sim[,7],col="gray80",cex=0.4)
# points(sd.prd.sim[,8],col="gray90",cex=0.4)
# 
# colfunc <- gray.colors(resultstoplot.sim$m.prd, start = 0, end = 0.4, gamma = 2.2, alpha = NULL)
# plot(resultstoplot.sim$X,resultstoplot.sim$Y,col=colfunc,cex=0.1)
# 
# library(caTools)
# 
# pdata("p.m.pdata",height=7,width=9)
# p.m
# dev.off()
# 
# 
# for(i in 1:8) {
#   pdata(paste("p.m", i, ".pdata", sep = ""),height=7,width=9)
#   get(paste("p.m", i, sep = ""))
#   dev.off()
# }
# 
# pdata("p.sd.pdata",height=7,width=9)
# p.sd
# dev.off()
# 
# 
# for(i in 1:8) {
#   pdata(paste("p.sd", i, ".pdata", sep = ""),height=7,width=9)
#   get(paste("p.sd", i, sep = ""))
#   dev.off()
# }
# plot(75,median(resultstoplot.sim[,75]),ylim=c(0.0001,0.01))
# for (i in seq(77,91,2)){
#   points(i,median(resultstoplot.sim[,i]))
# }
# 
# plot(75,mean(resultstoplot.sim[,75]),ylim=c(0.0001,0.01))
# for (i in seq(77,91,2)){
#   points(i,mean(resultstoplot.sim[,i]))
# }
# 
# plot(76,median(resultstoplot.sim[,76]),ylim=c(0.0001,0.01),xlim=c(76,92))
# for (i in seq(78,92,2)){
#   points(i,median(resultstoplot.sim[,i]))
# }
# 
# plot(76,mean(resultstoplot.sim[,76]),ylim=c(0.0001,0.01),xlim=c(76,92))
# for (i in seq(78,92,2)){
#   points(i,mean(resultstoplot.sim[,i]))
# }
# 
# # 
# # stk.dat =inla.stack(data=list(y=dataforfit$INSP_POSITIVA,link="logit"),
# #              A=list(A.est,1),
# #              effects=list(c(mesh.index,
# #                     list(Intercept=1)),
# #                   list(dataforfit[,73:74])),
# #              tag="est")
# # 
# # r.s <- inla(formula, family="binomial",
# #             data=inla.stack.data(stk.dat), verbose=TRUE,
# #             control.predictor=list(A=inla.stack.A(stk.dat),
# #                                    compute=TRUE))
# # 
# # r.f <- inla.spde2.result(r.s, "spatial", spde, do.transf=TRUE)
# # 
# # 
# # par(mfrow=c(2,3))
# # par(mar=c(5,3,1,1))
# # plot(r.s$marginals.fix[[1]], type="l", xlab="Intercept",
# #        ylab="Density")
# # plot(r.s$marginals.hy[[1]], type="l", ylab="Density",
# #        xlab=expression(phi))
# # plot.default(r.f$marginals.variance.nominal[[1]], type="l",
# #                xlab=expression(sigma[x]^2), ylab="Density")
# # plot.default(r.f$marginals.range.nominal[[1]], type="l",
# #                xlab="Practical range", ylab="Density")
# # plot(r.s$marginals.hy[[2]], type="l", ylab="Density",
# #      xlab=names(r.s$marginals.hy)[2])
# # #define coordinate matrix
# # coord.prd <- cbind(data$LONGITUDE, data$LATITUDE)
# # 
# # 
# # projgrid <- inla.mesh.projector(mesh1,loc=coord.prd)
# # 
# # A.prd <- projgrid$proj$A
# # 
# # ef.prd = list(c(mesh.index,
# #                              list(Intercept=1),
# #                            list(data[,73:74])),
# #               tag="prd")
# # 
# # stk.prd <- inla.stack(data=list(y=NA), A=list(A.prd,1),
# #                         tag="prd", effects=list(c(mesh.index,
# #                                                   list(Intercept=1)),
# #                                                 list(data[,73:74])))
# # stk.all <- inla.stack(stk.dat, stk.prd)
# # 
# # 
# # r2.s <- inla(formula, family="binomial",
# #              data=inla.stack.data(stk.all),
# #              control.predictor=list(A=inla.stack.A(stk.all),
# #                                     compute=TRUE,link = 1),
# #              quantiles=NULL,
# #              control.results=list(return.marginals.random=F,
# #                                   return.marginals.predictor=F),
# #              verbose=TRUE,
# #              control.inla = list(int.strategy = "eb"))
# # 
# # 
# # id.prd <- inla.stack.index(stk.all, "prd")$data
# # sd.prd <- m.prd <- matrix(NA, dim(coord.prd)[1], dim(coord.prd)[1])
# # 
# # m.prd2 <- r2.s$summary.fitted.values$mean[id.prd]
# # sd.prd2 <- r2.s$summary.fitted.values$sd[id.prd]
# # 
# # results <- cbind(data, m.prd2, sd.prd2)
# # 
# # 
# # ggplot(results) +
# #   geom_point(aes(x=X, y=Y, col=m.prd2),size=0.4) +
# #   scale_color_gradientn(colours=rainbow(7))
# # 
# # 
# # ggplot(results) +
# #   geom_point(aes(x=X, y=Y, col=sd.prd2),size=0.5) +
# #   scale_color_gradientn(colours=rainbow(7))
# # 
# # 
# # 
