distr <- paste("dist",letters[1:16])
set.seed(1977)
report_b <- rpois(16,1.4)
report <- report_b^2
distr_denuncias <- data.frame(distr,report)

distr_denuncias <- distr_denuncias[order(distr_denuncias$report),]

distr_mat <- matrix(distr_denuncias$distr, ncol=2, byrow=T)
dat <- data.frame(distr_mat)
names(dat) <- c("col1","col2")

set.seed(1977)
dat$group_col1<-sample(c("control","afiche"), length(dat$col1), replace=T)

library(data.table)
distritos_denuncias<-read.csv("/Users/Rodrigo/Downloads/DISTRITOS CON CHINCHES.csv")

#ordenando las 
distritos_denuncias<-as.data.table(distritos_denuncias)
distritos_denuncias<-distritos_denuncias[order(distritos_denuncias$NUMERO.DE.DENUNCIAS)]
aux<-matrix(distritos_denuncias$DISTRITO,ncol = 2,byrow = T)
aux<-as.data.table(aux)
setnames(aux,"V1","PAR_1")
setnames(aux,"V2","PAR_2")
aux$GROUP_PAR1<-sample(c("CONTROL","AFICHE"),length(aux$PAR_1),replace = T)
aux[,GROUP_PAR2:=ifelse(GROUP_PAR1=="CONTROL","AFICHE","CONTROL")]
  merge_ExP[, ADDED_II_CICLO := ifelse(ESTA_ESTRATEGIAS== 0, 1, 0)]