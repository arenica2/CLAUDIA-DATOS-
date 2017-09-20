###Rabia####
library(data.table)

vivienda<-read.csv("~/Downloads/vivienda.csv",sep = ",")
perro_segu<-read.csv("~/Downloads/perro_seguimiento.csv",sep = ';')
perro_nuevo<-read.csv('~/Downloads/perro_nuevo.csv',sep = ';')
setnames(vivienda,'VIVIENDA_UNICODE','UNICODE')
setnames(perro_nuevo,'UNICODE_PERRO_NUEVO','UNICODE')
setnames(perro_segu,'UNICODE_PERRO_SEGUIMIENTO',"UNICODE")
viv_perro_seg<-merge(vivienda,perro_segu,by = 'UNICODE',all =  TRUE)
viv_perro_seg_NUEVO<-merge(viv_perro_seg,perro_nuevo,by='UNICODE',all=TRUE)
merge_2016<-read.csv("~/Rabies/rabia ASA encuestas 2016/resultados/MERGE_ENCUESTAS_PERROS_JUN_2017.csv")
 aux<-merge(merge_2016,viv_perro_seg_NUEVO,by = 'UNICODE',all.y = TRUE)

 library(data.table)
 model_2<-read.csv('~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-04-25.csv',sep = ' ')
 





####modelo visitas por persona######
library(data.table)
model_2<-read.csv('~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-04-25.csv',sep = ' ')
model_3<-read.csv('~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-04-26.csv',sep = ' ')
model_4<-read.csv('~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-04-27.csv',sep = ' ')
model_5<-read.csv('~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-05-01.csv',sep = ' ')
model_6<-read.csv('~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-05-02.csv',sep = ' ')
model_7<-read.csv('~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-05-03.csv',sep = ' ')
model_8<-read.csv('~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-05-04.csv',sep = ' ')
model_9<-read.csv('~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-05-08.csv',sep = ' ')
#model_10<-read.csv('~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-05-10.csv',sep = ' ')
model_11<-read.csv('~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-05-12.csv',sep = ' ')
model_12<-read.csv('~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-05-14.csv',sep = ' ')
model_13<-read.csv('~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-05-15.csv',sep = ' ')
model_14<-read.csv('~/PETM-shiny/MODEL_RESULTS/MODEL_RESULTS_2017-05-17.csv',sep = ' ')

model_2<-as.data.table(model_2)
m<-max(model_2$probability)
model_2$probabilidad_relativa<- model_2$probability/m
m2<-max(model_3$probability)
model_3$probabilidad_relativa<- model_3$probability/m2
m3<-max(model_4$probability)
model_4$probabilidad_relativa<- model_4$probability/m3

MANZANAS<-read.csv('~/PETM-shiny/unicode_numberMzn/Manzanas _Arequipa/Mariano Melgar/MARIANO MELGAR.csv',sep = ',')
cerrocolorado<-read.csv()

set.seed(123)
df <- data.frame(id       = sample(1:100, 20, replace = TRUE),
                 inputval = sample(seq(0, 1, by=0.01), 20, replace = TRUE),
                 outcome  = sample(1:4, 20, replace = TRUE))

cols <- with(df, ifelse(outcome == 1, 'magenta', 'white'))
#### para hacer informes y sacar pdf o html como documento#####
library('htmlTable')
htmlTable(as.matrix(df), col.rgroup = cols)

ejemplo<-htmlTable(as.matrix(df), col.rgroup = cols)

library(DT)
datatable(df, rownames = FALSE) %>%
  formatStyle(columns = "inputval", 
              background = styleInterval(c(0.7, 0.8, 0.9)-1e-6, c("white", "lightblue", "magenta", "white"))) %>%
  formatStyle(columns = "outcome", 
              background = styleEqual(c(1, 4), c("magenta", "lightblue")))

x <- head(cars)
where <- rbind(c(2,2), c(2,1), c(5,2))
style <- c('background-color: red; color: white;',
           'border: solid 1px;',
           'font-weight: 900; color: blue;')

css.cell <- matrix('', nrow(x), ncol(x))
css.cell[where] <- style
htmlTable(head(cars), css.cell = css.cell)



tableHTML(mtcars) %>%
  add_css_header(css = list(c('background-color', 'border'), c('lightgray', '3px solid green')),
                 headers = 2)
tableHTML(mtcars) %>%
  add_css_header(css = list(c('background-color', 'border'), c('lightgray', '3px solid green')),
                 headers = c(1, 4))

able(text_tbl, booktabs = T) %>%
  kable_styling(position="center")%>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T) %>%
  column_spec(2, width = "28em")%>%
  landscape() 


GNsrtm3(poligonos$long,poligonos$lat)
GNgtopo30(poligonos$lat,poligonos$long)


text_tbl <- data.frame(
  UNICODE= c("Sigue aqui en la casa?", "Se murio,perdio,escapo o ahora vive en otra casa?", "Esa nueva casa donde queda en ASA o en otro ditrito de Arequipa o fuera de Arequipa?","Este anho vacunaron a.....?","Cuando lo vacunaron,(poner fecha)","Donde lo vacunaron?", "Que dificultades tuvo para llevarlo a vacunar"),
  Features = c(
    "SI   NO    NS" ,"Murió   Perd/escap   Otra casa ","    ","SI   NO   NS","       /    /    ","    ","   "))



#######code to push model results to app

library(RMySQL)
library(DBI)
library(png)

source('~/PETM-shiny/shiny/global.R')
source('~/PETM-shiny/shiny/loadSaveMethods.R')
#upload_csv_table(paste(output_direc, filename, sep=""), 'Model_Results')
upload_csv_table('~/Rabies/rabia_ASA_encuesta_2017/base/perro_para_jalar_2017.csv','CLUSTER_ENCUESTAS_PERRO')
######################
library(data.table)
library(ggplot2)
library(geosphere)
library(ggmap)
geosphere::areaPolygon(x=poligonos)
poligonos<-read.csv("~/Downloads/poligonos_localidadesAQP_03jun2015.csv",sep = ";")
poligonos<-as.data.table(poligonos)
poligonos<-poligonos [lat!=" "]
poligonos<-poligonos[, c('CODELOC','BLOCK') := tstrsplit(ident, "-", fixed=TRUE)]
poligonos<-poligonos[CODELOC=='Santa Rosa']
poligonos <- poligonos[order(poligonos$BLOCK),]
poligonos<-poligonos[,c("lat","long")]


poligonos<-as.data.frame(poligonos)
plot.new()
ggplot()+
  geom_polygon(data = poligonos, aes(x=poligonos$long, y = poligonos$lat, group = CODELOC),size=0.2, fill = NA, color = "black")

ggplot(poligonos)




library(data.table)


cerro<-read.csv("~/Participation/inpecciones_participation_cc/CERRO_AVANCE.csv",sep = ";")
cerro<-as.data.table(cerro)
loc_16<-cerro[, c('P','D','L','V') := tstrsplit(UNI_CODE, ".", fixed=TRUE)]
loc_16<-cerro[L=='16'& STATUS_INSPECCION=='V']
LOC_21<-cerro[L=='21'& STATUS_INSPECCION=='V']
LOC_20<-cerro[L=='20' & STATUS_INSPECCION=='V']
LOC_17<-cerro[L=='17' & STATUS_INSPECCION=='V']
LOC_37<-cerro[L=='37' & STATUS_INSPECCION=='V']
LOC_24<-cerro[L=='24' & STATUS_INSPECCION=='inspeccion']
loc_40<-cerro[L=='40' & STATUS_INSPECCION=='V']
LOC_41<-cerro[L=='41' & STATUS_INSPECCION=='V']
LOC_43<-cerro[L=='43' & STATUS_INSPECCION=='V']
LOC_51<-cerro[L=='51' & STATUS_INSPECCION=='V']
LOC_24<-cerro[L=='24' & STATUS_INSPECCION=='inspeccion']

#############example of INLA
data("SPDEtoy")
library(INLA)
dim(SPDEtoy)

data<-SPDEtoy
######## fising dates and inconsistencies in dates####
encuestas_minsa<-read.csv("~/PETM-shiny/Static_Data_formodel/TODO LOS DISTRITOS ENCUESTAS MINSA.csv")
encuestas_minsa<-encuestas_minsa[,-c(1)]

#fixing inconsistencies
encuestas_minsa$ANIO[encuestas_minsa$ANIO=="14"]<-'2014'
encuestas_minsa$ANIO[encuestas_minsa$ANIO=="15"]<-'2015'
encuestas_minsa$ANIO[encuestas_minsa$ANIO=="16"]<-'2016'
#fixing dates####claudia probando
encuestas_minsa$FECHA<-as.Date(paste(encuestas_minsa$ANIO,encuestas_minsa$MES,encuestas_minsa$DIA),"%Y%m%d")
write.csv(encuestas_minsa,"~/PETM-shiny/Static_Data_formodel/TODO LOS DISTRITOS ENCUESTAS MINSA.csv",row.names = FALSE)
