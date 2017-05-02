## Función sample (muestreos aleatorios)
?sample

# La función sample hace muestreos de los elementos contenidos en el
# primer argumento. Por defecto muestrea sin reposición todos los
# elementos posibles:
sample(1:5)
sample(1:5)
sample(1:5)

# como el lector puede observar, cada nueva corrida del comando genera un muestreo
# aleatorio, por lo tanto es esperable que el orden de los números sea siempre 
# distinto. Intente probar con una mayor cantidad de números o correr varias veces
# el argumento de arriba...

# Nótese que este comando es básicamente el equivalente a tomar aleatoriamente 
# elementos de un conjunto,
# tal como sucede en una lotería o un sorteo. 

# En la ayuda se pueden ver varios argumentos para modificar el comportamiento 
# de la función. Veamos algunos ejemplos:
sample(1:5, replace=TRUE)  # muestro con reposición
sample(1:5, 2)             # muestrea sólo dos valores, sin reposición
sample(1:5, size = 2)      # ídem
sample(1:5, 2, replace = TRUE) # los dos casos anteriores combinados

# Los mismos resultados se pueden obtener si usamos vectores "character":
sample(c('Homero', 'Marge', 'Bart', 'Lisa', 'Maggie'), 3, replace=TRUE) # nombres
sample(letters, 5) # letras

# Algo que puede confundir: si le damos solamente un número como entrada, puede
# tener comportamientos tal vez inesperados:
sample(10)     # esto es idéntico a pedir "sample(1:10)"
sample(10.3)   # redondea 10.3 --> 10, así que es igual que el anterior o a:
sample(floor(10.3)) # La función floor es para redondear "hacia abajo"
sample(-10.3)  # el comportamiento es distinto ahora, por ser negativo ...

# Otra opción útil puede ser usar el argumento "prob" de la función sample, 
# lo que permite que el muestreo sea sesgado hacia ciertos valores. Este 
# argumento permite asignar distintos valores de peso a cada uno de los 
# elementos del vector a muestrear.

# Considerando el siguiente ejemplo genérico (no correr):
sample(x, 100, replace = T, prob = p)

# Aquí p debe tener la misma longitud/length que x, de forma que se asignen 
# los "pesos relativos" de cada valor de x.

# Por ejemplo, si queremos muestrear los valores 5, 10 y 15 pero de tal
# forma que el valor 5 sea 3 veces más frecuente que el 15, y que el
# el 10 sea 2 veces más frecuente que el 15, entonces podemos ingresar:
muestreo <- sample(c(5, 10, 15), size=6000, prob=c(3, 2, 1), replace=T)
x <- table(muestreo)
x  # x tiene por nombres los valores 5, 10 y 15
x['5'] / x['15']  # ¿Es el valor que esperábamos?
x['5'] / x['10']  # ¿Es el valor que esperábamos?
# las comillas indican el nombre del elemento
# en el resultado el 5 indica el nombre del elemento, de forma similar
# a cuando pedimos que muestre x.

# Nótese que:
# prob es un sólo argumento, pero eso no equivale a un sólo número, si no
# a un sólo objeto (en este caso un vector, con tres elementos).
# prob puede aceptar valores decimales, incluso pueden asignarse
# probabilidades, aunque da error si alguno es negativo.
# Internamente los valores son siempre convertidos a un valor
# de probabilidad (entre 0 y 1); si el usuario ingresa lo siguiente:
# prob=c(A, B, C)
# Entonces transforma los valores así:
# a = A / (A + B + C)
# b = B / (A + B + C)
# c = C / (A + B + C)
# Y utiliza a, b y c como las probabilidades de que sean muestreados
# los elementos correspondiente del vector a muestre#

#FORMA NUMERO 1  
#ESCOGIENDO QUE DISTRITOS DE AREQUIPA SIN DENUNCIAS SERAN USADOS COMO CONTROL 
CONTROL<-read.csv("/Users/Rodrigo/Downloads/AQP_DIST.csv")
set.seed(1)
INDEX_1<-sample(1:nrow(CONTROL),2)
CONTROL[INDEX_1,]
#Los distritos elegidos
# son Characato y Miraflores 
#ORDEN   DISTRITO
#3     3  Characato
#4     4 Miraflores

#escogiendo cual sera primero si o no 
sample(c("SI","NO"),size =2,replace = TRUE )
#RESULTADOS:
#"NO"primero
#"SI"segundo
#Leyendo la base datos de distritos con cimex
distritos_denuncias<-read.csv("/Users/Rodrigo/Downloads/DISTRITOS CON CHINCHES.csv")

set.seed(123)
index <- sample(1:nrow(distritos_denuncias), 6)
index
distritos_denuncias[index, ]
#RESULTADOS
#ORDEN          DISTRITO     CODIGO.DISTRITO     NUMERO.DE.DENUNCIAS       NUMERO.DE.INSPECCIONES.POSITIVAS
#4      4 CERRO COLORADO               4                  16                               14
#9      9        TIABAYA              24                   5                                5
#5      5         HUNTER               7                   3                                3
#8      8     PAUCARPATA              13                   2                                2
#11    11      CHARACATO               5                   0                                0
#1      1            ASA               1                  16                               13

#A estos distritos no se les pondra afiches su asignacion sera "NO"
#A los restantes se le spondra afiches su asiganacion sera "SI"

#ORDEN          DISTRITO     CODIGO.DISTRITO     NUMERO.DE.DENUNCIAS       NUMERO.DE.INSPECCIONES.POSITIVAS
#2               CERCADO          2                 1                           1
#3               CAYMA            3                 6                           6
#6            J. L. B. y R.       8                 4                           4
#7            MARIANO MELGAR      10                3                           3
#10             SOCABAYA          25                6                           6
#12            MIRAFLORES         11                0                           0

#ELIIENDO FORMA 3 
#DIVIENDO EN CUATRO CATEGORIAS ALTO,MEDIO,BAJO ,CONTROL
#ALTO 10:16
#MEDIO 4:6
#BAJO 1:3
#CONTROL 0 DENUNCIAS 
#RESULTADOS 
set.seed(122)
int<- sample(c(1,4),size =1 ,p=c(1,1),replace = T)#en la 4 no se pondra afiches EN ASA SI 
int1<-sample(c(3,10,9,6),size = 2,replace = T)#sin afiche en 3 y 9 CAYMA Y TIABAYA,SOCABAYA Y JLBY R CON AFICHES 
int2<-sample(c(5,7,8,2),size = 2,replace = T)# sin afiche en 8 y 2 en PAUCARPATA Y CERCADO,HUNTER Y M.MELGAR CON
int3<-sample(c(11,12),size = 1,p=c(1,1),replace = T)#sin afiche 12 miraflores,con afiche Characato.


#####################################################################
#######################################################################
library(data.table)
distritos_denuncias<-read.csv("/Users/Rodrigo/Downloads/DISTRITOS CON CHINCHES.csv")

#ordenando las 
distritos_denuncias<-as.data.table(distritos_denuncias)
distritos_denuncias<-distritos_denuncias[order(distritos_denuncias$NUMERO.DE.DENUNCIAS)]



indic<-rep(c(0,1),6)
dat <- cbind(distritos_denuncias, indic)

split(nrow(dat), sample(C(1,0), replace = TRUE))



ss <- sample(c(0,1),size=nrow(dat),replace=TRUE,prob=c(0.5,0.5))

si <- createDataPartition(nrow(dat), p = .5, 
                                  list = FALSE, 
                                  times = 1)

dat<-as.data.table(dat)
setkey(dat$indic)



AUX<-dat[ifelse(dat$indic==1 ,"SI", "NO")]
set.seed(1) # So you get the same random numbers each time you run it.
ifelse(df$X3 <= runif(n), "SI", "NO")







