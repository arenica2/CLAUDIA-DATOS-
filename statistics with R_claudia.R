##### CLAUDIA AREVALO#####
#### PRACTICING WITH R STATISTICS#####

library(ISwR)
plot(rnorm(1000))

exp(-2)

rnorm(15)

weight<-c(60,72,57,90,95,72)
height<-c(1.75,1.80,1.65,1.90,1.74,1.91)

bmi<-weight/height^2
t.test(bmi,mu = 22.5)
# One Sample t-test
# 
# data:  bmi
# t = 0.34488, df = 5, p-value = 0.7442
# alternative hypothesis: true mean is not equal to 22.5
# 95 percent confidence interval:
#   18.41734 27.84791
# sample estimates:
#   mean of x 
# 23.13262 

plot(height,weight)

plot(height,weight,pch=2)
hh<-c(1.65,1.70,1.75,1.80,1.85,1.90)
lines(hh,22.5*hh^2)
d







