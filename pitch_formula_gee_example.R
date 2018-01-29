age = c(14,23,35,48,52,67)
pitch = c(252,244,240,233,212,204)
my.df = data.frame(age,pitch)
xmdl = lm(pitch ~ age, my.df)
summary(xmdl)


my.df$age.c = my.df$age - mean(my.df$age)
xmdl = lm(pitch ~ age.c, my.df)
summary(xmdl)

plot(fitted(xmdl),residuals(xmdl))
plot(residuals(xdml))
plot(rnorm(100),rnorm(100))
plot(residuals(rnorm(100)),fitted(rnorm(100)))
hist(residuals(xmdl))
qqnorm(residuals(xmdl))
dfbeta(xmdl)
library(lme4)
politeness= read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
which(!complete.cases(politeness))
which(is.na(politeness$frecuency))



boxplot(frequency ~ attitude*gender,
        col=c("white","lightgray"),politeness)

lmer(frequency ~ attitude, data=politeness)
politeness.model = lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=politeness)
summary(politeness.model)



##examples loops
u1 <- rnorm(30) # create a vector filled with random normal values
print("This loop calculates the square of the first 10 elements of vector u1")

usq<-0
for(i in 1:10) 
{
  usq[i]<-u1[i]*u1[i] # i-th element of u1 squared into i-th position of usq
  print(usq[i])
}
print(i)
# nested for: multiplication table
mymat = matrix(nrow=30, ncol=30) # create a 30 x 30 matrix (of 30 rows and 30 columns)
for(i in 1:dim(mymat)[1])  # for each row
{
  for(j in 1:dim(mymat)[2]) # for each column
  {
    mymat[i,j] = i*j     # assign values based on position: product of two indexes
  }
}

readinteger <- function()
{ 
  n <- readline(prompt="Please, enter an integer: ")
  # return(as.integer(n))
}
nr<-as.integer(readinteger())
mymat = matrix(0,nr,nr) # create a n x n matrix with zeroes (n rows and n columns)

for(i in 1:dim(mymat)[1])  # for each row
{
  for(j in 1:dim(mymat)[2]) # for each column
  {
    mymat[i,j] = i*j     # assign values based on position: product of two indexes
  }
}
if (nr>10) 
{
  mymat[1:10,1:10]  # for space reasons, we just show the first 10x10 chunk
} else mymat[1:nr,1:nr]  # ...or the first nr x nr chunk
}





