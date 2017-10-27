x <- 10
f1 <- function(x) {
  function() {
    x + 10
  }
}
f1(1)()

f <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
f()
rm(f)

x <- 2
g <- function() {
  y <- 1
  c(x, y)
}
g()
rm(x, g)


x <- 1
h <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}
h()
rm(x, h)
j <- function(x) {
  y <- 2
  function() {
    c(x, y)
  }
}
k <- j(1)
k()
rm(j, k)
##############################
#############################


l <- function(x) x + 1
m <- function() {
  l <- function(x) x * 2
  l(l)
}
m()

m()

n <- function(x) x / 2
o <- function() {
  n <- 10
  n(n)
}
o()

f <- function() x
x <- 20
f()
codetools::findGlobals(f)
f <- function() x + 1


f <- function(x) {
  f <- function(x) {
    f <- function(x) {
      x ^ 2
    }
    f(x) + 1
  }
  f(x) * 2
}
f(5)


f(10)
x<-5
f()

x <- 10
y <- 5
x + y

for (i in 1:15) print(i)

add <- function(x, y) x + y
sapply(1:20, add, 3)


f <- function(abcdef, bcde1, bcde2) {
  list(a = abcdef, b1 = bcde1, b2 = bcde2)
}
str(f(1, 2, 3))

# Create a vector filled with random normal values
u1 <- rnorm(30)
print("This loop calculates the square of the first 10 elements of vector u1")

# Initialize `usq`
usq <- 0

for(i in 1:10) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  usq[i] <- u1[i]*u1[i]
  print(usq[i])
}

print(i)

# Create a 30 x 30 matrix (of 30 rows and 30 columns)
mymat <- matrix(nrow=30, ncol=30)
i=10
# For each row and for each column, assign values based on position: product of two indexes
for(i in 1:dim(mymat)[1]) {
  for(j in 1:dim(mymat)[2]) {
    mymat[i,j] = i*j
  }
}

# Just show the upper left 10x10 chunk
mymat[1:10, 1:10]

# Create your three-dimensional array
my_array <- array(1:20, dim=c(20, 20, 20))

for (i in 1:dim(my_array)[1]) {
  for (j in 1:dim(my_array)[2]) {
    for (k in 1:dim(my_array)[3]) {
      my_array[i,j,k] = i*j*k
    }
  }
}

# Show a 10x10x15 chunk of your array
my_array[1:10, 1:10, 1:15]

# Insert your own integer here
my_int <- 4

nr <- as.integer(my_int)

# create a `n` x `n` matrix with zeroes
mymat <- matrix(0,nr,nr)

# For each row and for each column, assign values based on position
# These values are the product of two indexes
for(i in 1:dim(mymat)[1]) {
  for(j in 1:dim(mymat)[2]) {
    mymat[i,j] = i*j
  }
}

# Show the first 10x10 chunk or the first `nr` x `nr` chunk
if (nr > 10) {
  mymat[1:10, 1:10]
} else mymat[1:nr, 1:nr]

# Your User Defined Function
readinteger <- function(){
  n <- readline(prompt="Please, enter your ANSWER: ")
}

response <- as.integer(readinteger())

while (response!=42) {   
  print("Sorry, the answer to whatever the question MUST be 42");
  response <- as.integer(readinteger());
}

readinteger <- function(){
  n <- readline(prompt="Please, enter your ANSWER: ") 
}

repeat {   
  response <- as.integer(readinteger());
  if (response == 42) {
    print("Well done!");
    break
  } else print("Sorry, the answer to whatever the question MUST be 42");
}
34
# Make a lower triangular matrix (zeroes in upper right corner)
m=10 
n=10

# A counter to count the assignment
ctr=0

# Create a 10 x 10 matrix with zeroes 
mymat = matrix(0,m,n)

for(i in 1:m) {
  for(j in 1:n) {   
    if(i==j) { 
      break;
    } else {
      # you assign the values only when i<>j
      mymat[i,j] = i*j
      ctr=ctr+1
    }
  }
  print(i*j) 
}

# Print how many matrix cells were assigned
print(ctr)
#####
m=20

for (k in 1:m){
  if (!k %% 2)
    next
  print(k)
}
#######
for (i in 1:n) { 
  v3[i] <-v1[i] + v2[i] 
}
v3
##############
# This is a bad loop with 'growing' data
set.seed(42)
m=10
n=10

# Create matrix of normal random numbers
mymat <- replicate(m, rnorm(n))

# Transform into data frame
mydframe <- data.frame(mymat)

for (i in 1:m) {
  for (j in 1:n) {
    mydframe[i,j]<-mydframe[i,j] + 10*sin(0.75*pi)
    print(mydframe)
  }
}
######
set.seed(42)
m=10
n=10
mymat <- replicate(m, rnorm(n)) 
mydframe <- data.frame(mymat)
mydframe <- mydframe + 10*sin(0.75*pi)
mydframe

