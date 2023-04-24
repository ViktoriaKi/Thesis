#------------------- Misspecifications ----------------------#

#Setup 
library(stats)
# toeplitz
n <- 1000
p <- 50
rho <- 0
level<-0.05 #17/02/23 VK, setting significance level only once
Cov <- toeplitz(rho ^ (seq(0, p - 1)))
sel.index <- c(1, 5, 10, 15, 20)
ind <- sel.index
beta <- rep(0, p)
beta[sel.index] <- 70
sparsity <- length(sel.index) # 17/02/23 VK, changed so that value automatically updates
#set.seed(42) # to make different methods comparable, fix the x-matrix
x <- mvrnorm(n, rep(0, p), Cov)
print (x[1,1])
# should create the right x on D-MATH server, x[1 ,1] = 0.958
xb <- x %*% beta
p.true <- exp(xb) / (1 + exp(xb))

#Binomial distribution
y <- rbinom(n,1,p.true) 

# Old notation 
# ylim <- runif(n)
# y2 <- rep(0, n)
# y2[ylim < p.true] <- 1

 

#trying to see the distribution

trials<-1:1000
mylist <- list()
for(i in 1:length(trials)){
y <- rbinom(n,1,p.true) 
listtmp <- var(y)
mylist <- append(mylist, list(listtmp))
}
hist(unlist(mylist))

trials<-1:100
#Plot of the distribution for completely simulated data
plot(trials,dbinom(trials, size=100, prob=.5), type='h')
plot(trials,dbetabinom(trials, size=100, prob=.5), type='h')



################# BETA BINOMIAL ##############

# Setup
library(stats)
# toeplitz
n <- 100
p <- 200
rho <- 0
level<-0.05 #17/02/23 VK, setting significance level only once
Cov <- toeplitz(rho ^ (seq(0, p - 1)))
sel.index <- c(1, 5, 10, 15, 20)
ind <- sel.index
beta <- rep(0, p)
beta[sel.index] <-2
sparsity <- length(sel.index) # 17/02/23 VK, changed so that value automatically updates
#set.seed(42) # to make different methods comparable, fix the x-matrix
x <- mvrnorm(n, rep(0, p), Cov)
print (x[1,1])
# should create the right x on D-MATH server, x[1 ,1] = 0.958
xb <- x %*% beta


#Option 1: sample p from a beta distribution using the data at hand
p.true<-pbeta(xb, 100, 100, ncp = 0)
y <- rbinom(n,1,p.true) 

trials<-1:1000
mylist <- list()
for(i in 1:length(trials)){
  y <- rbinom(n,1,p.true)  
  listtmp <- mean(y)
  mylist <- append(mylist, list(listtmp))
}

hist(unlist(mylist))

#Option 2: use the beta binomial function
require('VGAM')
p.true <- exp(xb) / (1 + exp(xb))
mean(p.true)
y<-rbetabinom(n, 1, p.true, rho = 0.8)

trials<-1:100
mylist <- list()
for(i in 1:length(trials)){
  y<-rbetabinom(n, 1, p.true, rho=0.9) 
  listtmp <- var(y)
  mylist <- append(mylist, list(listtmp))
}

hist(unlist(mylist))


#option 3. use rbetabinomial.ab

y<- rbetabinom.ab(n, 1, shape1, shape2, limit.prob = p.true,.dontuse.prob = NULL)



mean(y1)
mean(y2)
mean(y3)
mean(y4)








################################

#--- Logit Link ---#
n <- 100
p <- 200
rho <- 0
level<-0.05 
Cov <- toeplitz(rho ^ (seq(0, p - 1)))
sel.index <- c(1, 5, 10, 15, 20)
ind <- sel.index
beta <- rep(0, p)
beta[sel.index] <- 2
sparsity <- length(sel.index) 
set.seed(42) 
x <- mvrnorm(n, rep(0, p), Cov)
xb <- x %*% beta

p.true <- exp(xb) / (1 + exp(xb)) #logit link

# Creating a binary response
ylim <- runif(n)
y <- rep(0, n)
y[ylim < p.true] <- 1



#--- CLogLog Link ---#
n <- 100
p <- 200
rho <- 0
level<-0.05 
Cov <- toeplitz(rho ^ (seq(0, p - 1)))
sel.index <- c(1, 5, 10, 15, 20)
ind <- sel.index
beta <- rep(0, p)
beta[sel.index] <- 2
sparsity <- length(sel.index) 
set.seed(42) 
x <- mvrnorm(n, rep(0, p), Cov)
xb <- x %*% beta

p.true <- 1 - exp(-exp(xb)) #cloglog link

# Creating a binary response
ylim <- runif(n)
y <- rep(0, n)
y[ylim < p.true] <- 1
