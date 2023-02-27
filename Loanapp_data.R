require(wooldridge)
data('loanapp')

# Data previously used here: https://www.researchgate.net/profile/William-Hunter-17/publication/5151431_The_Cultural_Gap_Hypothesis_and_Mortgage_Lending_Decisions/links/00b4951fa9f11a10f7000000/The-Cultural-Gap-Hypothesis-and-Mortgage-Lending-Decisions.pdf
# 
# They say that the original data is dirty (missing values, errors).
# The original data has 3,300+722 observations.
# They explain criterions for cleaning the data in section 2.
# 
# Also used here: https://www.jstor.org/stable/pdf/2118254.pdf?casa_token=Bx8AAZ9Rkv0AAAAA:OQquj6at0UQXJiXiOabnTtSM1sW3ziLMNqYzSEIXNCh-IvhVcxk5u1BzL4spW9pkDtbLzXs7GSg31JKjjDKdmEUtoQu19rcFfpSgOAmqour85hxLg1A
# 
# Information about the data is found in section 3.


sum(is.na(loanapp)) #230

#for now removing missing data 

#1989

loanapp<-na.omit(loanapp) #211 obs dropped
#not looking for extreme observations ect. right now 


#------------------------- A D D I N G     N O I S E --------------------------#
#(most papers suggest adding noise to Y or X, this could actually improve the selection process)

# OPTION 1: just randomly creating variables with different ranges


#just adding some columns by generating random numbers 
n<-dim(loanapp)[[1]]
p<-dim(loanapp)[[2]]


#5 columns with big numbers (-1000, 1000)
noise1<-data.frame(replicate(5,sample(-1000:1000,n,rep=TRUE)))
noise1<-data.frame(lapply(noise1, jitter))

#5 columns with number between -200 and 200
noise2<-data.frame(replicate(5,sample(-200:200,n,rep=TRUE)))
noise2<-data.frame(lapply(noise2, jitter))

#5 columns with numbers between 0 and 10
noise3<-data.frame(replicate(5,sample(0:10,n,rep=TRUE)))
noise3<-data.frame(lapply(noise3, jitter))

#5 binary columns 
noise4<-data.frame(replicate(5,sample(0:1,n,rep=TRUE)))


#new data
FullData<-cbind(loanapp,noise1, noise2, noise3, noise4)
rm(noise1, noise2, noise3, noise4)

# OPTION 2: adding Gaussian columns while the mean and sd are different 

noise<-data.frame(replicate(20, rnorm(n, mean=sample(2:10,1), sd=sample(4:10,1))))



#(the next part is without noise so far)

#--------------------------- Synthetic Y - Gaussian----------------------------#

#Using the procedure on riboflavin as example

loanapp.tmp <- t(loanapp) #here the first row is not excluded since its just the occupancy
colnames(loanapp.tmp) <- seq(1:n)
x<-loanapp.tmp # unclear why they remove the first row again x <- loanapp.tmp[, -1]
rm(loanapp)
rm(loanapp.tmp)
n <- dim(x)[1]
p <- dim(x)[2]
report.sigma <- FALSE
SNR <- 16
sparsity <- 2 # 4 in other set-up

# if the active variables should change for every simulation run, put the following in the loop
beta <- rep(0, p)
ind <- sample(1:p, sparsity)
beta[ind] <- 1
y.true <- x %*% beta
sigma <- sqrt(drop(var(y.true)) / SNR)

# sigma explanation:
# This can result in very different signal strength depending on the correlation 
# between the 2 variables, we fix the SNR on a per run basis by always adjusting σ 
# such that Var(Xβ)/ σ2 = 16. Here, Var(Xβ) denotes the empirical variance of
# the true underlying signal




#--------------------------- Synthetic Y - Binomial ----------------------------#


loanapp.tmp <- t(loanapp) #here the first row is not excluded since its just the occupancy
colnames(loanapp.tmp) <- seq(1:n)
x<-loanapp.tmp # unclear why they remove the first row again x <- loanapp.tmp[, -1]
rm(loanapp)
rm(loanapp.tmp)
n <- dim(x)[1]
p <- dim(x)[2]
# report.sigma <- FALSE
sparsity <- 2 # 4 in other set-up


# if the active variables should change for every simulation run, put the following in the loop
beta <- rep(0, p)
ind <- sample(1:p, sparsity) #if the 
beta[ind] <- 1
xb <- x %*% beta
p.true <- exp(xb) / (1 + exp(xb))

#explanaition: https://www.probabilitycourse.com/chapter13/Chapter_13.pdf
ylim <- runif(n) #runif() function generates random deviates of the uniform distribution and is written as runif(n, min = 0, max = 1) 
y <- rep(0, n)
y[ylim < p.true] <- 1