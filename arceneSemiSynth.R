

#sel.index <- c(1, 2, 5, 10, 15, 20)
#ind <- sel.index
#sparsity <- length(sel.index)
riboflavin <- read.table("arcene_train.data",
                         stringsAsFactors = FALSE)
#riboflavin.tmp <- t(riboflavin[, -1])
#colnames(riboflavin.tmp) <- riboflavin[, 1]
x <- riboflavin
rm(riboflavin)
#rm(riboflavin.tmp)
n <- dim(x)[1]
p <- dim(x)[2]
report.sigma <- FALSE
SNR <- 16
sparsity <- 6 # 4 in other set-up

beta <- rep(0, p)
ind <- sample(1:p, sparsity)
beta[ind] <- 1
xb <- as.matrix(x) %*% beta
p.true <- exp(xb) / (1 + exp(xb))
sigma <- sqrt(drop(var(p.true)) / SNR)

ylim <- runif(n) #runif() function generates random deviates of the uniform distribution and is written as runif(n, min = 0, max = 1) 
y <- rep(0, n)
y[ylim < p.true] <- 1



