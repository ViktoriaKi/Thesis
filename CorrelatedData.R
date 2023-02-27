# B LO C K   C O R R E L A T I O N S

#VK 25/02/23
require(partition)
require(MASS)

n <- 50
p <- 100

sel.index <- c(1, 2, 5, 10, 15, 20)
ind <- sel.index
beta <- rep(0, p)
beta[sel.index] <- 1
sparsity <- length(sel.index)


# create a 100 x 15 data set with 3 blocks
xc <- simulate_block_data(
  # create 3 correlated blocks of 5 features each
  block_sizes = rep(20, 5),
  lower_corr = .4,
  upper_corr = .6,
  n = 50
)
xc<-as.matrix(xc)

ggcorrplot::ggcorrplot(corr(xc))

y.true <- xc %*% beta
sigma<-2
y <- y.true + sigma * rnorm(n)




# Correlating using rho 



n <- 100
p <- 200
rho <- 0.9
level<-0.05 #17/02/23 VK, setting significance level only once
Cov <- toeplitz(rho ^ (seq(0, p - 1)))
sel.index <- c(1, 2, 5, 10, 15, 20)
ind <- sel.index
beta <- rep(0, p)
beta[sel.index] <- 1
sparsity <- length(sel.index) # 17/02/23 VK, changed so that value automatically updates
set.seed(42) # to make different methods comparable, fix the x-matrix
x <- mvrnorm(n, rep(0, p), Cov)

ggcorrplot::ggcorrplot(corr(x))
