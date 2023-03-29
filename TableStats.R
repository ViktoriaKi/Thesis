

library(BBmisc)
library(readxl)
library(stringr)

setwd('C:/Users/viki1/Documents/Uni/Thesis/Thesis/simulation_setups/multi_carve')

calculatePower <- function(subres, sparsity) {
  names<-c("carve5", "carvefw5", "split5", "splitfw5", "carve30",
           "carvefw30", "split30", "splitfw30", "carvefix5",
           "carvefwfix5", "splitfix5", "splitfwfix5", "carvefix30",
           "carvefwfix30", "splitfix30", "splitfwfix30")
  allrej <- matrix(NA, nrow = 1,ncol = length(names))
  fwer <- numeric()
  colnames(allrej) <- names
  for (name in names) {
    nameind <- which(colnames(subres) == name)
    mat <- subres[, nameind]
    fwer[as.character(name)] <- mean(mat[,sparsity + 1] < 0.05, na.rm = TRUE)
    rejmat <- mat[, 1:sparsity] < 0.05
    allrej[, as.character(name)] <- mean(rejmat, na.rm = TRUE)
  }
  return(list(allrej, fwer))
}

calculatePScreen <- function(subres, sparsity) {
  names<-c("carve5", "carvefw5", "split5", "splitfw5", "carve30",
           "carvefw30", "split30", "splitfw30", "carvefix5",
           "carvefwfix5", "splitfix5", "splitfwfix5", "carvefix30",
           "carvefwfix30", "splitfix30", "splitfwfix30")
  allrej <- matrix(NA, nrow = 1,ncol = length(names))
  colnames(allrej) <- names
  for (name in names) {
    nameind <- which(colnames(subres) == name)
    mat <- subres[, nameind]
    
    binMat <- mat[, 1:sparsity] < 0.05
    pscreen <- mean(rowMeans(binMat) == 1)
    allrej[, as.character(name)] <- pscreen
  }
  return(allrej)
}

calculateExpectations <- function(subres, sparsity) {
  names<-c("carve5", "carvefw5", "split5", "splitfw5", "carve30",
           "carvefw30", "split30", "splitfw30", "carvefix5",
           "carvefwfix5", "splitfix5", "splitfwfix5", "carvefix30",
           "carvefwfix30", "splitfix30", "splitfwfix30")
  EV <- matrix(NA, nrow = 1, ncol = length(names))
  colnames(EV) <- names
  ERV <- matrix(NA, nrow = 1, ncol = length(names))
  colnames(ERV) <- names
  for (name in names) {
    nameind <- which(colnames(subres) == name)
    mat <- subres[, nameind]
    
    binMat <- mat[, 1:sparsity] < 0.05
    pscreen <- mean(rowMeans(binMat) == 1)
    EV[, as.character(name)] <- pscreen
  }
  return(list(EV, ERV))
}

simDf <- read_excel('simulations.xlsx')[1,]
simDf$Time <-str_pad(simDf$Time, width=5, side="left", pad="0")

mainFunc <- function() {
  B.vec <- c(5, 10, 20, 50) # number of splits
  frac.vec <- c(0.5,0.75, 0.8, 0.9 , 0.99) # selection fraction
  sparsity <- 5
  intCols <- c('carve5', 'split5', 'carve30', 'split30')
  outCols <- c('pScreen', 'EV', 'ERV', 'FWER', 'FDR', 'Power', 'Level')
  finMat <- matrix(NA, nrow = (length(B.vec) * length(frac.vec) * length(intCols)), ncol = length(outCols))
  colnames(finMat) <- outCols
  for (B in B.vec) {
    for (frac in frac.vec) {
      for (row in 1:nrow(simDf)) {
        curDat <- tryCatch(expr = {
          date <- format(as.Date(toString(simDf[row, 1]), format = "%d.%m.%y"), "%d-%b-%Y")
          time <- toString(simDf[row, 2])
          folder <- paste0('./Binomial_', date, ' ', time, '/')
          # file <- list.files(folder, pattern=paste0('results.* split=0.5 B=',B,' seed=.*'))
          file <- list.files(folder, pattern=paste0('results.* split=', frac, ' B=',B,' seed=.*'))
          curDat <- load2(paste0(folder, file))$results
          #return(curDat)
        },
        error = function(e) {
          date <- format(as.Date(toString(simDf[row, 1]), format = "%d.%m.%y"), "%d-%b-%Y")
          date <- gsub("Mar", "Mrz", date)
          time <- toString(simDf[row, 2])
          folder <- paste0('./Binomial_', date, ' ', time, '/')
          file <- list.files(folder, pattern=paste0('results.* split=', frac, ' B=',B,' seed=.*'))
          # file <- list.files(folder, pattern=paste0('results.* split=', frac, ' B=',B,' seed=.*'))
          curDat <- load2(paste0(folder, file))$results
          return(curDat)
        })
        retList <- calculatePower(curDat, sparsity)
        power <- retList[[1]]
        fwer <- retList[[2]]
        
        # create dummy matrix to load
        curMat <- matrix(NA, nrow = 4, ncol = ncol(finMat))
        rownames(curMat) <- c(paste0('carve_gam5_B=', B, 'select=', frac), paste0('split_gam5_B=', B, 'select=', frac),
                              paste0('carve_gam30_B=', B, 'select=', frac), paste0('split_gam30_B=', B, 'select=', frac))
        
        # load power
        curMat[1, 6] <- power[1]
        curMat[2, 6] <- power[3]
        curMat[3, 6] <- power[5]
        curMat[4, 6] <- power[8]
        
        # load fwer
        curMat[1, 4] <- fwer[1]
        curMat[2, 4] <- fwer[3]
        curMat[3, 4] <- fwer[5]
        curMat[4, 4] <- fwer[8]
        
        # calc p screen
        pscreen <- calculatePScreen(curDat, sparsity)
        
        curMat[1, 1] <- pscreen[1]
        curMat[2, 1] <- pscreen[3]
        curMat[3, 1] <- pscreen[5]
        curMat[4, 1] <- pscreen[8]
        
        # capture EV and ERV
        #???
        
        # capture level
        # ????
        
        finMat <- rbind(finMat, curMat)
      }
    }
  }
  return(finMat)
}

finalMat <- mainFunc()

