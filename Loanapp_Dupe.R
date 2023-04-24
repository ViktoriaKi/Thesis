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


#not looking for extreme observations ect. right now 


#------------- Preparing the Data -------------#
#many variables are conencted with each other 

#if action ==3 -> reject, if action =1 / 2 -> approve 
dim(loanapp)

#creating one variable from reject and approve - label
loanapp$label<- ifelse(loanapp$approve==1, 1, 0)

unique(loanapp$msa)
#there is only one value for msa! We can drop that variable 

#removing action
loanapp<-loanapp[ , -which(names(loanapp) %in% c("action","reject", "approve", "msa"))]

loanapp$gdlin<-ifelse(loanapp$gdlin==666, NA, loanapp$gdlin)

loanapp<-na.omit(loanapp) 

sum(loanapp[rowSums(loanapp[,c("black", "white", "hispan")]) == 0,])
#meaning that race is already one hot encoded


#--------- Exploring distributions and extreme values ---------#
attach(loanapp)
table(occ)
table(suffolk)
table(typur)
table(unit)
table(married)
table(dep)
table(emp)
table(yjob)
table(self)
table(rep)
table(gdlin)
table(mortg)
table(cons)
table(pubrec)
table(fixadj)
table(prop)
table(inss)
table(inson)
table(gift)
table(cosign)
table(unver)
table(min30)
table(bd)
table(mi)
table(old)
table(vr)
table(sch)
table(black)
table(hispan)
table(male)
table(mortno)
table(mortperf)
table(mortlat1)
table(mortlat2)
table(chist)
table(multi)
table(thick)
table(white)
table(label)

#--
par("mfcol"=c(1, 1))
hist(loanamt, col="blue")
sum(review>=800)
hist(atotinc, col="green")
hist(cototinc, col="red") 
hist(hexp, col="blue")
hist(other, col="green")
hist(liq, col="red") 
hist(lines, col="blue")
hist(hrat, col="green")
hist(obrat, col="red") 
hist(term, col="red") 
hist(apr, col="blue")
hist(review, col="green")
hist(netw, col="red")
hist(unem, col="blue")
hist(loanprc, col="green")



detach(loanapp)





#Seeing how many unique values each variable has
apply(loanapp, 2, function(x) length(unique(x)))
# occ, typur, gdlin, mortg, cons, prop 
# for mortg and cons we are assuming that they are still encoded based on 
# "Mortgage Lending in Boston: Interpreting HMDA Data"

#I am assuming that gdlin==666 means missing. These two observations will be dropped
unique(loanapp$prop)
unique(loanapp$typur)
table(loanapp$mortg)

#--------- 1. One Hot Encoding ---------#

 library(dplyr)
# drop.cols<-c("occ", "typur", "gdlin", "mortg","cons", "prop", "label") 
# #label is also removed to exclude from standardization and interactions
# loanapp<-loanapp %>% 
#   mutate(occ1 = ifelse(occ==1, 1, 0),
#          occ2 = ifelse(occ==2, 1, 0),
#          occ3 = ifelse(occ==3, 1, 0),
#          typur0 = ifelse(typur==0, 1, 0),
#          typur1 = ifelse(typur==1, 1, 0),
#          typur3 = ifelse(typur==3, 1, 0),
#          typur5 = ifelse(typur==5, 1, 0),
#          typur6 = ifelse(typur==6, 1, 0),
#          typur7 = ifelse(typur==7, 1, 0),
#          typur8 = ifelse(typur==8, 1, 0),
#          typur9 = ifelse(typur==9, 1, 0),
#          mortg1 = ifelse(mortg==1, 1, 0),
#          mortg2 = ifelse(mortg==2, 1, 0),
#          mortg3 = ifelse(mortg==3, 1, 0),
#          mortg3 = ifelse(mortg==3, 1, 0),
#          cons1 = ifelse(cons==1, 1, 0),
#          cons2 = ifelse(cons==2, 1, 0),
#          cons3 = ifelse(cons==3, 1, 0),
#          cons4 = ifelse(cons==4, 1, 0),
#          cons5 = ifelse(cons==5, 1, 0),
#          cons6 = ifelse(cons==6, 1, 0),
#          prop1 = ifelse(prop==1, 1, 0),
#          prop2 = ifelse(prop==2, 1, 0),
#          prop3 = ifelse(prop==3, 1, 0)) %>% select(-one_of(drop.cols))


loanapp$race<-0
loanapp$race<-ifelse(loanapp$black==1, "black",loanapp$race)
loanapp$race<-ifelse(loanapp$hispan==1, "hispan",loanapp$race)
loanapp$race<-ifelse(loanapp$white==1, "white",loanapp$race)
loanapp$race<-as.factor(loanapp$race)
#loanapp$prop<-as.factor(loanapp$prop)


cols <- c("occ", "typur", "gdlin", "mortg","cons", "prop", "label",
          "thick", "suffolk", "married", "self", "pubrec", "fixadj", "inson", 
          "gift", "cosign", "unver", "min30", "bd", "mi", "old", "vr", "sch", "male")
loanapp[cols] <- lapply(loanapp[cols], factor)  ## as.factor() could also be used


#some of them I removed because I made factors out of them, some I removed cause of VIF
drop.cols<-c("black", "hispan", "white", "mortno", "mortperf", "mortlat1", "prop", "hexp", "loanamt",
             "mortlat2", "multi", "loanprc", "thick", "inss", "multi", "apr", "chist", "emp", "atotinc", "cons") #adding white since lin comb. of black and hisp 


loanapp<- loanapp%>% select(-one_of(drop.cols))




#--------- 2. Normalizing ---------#
colsNoNorm<-c("label","occ", "typur", "gdlin", "mortg", "label",
               "suffolk", "married", "self", "pubrec", "fixadj", "inson", 
              "gift", "cosign", "unver", "min30", "bd", "mi", "old", "vr", "sch", "male", "race")

loanapp.norm<-loanapp[ , -which(names(loanapp) %in% colsNoNorm)]

loanapp.norm<-scale(loanapp.norm)
loanapp.norm<-as.data.frame(loanapp.norm)




#--------- 3. Interactions & Polynomials ---------#

p<-dim(loanapp.norm)[[2]]
library(eList)
loanapp.inter<-DF(for(i in loanapp.norm[1:p]) for(j in loanapp.norm[1:p]) i*j)

loanapp.inter<-cbind(loanapp[,colsNoNorm] , loanapp.inter)
loanapp.inter <- sapply(loanapp.inter, as.numeric)
save(loanapp.inter, file = "loanappInter.Rdata")

#--------------------------- Synthetic Y - Binomial ----------------------------#
# 
# 
# loanapp.tmp <- t(loanapp) #here the first row is not excluded since its just the occupancy
# colnames(loanapp.tmp) <- seq(1:n)
# x<-loanapp.tmp # unclear why they remove the first row again x <- loanapp.tmp[, -1]
# rm(loanapp)
# rm(loanapp.tmp)
# n <- dim(x)[1]
# p <- dim(x)[2]
# # report.sigma <- FALSE
# sparsity <- 2 # 4 in other set-up
# 
# 
# # if the active variables should change for every simulation run, put the following in the loop
# beta <- rep(0, p)
# ind <- sample(1:p, sparsity) #if the 
# beta[ind] <- 1
# xb <- x %*% beta
# p.true <- exp(xb) / (1 + exp(xb))
# 
# #explanaition: https://www.probabilitycourse.com/chapter13/Chapter_13.pdf
# ylim <- runif(n) #runif() function generates random deviates of the uniform distribution and is written as runif(n, min = 0, max = 1) 
# y <- rep(0, n)
# y[ylim < p.true] <- 1 

