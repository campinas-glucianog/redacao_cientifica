# ------------------------------ EX3 - Part 1 ------------------------------ #
database <- read.csv("D:/dbs/MO430/ex1.csv")

database$type <- sapply(database$type, as.character)
database[which(database$type == 'No'),"type"] <- 0
database[which(database$type == 'Yes'),"type"] <- 1
database$type <- sapply(database$type, as.numeric)

bpNo <- array()
bpYes <- array()
y <- 1
z <- 1


for(i  in 1:length(database$type)){
  if(database$type[i] == 0){
    bpNo[y] <- database$bp[i]
    y <- y+1
  }else{
    bpYes[z] <- database$bp[i]
    z <- z+1
  }
}

# ---------------------- AMOSTRAS NÃO PAREADAS ---------------------- #
library(BEST)
bayesianCalcs = BESTmcmc(bpYes,bpNo)
summary(bayesianCalcs)


summary(bayesianCalcs, ROPEm=c(-4,4))



# ------------------------ AMOSTRAS PAREADAS ------------------------ #

database <- read.csv("D:/dbs/MO430/ex1-paired.csv")

Aug <- database$August
Nov <- database$November

library(BEST)
bayesianCalcs = BESTmcmc(Nov-Aug)
summary(bayesianCalcs)
summary(bayesianCalcs, ROPEm=c(-3.5,3.5))

