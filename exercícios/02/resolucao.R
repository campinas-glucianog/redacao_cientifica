# ------------------------------ EX2 - Part 1 ------------------------------ #
ex1 <- read.csv("D:/dbs/MO430/ex1.csv")

ex1$type <- sapply(ex1$type, as.character)
ex1[which(ex1$type == 'No'),"type"] <- 0
ex1[which(ex1$type == 'Yes'),"type"] <- 1
ex1$type <- sapply(ex1$type, as.numeric)

bpNo <- array()
bpYes <- array()
y <- 1
z <- 1


for(i  in 1:length(ex1$type)){
  if(ex1$type[i] == 0){
    bpNo[y] <- ex1$bp[i]
    y <- y+1
  }else{
    bpYes[z] <- ex1$bp[i]
    z <- z+1
  }
}


# --------------------------- CALCULO DO P VALOR --------------------------- #
t.test(bpNo, bpYes)$p.value
wilcox.test(bpNo, bpYes)$p.value


# ---------------------------- T TESTE REVERSO ----------------------------- #
t.test(bpNo)$conf.int
t.test(bpYes)$conf.int

# ----------------------- WILCOXON RANK SUM REVERSO ------------------------ #

wilcox.test(bpNo, conf.int=T)$conf.int
wilcox.test(bpYes, conf.int=T)$conf.int


# ------------------------------- BOOTSTRAP -------------------------------- #
library(boot)
auxf <- function(dado,indice){
  return(mean(dado[indice]))
}

bbNo = boot(bpNo,R=5000, statistic=auxf)
boot.ci(bbNo,type="bca")

bbYes = boot(bpYes,R=5000, statistic=auxf)
boot.ci(bbYes,type="bca")


# ------------------------------ EX2 - Part 2 ------------------------------ #

# -------------------------------- COHEN D --------------------------------- #
library(effsize)
cohen.d(bpNo, bpYes, pooled=TRUE)
cohen.d(bpYes, bpNo, pooled=TRUE)




