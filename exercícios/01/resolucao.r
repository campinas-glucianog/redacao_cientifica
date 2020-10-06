# ------------------------------ EX1 - Part 1 ------------------------------ #
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

cat("Mediana das pessoas diab�ticas: ", median(bpYes))
cat("Mediana das pessoas n�o diab�ticas: ", median(bpNo))

if(median(bpYes) < median(bpNo)){
  cat("Pessoas n�o diab�ticas tem maior press�o.")
}else{
  cat("Pessoas diab�ticas t�m maior press�o.")
}


t.test(bpNo,bpYes)
wilcox.test(bpNo,bpYes)

# ------------------------------ EX1 - Part 2 ------------------------------ #

ex1Paired <- read.csv("D:/dbs/MO430/ex1-paired.csv")

Aug <- ex1Paired$August
Nov <- ex1Paired$November

t.test(Aug, Nov, paired = TRUE)
wilcox.test(Aug, Nov, paired = TRUE)

cat("A m�dia do m�s de Agosto �: ",mean(Aug))
cat("A m�dia do m�s de Novembro �: ",mean(Nov))
cat("A mediana do m�s de Agosto �: ",median(Aug))
cat("A mediana do m�s de Novembro �: ",median(Nov))

if(mean(Aug) < mean(Nov)){
  cat("A maior m�dia � do  m�s Novembro com valor: ",mean(Nov))
}else{
  cat("A maior m�dia � do  m�s Agosto com valor: ",mean(Aug))
}
if(median(Aug) < median(Nov)){
  cat("A maior mediana � do  m�s Novembro com valor: ",mean(Nov))
}else{
  cat("A maior mediana � do  m�s Agosto com valor: ",mean(Aug))
}

# ------------------------------ EX1 - Part 3 ------------------------------ #

# ----- Gerar conjuntos com n = 30, x = 10 e 12 e sd =5  ----- #
set.seed(1234)
fonteA <- rnorm(n = 30, mean = 10, sd = 5)
fonteB <- rnorm(n = 30, mean = 12, sd = 5)


# ------------------------------------------------------------ #
# ------------------ UTILIZANDO O T-TEST --------------------- #
# ------------------------------------------------------------ #
# ----------- M�dia de p valor para 50 repeti��es  ----------- #
pVal <- array()

for(i in 1:50) {
  fonteA <- rnorm(n = 30, mean = 10, sd = 5)
  fonteB <- rnorm(n = 30, mean = 12, sd = 5)
  pVal[i] <- t.test(fonteA, fonteB)$p.value
}

cat("M�dia do valor de p-value:", mean(pVal))

# ------ M�dia de p valor para 50 repeti��es com n = 60 ------ #

for(i in 1:50) {
  fonteA <- rnorm(n = 60, mean = 10, sd = 5)
  fonteB <- rnorm(n = 60, mean = 12, sd = 5)
  pVal[i] <- t.test(fonteA, fonteB)$p.value
}

cat("M�dia do valor de p-value para n = 60:", mean(pVal))

# ----- M�dia de p valor para 50 repeti��es desvio de 10 ----- #

for(i in 1:50) {
  fonteA <- rnorm(n = 30, mean = 10, sd = 10)
  fonteB <- rnorm(n = 30, mean = 12, sd = 10)
  pVal[i] <- t.test(fonteA, fonteB)$p.value
}

cat("M�dia do valor de p-value para desvio = 10:", mean(pVal))

# ---- M�dia de p valor para 50 repeti��es m�dia 10 e 15 ---- #

for(i in 1:50) {
  fonteA <- rnorm(n = 30, mean = 10, sd = 5)
  fonteB <- rnorm(n = 30, mean = 15, sd = 5)
  pVal[i] <- t.test(fonteA, fonteB)$p.value
}

cat("M�dia do valor de p-value para m�dias = 10 e 15:", mean(pVal))

# ------------------------------------------------------------ #
# ------------ UTILIZANDO O WILCOXON RANK-SUM  --------------- #
# ------------------------------------------------------------ #
# ----------- M�dia de p valor para 50 repeti��es  ----------- #
pVal <- array()

for(i in 1:50) {
  fonteA <- rnorm(n = 30, mean = 10, sd = 5)
  fonteB <- rnorm(n = 30, mean = 12, sd = 5)
  pVal[i] <- wilcox.test(fonteA, fonteB)$p.value
}

cat("M�dia do valor de p-value:", mean(pVal))

# ------ M�dia de p valor para 50 repeti��es com n = 60 ------ #

for(i in 1:50) {
  fonteA <- rnorm(n = 60, mean = 10, sd = 5)
  fonteB <- rnorm(n = 60, mean = 12, sd = 5)
  pVal[i] <- wilcox.test(fonteA, fonteB)$p.value
}

cat("M�dia do valor de p-value para n = 60:", mean(pVal))

# ----- M�dia de p valor para 50 repeti��es desvio de 10 ----- #

for(i in 1:50) {
  fonteA <- rnorm(n = 30, mean = 10, sd = 10)
  fonteB <- rnorm(n = 30, mean = 12, sd = 10)
  pVal[i] <- wilcox.test(fonteA, fonteB)$p.value
}

cat("M�dia do valor de p-value para desvio = 10:", mean(pVal))

# ---- M�dia de p valor para 50 repeti��es m�dia 10 e 15 ---- #

for(i in 1:50) {
  fonteA <- rnorm(n = 30, mean = 10, sd = 5)
  fonteB <- rnorm(n = 30, mean = 15, sd = 5)
  pVal[i] <- wilcox.test(fonteA, fonteB)$p.value
}

cat("M�dia do valor de p-value para m�dias = 10 e 15:", mean(pVal))