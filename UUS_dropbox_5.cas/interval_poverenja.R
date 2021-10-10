# 95%- interval poverenja za m kod N(m,sigma^2) kad je simga^2 poznato
# simulacija intervala je izvrsena za N(2,1) po formuli koju smo pokazali u 
# prvom zadatku.
int <- c()
j = 1
alpha <- 0.05 # = 1-0.95
q <- qnorm(1-alpha/2) # ovo je nama bilo c, ali nije preporucljivo da sa c
# nazovemo neku promenljivu, jer je c rezervisano za funkciju c()
for(i in 1:200){ # simuliramo 100 intervala na osnovu psu obima 1000 iz N(2,1)
  if(i%%2){
    x_sr <- mean(rnorm(1000,2,1))
    int[i] <- x_sr 
    int[i+1] <- x_sr 
  }
}
int[seq(1,199,2)] <- int[seq(1,199,2)] - q/sqrt(1000)
int[seq(2,200,2)] <- int[seq(2,200,2)] + q/sqrt(1000)


j<-1
n <- c()
for(i in 1:200){
  if(i %% 2){
    n[i] <- j
    n[i+1] <- j
    j <- j+1
  }
}
n
boje <- rep("black",200)
j <- 1
for(i in 1:200){
  if(i%%2){
    boje[j] <- ifelse((int[i]<2)&(2<int[i+1]), "black", "red")
    # tacke krajeva intervala koji nisu obuhvatili 2 bojimo u crveno, ostale u crno
    boje[j+1] <- boje[j]
    j <- j + 2
  }
}
boje
plot(int~n,pch=19,col = boje)
abline(h=2, lwd = 1.5, col = "green")
text(int~n, labels = n, pos = 4) # zumirati sliku da se bolje vidi!
