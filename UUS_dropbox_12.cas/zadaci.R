
# 51.

mk<-c(45,30,15,10)
n_pj <- c(1/2,1/4,1/8,1-7/8)*100 # provera da su svi n*pj>=5
n_pj
t_stat <- sum((mk-n_pj)^2/n_pj) # test statistika
t_stat
qchisq(1-0.05,4-1)


# 52.

sredine <- c(2.5, 7.5, 12.5, 17.5, 22.5)
mk <- c(15, 75, 100, 50, 20)
uzorak_sredina <- c()
for(i in 1:5) uzorak_sredina <- c(uzorak_sredina, rep(sredine[i], mk[i]))
# provera:
uzorak_sredina
# tražene ocene:
mean(uzorak_sredina) # uzoračka sredina
n <- length(uzorak_sredina)
var(uzorak_sredina)*(n-1)/n # uzoračka disperzija

# ocenjujemo pj: 
pnorm((5-12.2)/5.04) 
pnorm((10-12.2)/5.04)-pnorm((5-12.2)/5.04)
pnorm((15-12.2)/5.04)-pnorm((10-12.2)/5.04)
pnorm((20-12.2)/5.04)-pnorm((15-12.2)/5.04)
1-pnorm((20-12.2)/5)
# zaokružicemo sve vrednosti na 2 decimale radi jednostavnosti

pj <- c(0.08, 0.25,0.38,0.23,0.06)
mk <- c(15,75,100,50,20)
n <- sum(mk)
# provera da je n*pj >= 5 za sve j iz {1,2,3,4,5}
n_pj <- n*pj
n_pj

t_stat <- sum((mk-n_pj)^2/(n_pj)) # test statistika
t_stat
qchisq(1-0.05,5-1-2)



# 54.

Mij <- matrix(c(30,30,5,20,10,5), nrow = 2, byrow = T)
n <- 100
p_pol <- c(0.65, 0.35)
p_marka <- c(0.5,0.4,0.1)
t_stat <-0
for(i in 1:2){
  for(j in 1:3){
    t_stat <- t_stat+(Mij[i,j]-n*p_pol[i]*p_marka[j])^2/(n*p_pol[i]*p_marka[j])
  }
}
t_stat # test statistika

qchisq(1-0.01,(2-1)*(3-1))
