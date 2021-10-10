x_sr <- c() # ovde cemo cuvati vrednosti uzoracke sredine za razlicite vrednosti
# obima PSU
n <- seq(15,1500,5) # razlicite vrednosti obima PSU
n
x_sr <- sapply(n, function(x) mean(rnorm(x))) # vadimo PSU iz std. norm. raspodele
# razlicitog obima, pa racunamo njegovu uzoracku sredinu.
?rnorm() # m i sigma^2 se zadaju kao argumenti mean i sd; po difoltu se koristi
# mean = 0, sd = 1
?sapply() # funkcija sapply() primenjuje funkciju(argument FUN) na elemente vektora
# X i vraca vektor sa vrednostima te funkcije u svakoj tacki

# sledeca petlja je alternativa pozivu sapply()
# for(i in n){
#  x_sr <-c(x_sr, mean(rnorm(i)))
# }

plot(x_sr ~ n)
abline(h=0, col = "red") # 0 je prava vrednost parametra m
# vidimo da kako je obim uzorka rastao, tako je ocena sve manje oscilovala od
# prave vrednosti parametra, tj. od 0

###############################################################################

podaci <- c(-0.8, 0.9, 0.6, -1.2, 1.6, -1.5, 0, 0, 1.1, -1.1,
            -2.2, 0.4, -0.1, -0.8, -0.5, -0.5, -0.1, -1.6, 0.3,
            -1.6, -1.4, -0.3, 1.5, -0.9, 0)
sort(podaci)
n <- length(podaci)
n
k <- ceiling(log(n, base = 2 )) + 1
k
range(podaci)
diff(range(podaci))/k
h <- 0.7
podela <- -2.35 + (0:k)*h # tacke koje su krajevi intervala u kojima zelimo da
# uctramo histogram
podela
hist(podaci, breaks = podela, col = "lightblue", main = "histogram frekvencija")
# oblik hisograma ukazuje na normalnu raspodelu
hist_density<-hist(podaci, breaks = podela, probability = T , 
                   col = "lightblue", main = "histogram gustine")
hist_density$density # ocene gustine
# histogram gustine je istog oblika kao histogram frekvencija, jer je visina 
# stubaca samo podeljena sa n*h:
# visina stubaca kod histograma frekvencije: 1,6,6,7,3,2
c(1,6,6,7,3,2)/(n*h) # ocene gustine

# sada cemo uctrati grafik funkcije gustine normalne raspodelee cije smo parametre
# ocenili na osnovu podataka: ocena za m je uzoracka sredina, a ocena za sigma^2
# je (popravljena) uzoracka disperzija
lines(seq(-3,2, by=0.05), dnorm(seq(-3,2, by=0.05),
                          mean(podaci), var(podaci)), lwd = 2)
# var() vraca popravljenu uzoracku disperziju
?lines()
?dnorm() # vrednosti gustine normalne raspodele sa prosledjenim parametrima

boxplot(podaci, col = "pink") # vidimo da nema autlajera i da je raspodela
# simetricna
fivenum(podaci) # ovako boxplot racuna uzoracke kvartile
?fivenum()
abline(h = fivenum(podaci))
?quantile() # imamo izlistane definicije uzorackih kvantila (videti Types)



podaci <- c(0.1,rep(0.2,7),0.3,0.4,0.5,0.7,0.7,0.8,0.8,0.8,
            1,1.1,1.2,1.2,1.6,1.7,2.4,2.4,2.9)
boxplot(podaci, col = "coral") # asimetricna raspodela (eng. skewed distribution)
# imamo autlajer, u ovom primeru je to tacka maksimuma ovog niza:
abline(h = max(podaci))
n <- length(podaci)
n
k <- ceiling(log(n, base = 2 )) + 1
k
range(podaci)
diff(range(podaci))/k
h <- 0.5
podela <- 0 + (0:k)*h
podela
hist(podaci, breaks = podela, col = "green", probability = T,
     main = "histogram gustine") # histogram nas asocira na eksponencijalnu raspodelu
lines(seq(0,3, by=0.05), dexp(seq(0,3, by=0.05), 1/mean(podaci)), lwd = 2) # ocenicemo
# parametar kao 1/uzoracka sredina


data() # neki skupovi podataka koji postoje u R-u
podaci <- chickwts
podaci
str(podaci) # vidimo strukturu podataka 
boxplot(podaci$weight ~ podaci$feed, col = "lightblue") # crtamo vise boxplotova
# odjednom
# uzi boxplot znaci da je disperzija podataka manja