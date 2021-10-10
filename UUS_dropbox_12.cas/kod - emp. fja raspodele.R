
par(mfrow = c(2,2)) # crtamo po dve slike u dva reda

uzorak <- rexp(10) # vadimo PSU obima 10 iz exp(1) raspodele
# funkcija ecdf() računa empirijsku funkciju raspodele na osnovu prosleđenog uzorka
?ecdf()
plot(ecdf(uzorak), main = "n = 10") # crtamo empirijsku funkciju raspodele
x = seq(0,max(uzorak)+1, by = (max(uzorak)+1)/100) # tačke u kojima ćemo računati
# pravu funkciju raspodele
y = sapply(x, function(x) 1-exp(-1*x)) # u tačkama vektora x računamo funkciju 
# exp(1) raspodele
lines(x,y) # dodajemo tačke (x,y) na plot 

# zatim povećavamo obim PSU i ponavljamo prethodne korake:

uzorak <- rexp(30)
plot(ecdf(uzorak), main = "n = 30")
x = seq(0,max(uzorak)+1, by = (max(uzorak)+1)/100)
y = sapply(x, function(x) 1-exp(-1*x))
lines(x,y)

uzorak <- rexp(50)
plot(ecdf(uzorak), main = "n = 50")
x = seq(0,max(uzorak)+1, by = (max(uzorak)+1)/100)
y = sapply(x, function(x) 1-exp(-1*x))
lines(x,y)

uzorak <- rexp(100)
plot(ecdf(uzorak), main = "n = 100")
x = seq(0,max(uzorak)+1, by = (max(uzorak)+1)/100)
y = sapply(x, function(x) 1-exp(-1*x))
lines(x,y)

# vidimo da se grafik empirijske i prave funkcije raspodele sve više približavaju
# jedan drugom kako je obim uzorka rastao.


# 53.

par(mfrow = c(1,1))
uzorak <- c(0.59, 0.72, 0.47, 0.43, 0.31, 0.56, 0.22, 0.90, 
            0.96, 0.78, 0.66, 0.18, 0.73, 0.43, 0.58, 0.11)
plot(ecdf(uzorak))

x = seq(0,1, by = 1/100)
y = sapply(x, function(x) x)
lines(x,y)

# rastojanje sledeće dve crvene tačke je supremum apsolutne razlike F_0 i F_n
points(x = c(0.43,0.43), y = c(0.43, 4/16), col = "red", pch = 20)
points(x = c(0.43,0.43), y = c(0.43, 4/16), col = "red", type = "l")
