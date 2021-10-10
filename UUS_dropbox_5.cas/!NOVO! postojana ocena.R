# Sada cemo za razne vrenosti obima PSU simulirati po 100 vrednosti ocene iz primera postojane
# ocene cija MSE ne tezi nuli . Kao i u nekom ranijem
# primeru, videcemo da kako obim uzorka raste, to postojana ocena sve manje osciluje
# oko prave vrednosti parametra. Simulacija je izvedena za PSU iz N(2,1) raspodele.
# 


n<-seq(10,200,5) # razne vrednosti obima PSU
obim <-as.vector(sapply(n, function(x) rep(x,100))) # odgovara nam da svaku
# vrednost obima ponovimo po 100 puta zbog lakseg plotovanja
ocena <- c() # ovde smestamo vrednosti ocene
for(i in n){
    for(j in 1:100){
      uzorak <- rnorm(i,2,1)
      ocena <- c(ocena, ifelse(runif(1) < (i-1)/i, mean(uzorak), i ))
    }
}

plot(ocena~obim, ylim = c(1,3)) # plotujemo samo one ocene koje su u intervalu (1,3)
abline(h = 2, lwd = 3, col = "green") # Dakle, za kako je obim uzorka veci, to su
# kruzici kojima su predstavljene ocene sve vise zgusnuti oko zelene linije, koja
# je ucrtana u pravoj vrednosti parametra m, sto je kod nas 2.

plot(ocena~obim) # Medjutim, sta se desava kada promenimo skalu vertikalne ose? Tj. kada
# ne posmatramo samo ocene koje su u inervalu (1,3)? Ocene se i dalje grupisu oko 2,
# iako to ne vidimo na ovako 'ruznom' plotu, ali vidimo da poneke ocene uzmu i
# drasticno drugacije vrednosti od 2. Te vrednosti koje daleko odstupaju od prave
# vrednosti parametra su i uticale na to da MSE ove ocene ne tezi 0 kako obim uzorka
# raste, sto smo formalno i dokazali.
