
#-----------------*************** Lista 6 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 6.1--------------------------------------
#
#_______________________________________________________________________________
# Dla dwóch wybranych spółek giełdowych z pliku Notowania.csv wyznacz
#
# (a) oczekiwane zwroty i odchylenia standardowe zwrotów,
#
# (b) oczekiwane zwroty i odchylenia standardowe zwrotów z portfela 
#     składającego się z tych akcji z wagami
#
#     (0,1), (0.1,0.9), (0.2,0.8), ..., (0.8,0.2),(0.9,0.1), (1,0).
#
#  (c) oczekiwane zwroty i odchylenia standardowe zwrotów z portfela 
#      dopuszczając krótką sprzedaż tj. składającego się z tych akcji z wagami
#
#     (-1.5,2.5), (-1.4,2.4), ..., (1.4,-0.4),(1.5,-0.5).
#
#  (d) wykonaj wykresy oczekiwanej stopy zwrotu z portfela w zalezności od
#      ryzyka (mierzonego odchyleniem standardowym) dla portfeli z podpunktów 
#      (b) oraz (c).
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

data <- read.csv("Notowania.csv")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#                                 6.1 a)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
E_KGHM <- c(1:2500)
E_KGHM[1] <- 0
for(k in 2:2500){
  E_KGHM[k] <- (data$KGHM[k]-data$KGHM[k-1])/data$KGHM[k-1]
}
ocz_zwrot_KGHM <- sum(E_KGHM)/(length(data$KGHM)-1)
odchylenie_KGHM <- (sum(E_KGHM^2)/(length(data$KGHM)-1) - ocz_zwrot_KGHM^2)^(1/2)

E_PKN <- c(1:2500)
E_PKN[1] <- 0
for(k in 2:2500){
  E_PKN[k] <- (data$PKN[k]-data$PKN[k-1])/data$PKN[k-1]
}
ocz_zwrot_PKN <- sum(E_PKN)/(length(data$PKN)-1)
odchylenie_PKN <- (sum(E_PKN^2)/(length(data$PKN)-1) - ocz_zwrot_PKN^2)^(1/2)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#                                 6.1 b)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
w1 <- seq(0,1,0.1)
w2 <- seq(1,0,-0.1)
w <- cbind(w1,w2)
ocz_zwr_wagi <- c(1:11)
for(i in 1:11){
  ocz_zwr_wagi[i] <- w[i, 1]*ocz_zwrot_KGHM + w[i,2]*ocz_zwrot_PKN
}
odchylenie_wagi <- c(1:11)
for(i in 1:11){
  odchylenie_wagi[i] <- (w[i,1]^2*odchylenie_KGHM^2 + w[i,2]^2*odchylenie_PKN^2 + 2*w[i,1]*w[i,2]*cov(E_KGHM, E_PKN))^(1/2)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#                                 6.1 c)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
w1 <- seq(-1.5,1.5,0.1)
w2 <- seq(2.5,-0.5,-0.1)
w <- cbind(w1,w2)
ocz_zwr_krótka <- c()
for(i in 1:31){
  ocz_zwr_krótka[i] <- w[i, 1]*ocz_zwrot_KGHM + w[i,2]*ocz_zwrot_PKN
}
odchylenie_krótka <- c()
for(i in 1:31){
  odchylenie_krótka[i] <- (w[i,1]^2*odchylenie_KGHM^2 + w[i,2]^2*odchylenie_PKN^2 + 2*w[i,1]*w[i,2]*cov(E_KGHM, E_PKN))^(1/2)
}




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#                                 6.1 d)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 







#_______________________________________________________________________________
#
#------------------------------Zadanie 6.2--------------------------------------
#
#_______________________________________________________________________________
# Dla trzech wybranych spółek giełdowych z pliku Notowania.csv wyznacz wagi
# portfela o minimalnej wariancji składającego się z tych akcji.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(matlib)
E_KGHM <- c(1:2500)
E_KGHM[1] <- 0
for(k in 2:2500){
  E_KGHM[k] <- (data$KGHM[k]-data$KGHM[k-1])/data$KGHM[k-1]
}

E_PKN <- c(1:2500)
E_PKN[1] <- 0
for(k in 2:2500){
  E_PKN[k] <- (data$PKN[k]-data$PKN[k-1])/data$PKN[k-1]
}

E_PKOBP <- c(1:2500)
E_PKOBP[1] <- 0
for(k in 2:2500){
  E_PKOBP[k] <- (data$PKOBP[k]-data$PKOBP[k-1])/data$PKOBP[k-1]
}
C <- matrix(nrow = 3, ncol =3)
C[1,2] <- cov(E_KGHM, E_PKOBP)
C[1,1] <- cov(E_KGHM, E_KGHM)
C[1,3] <- cov(E_KGHM, E_PKN)
C[2,1] <- cov(E_PKOBP, E_KGHM)
C[2,2] <- cov(E_PKOBP, E_PKOBP)
C[2,3] <- cov(E_PKOBP, E_PKN)
C[3,1] <- cov(E_PKN, E_KGHM)
C[3,2] <- cov(E_PKN, E_PKOBP)
C[3,3] <- cov(E_PKN, E_PKN)
u <- matrix(c(1,1,1), ncol=3)
u_T <- matrix(c(1,1,1), nrow=3)
C_1 <- inv(C)
d <- u%*%C_1%*%u_T
w <- u%*%C_1/d[1,1]


#_______________________________________________________________________________
#
#------------------------------Zadanie 6.3--------------------------------------
#
#_______________________________________________________________________________
# Na początku:

# - umieść plik NotowaniaKwartalne.csv (zawierającego kursy akcji 7 spółek 
#   notowanych na GPW na koniec każdego kwartału od 31/12/2013 do 31/12/2018)
#   w katalogu roboczym,
# - zainstaluj pakiet quadprog,
#   uruchom załączony kod OptimalPortfolio.
#
# Następnie:
#
# (a) objaśnij działanie kodu,
# (b) objaśnij, co jest przedstawione na wykresie utworzonym przez kod OptimalPortfolio,
# (c) podaj obliczone w programie wartości:
#
#  - oczekiwanego zwrotu,
#  - odchylenia standardowego,
#  - współczynnika Sharpe’a
#
# oraz wag dla
#
# - portfela o minimalnej wariancji,
# - portfela z największym współczynnikiem Sharpe’a.
#
# (d) zmodyfikuj kod tak, aby znaleźć portfele optymalne 
#     (bez krótkiej sprzedaży) bazujące na wszystkich 7 akcjach 
#     (może się okazać, że wagi przy niektórych z nich są równe 0),
#
# (e) zmodyfikuj kod tak, aby znaleźć portfele optymalne bez ograniczenia 
#     na krótką sprzedaż.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


#_______________________________________________________________________________
#
#-----------------------  OptimalPortfolio  -----------------------
#
#_______________________________________________________________________________

library(quadprog)

NK <- read.csv("NotowaniaKwartalne.csv", header=TRUE)
n <- 21
attach(NK)

PZUZwrot <- 100*(PZU[-1]/PZU[-n]-1) 
PKNZwrot <- 100*(PKN[-1]/PKN[-n]-1)
CDRZwrot <- 100*(CDR[-1]/CDR[-n]-1) 
LPPZwrot <- 100*(LPP[-1]/LPP[-n]-1)
CEZZwrot <- 100*(CEZ[-1]/CEZ[-n]-1) 
CPSZwrot <- 100*(CPS[-1]/CPS[-n]-1) 
KERZwrot <- 100*(KER[-1]/KER[-n]-1) 

R <- cbind(PKNZwrot,CEZZwrot,KERZwrot)

mean_vect <-  apply(R,2,mean)
cov_mat <-  cov(R)
sd_vect <-  sqrt(diag(cov_mat))

Amat = cbind(rep(1,3),mean_vect,diag(1,nrow=3))

muP <-  seq(min(mean_vect)+.01,max(mean_vect)-.01,length=300)  
sdP <-  muP 
weights <-  matrix(0,nrow=300,ncol=3) 

for (i in 1:length(muP)){
  bvec <-  c(1,muP[i],rep(0,3))
  result <-  
    solve.QP(Dmat=2*cov_mat,dvec=rep(0,3),Amat=Amat,bvec=bvec,meq=2)
  sdP[i] <-  sqrt(result$value)
  weights[i,] <-  result$solution
}

par(mfrow = c(1,1))
plot(sdP,muP,type="l",xlim=c(0,25),ylim=c(0,15),lty=3)  

mufree <-  1.6/4 
points(0,mufree,cex=4,pch="*",col="violet")  

sharpe <- (muP-mufree)/sdP 
ind <-  (sharpe == max(sharpe)) 
weights[ind,] 

lines(c(0,sdP[ind]),c(mufree,muP[ind]),lwd=4,lty=1, col = "blue") 
points(sdP[ind],muP[ind],cex=4,pch="*",col="orange") 

ind2 <-  (sdP == min(sdP)) 
points(sdP[ind2],muP[ind2],cex=2,pch="+",col="green") 

ind3 <-  (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.3),lwd=2, col = "red")  

text(sd_vect[1],mean_vect[1],"PKN",cex=1.15)
text(sd_vect[2],mean_vect[2],"CEZ",cex=1.15)
text(sd_vect[3],mean_vect[3],"KER",cex=1.15)

#_______________________________________________________________________________



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 6.3a - do objasnienia na zajeciach
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 6.3b - do objasnienia na zajeciach
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 6.3c

# oczekiwany zwrot dla portfela o minimalnej wariancji:


# odchylenie standardowe dla portfela o minimalnej wariancji:


# współczynnik Sharpe'a dla portfela o minimalnej wariancji:


# wagi dla portfela o minimalnej wariancji:





# oczekiwany zwrot dla portfela o największym współczynniku Sharpe'a:


# odchylenie standardowe dla portfela o największym współczynniku Sharpe'a:


# współczynnik Sharpe'a dla portfela o największym współczynniku Sharpe'a:


# wagi dla portfela o największym współczynniku Sharpe'a:
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 6.3d
#
#     zmodyfikuj kod OptimalPortfolio tak, aby znaleźć portfele optymalne 
#     (bez krótkiej sprzedaży) bazujące na wszyskich 7 akcjach 
#     (może się okazać, że wagi przy niektórych z nich są równa 0),
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 









# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 6.3e
#
#     zmodyfikuj kod 6.3d tak, aby znaleźć portfele optymalne 
#     bez ograniczenia na krótką sprzedaż.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 



