
#-----------------*************** Lista 4 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 4.1--------------------------------------
#
#_______________________________________________________________________________
# Dwunastoletnia obligacja kuponowa o kuponie 3.5% płatnym w dniach rocznicy 
# dnia emisji była emitowana i sprzedawana inwestorom przy YTM=5%.
# Wartość nominalna obligacji to FV = 1000 PLN.
# Jaka była cena emisyjna tej obligacji?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
r <- 0.035
f <- 1000
q <- r*f
p <- q*((1-(1.05)^(-13))/(1-1.05^(-1)) -1) + f*(1.05)^(-12)

#albo z użyciem pakietu fincal
years = 12
p <- pv.uneven(r = 0.05, cf = c(rep(-f* r, years -1), -f - f*r))
#_______________________________________________________________________________
#
#------------------------------Zadanie 4.2--------------------------------------
#
#_______________________________________________________________________________
# Czteroletnia obligacja kuponowa o kuponie 6% (w skali roku), 
# płatnym co pół roku, była emitowana 
# i sprzedawana inwestorom przy YTM=5%. 
# Wartość nominalna obligacji to FV=10 000 PLN. 
# Jaka była cena emisyjna tej obligacji?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
FV <- 10000
q <- FV*0.06/2
r <- 0.05
t <- c(1:8)
p <- q*sum((1 +r/2)^(-t)) + FV*(1+r/2)^(-8)
p

#albo

pv(r=0.05/2, n=4*2, fv= -10000, pmt= -10000*0.06/2)

#albo
npv(r=0.05/2, cf=c(0, rep(10000 * 0.06/2, 7), 10000 + 10000*0.06/2))


#_______________________________________________________________________________
#
#------------------------------Zadanie 4.3--------------------------------------
#
#_______________________________________________________________________________
# Korzystając np. z funkcji uniroot() uzasadnij, że faktycznie 
# YTM = 2.158...% w przykładzie z obligacją serii PS0424 z wykładu drugiego 
# (slajdy 9-10/16).
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
f <- function(r){
  t <- c(73, 441, 805, 1169, 1534, 1900)
  return (25*(sum((1+r)^(-t/365))) + 1000*(1+r)^(-1900/365) - 1036.51)
}
uniroot(f, c(0, 0.03))

#_______________________________________________________________________________
#
#------------------------------Zadanie 4.4--------------------------------------
#
#_______________________________________________________________________________
# Wiadomo, że YTM = 3,5% dla obligacji o terminie wykupu za 5 lat, 
# wartości nominalnej 1000 PLN i cenie 950 PLN. 
# Jaka jest wartość każdego z kuponów płatnych raz na rok?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
f <- function(C){
  r <- 0.035
  t <- c(1,2,3,4,5)
  FV <- 1000
  P <- 950
  return(C*sum((1+r)^(-t)) + 1000*(1+r)^(-5) - 950)
}
x <- uniroot(f, c(0, 300))
q <- as.numeric(x[1])
cat("Wartość każdego z kuponów płatnych raz na rok to", q)

#można też uniroot(...)$root
#_______________________________________________________________________________
#
#------------------------------Zadanie 4.5--------------------------------------
#
#_______________________________________________________________________________
# Znajdź YTM dla 30-letniej obligacji o wartości nominalnej 1000 PLN 
# i kuponach 40 PLN płaconych na koniec każdego roku. 
# Cena obligacji wynosi 1200 PLN.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
f <- function(r){
  C <- 40
  t <- c(1:30)
  FV <- 1000
  P <- 1200
  return(C*sum((1+r)^(-t)) + FV*(1+r)^(-30) - P)
}
YTM <- as.numeric(uniroot(f, c(0, 0.05))[1])
cat("YTM dla powyższej obligacji wynosi", YTM*100,"%")
#_______________________________________________________________________________
#
#------------------------------Zadanie 4.6--------------------------------------
#
#_______________________________________________________________________________
# Znajdź YTM obligacji o terminie wykupu za 8 lat i 
# o wartości nominalnej 10 000 PLN, cenie 9800 PLN oraz kuponach 280 PLN 
# (w skali roku) płatnych co pół roku. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
f <- function(r){
  C <- 140
  t <- c(1:16)
  FV <- 10000
  P <- 9800
  return(C*sum((1+r/2)^(-t)) + FV*(1+r/2)^(-16) - P)
}
YTM <- as.numeric(uniroot(f, c(0, 0.05))[1])
cat("YTM dla powyższej obligacji wynosi", YTM*100,"%")
#_______________________________________________________________________________
#
#------------------------------Zadanie 4.7--------------------------------------
#
#_______________________________________________________________________________
# Rozpatrujemy obligacje skarbowe serii PS0728. 
# Oblicz YTM tej obligacji kupionej na przetargu, 
# który był rozliczany 25.01.2023.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
t1 <- as.Date("2022-07-25")
t2 <- as.Date("2023-01-25")
d<- as.Date(c("2023-07-25","2024-07-25","2025-07-25","2026-07-27","2027-07-26","2028-07-25"))
d6<- as.Date("2028-07-25")

b <- as.numeric(t2-t1)
q <- 75
P <- 1070
FV <- 1000
brud <- q*b/365 + P

t <- as.numeric(d-t2)/365

f <- function(r){
  C <- 75
  P <- brud
  FV <- 1000
  t <- t
  return(C*sum((1+r)^(-t)) + FV*(1+r)^(-(as.numeric(d6-t2))/365) - P)
}
YTM <- as.numeric(uniroot(f, c(0, 0.1))[1])
cat("YTM dla powyższej obligacji wynosi", YTM*100,"%")











#_______________________________________________________________________________
#
#------------------------------Zadanie 4.8--------------------------------------
#
#_______________________________________________________________________________
# Rozważmy obligacje kuponowe  z następującymi parametrami:
#
# - wartość nominalna FV=100 PLN,
# - czas do wykupu n lat,
# - kupony w wysokości C PLN płatne co roku,
# - stopy YTM=1%, 2%, ..., 100%.
#_______________________________________________________________________________




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#(a) 
# 
# Niech n=10 oraz C = 5,10,15. 
# Wykonaj wykresy zależności ceny obligacji od YTM.
# Czy zmiana wielkości kuponu wpływa tutaj na zmianę ceny obligacji?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -








# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#(b) 
#
# Niech  n = 5,10,15 oraz C=10.
# Wykonaj wykresy zależności ceny obligacji od YTM.
# Czy zmiana terminu do wykupu wpływa tutaj na zmianę ceny obligacji?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 










































