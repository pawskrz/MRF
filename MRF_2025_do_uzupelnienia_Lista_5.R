
#-----------------*************** Lista 5 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 5.1--------------------------------------
#
#_______________________________________________________________________________
# Rozpatrzmy obligację z dwuletnim terminem wykupu, o wartości nominalnej 
# 1000 PLN, 20% kuponie płatnym co pół roku i YTM równym 15%. 
#
# (a) Jaka jest jej bieżąca wartość (cena)?
# (b) Jaki jest czas trwania obligacji?
# (c) Jaki jest zmodyfikowany czas trwania obligacji?
# (d) Jak i w przybliżeniu o ile zmieni się cena obligacji, gdy YTM 
#     spadnie do 14%?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(FinCal)
#(a)
FV <- 1000
YTM <- 0.15/2
C <- 0.2*FV/2
P1 <- pv(YTM, 4, -FV, -C)
P1
#(b)
t <- c(1,2,3,4)
MacD <- function(P, t, C, YTM, t_0) 1/P*(sum(t/2*C/(1+YTM)^t) + t_0/2*FV/(1+YTM)^t_0) 
MD_1 <- MacD(P1, t, C, YTM, 4)
MD_1
#(c)
ModD_1 <- 1/(1+YTM)*MD_1
ModD_1
#(d)
FV <- 1000
YTM <- 0.14/2
C <- 0.2*FV/2
P2 <- pv(YTM, 4, -FV, -C)
róż <- P2-P1
# albo z delty
róż_delta_1 <- P1 * (-ModD_1 * 0.01)
róż_delta_1
#_______________________________________________________________________________
#
#------------------------------Zadanie 5.2--------------------------------------
#
#_______________________________________________________________________________
# Inwestor nabył 5-letnią obligację rządową w dniu jej emisji po cenie równej 
# wartości nominalnej, która wynosi 1000 PLN. Stopa kuponowa wynosi 2,1% 
# a same kupony są wypłacane w okresach rocznych. Stopa procentowa wynosi 2%.
# Na rynku pojawiają się informacje, że stopa procentowa może 
# w najbliższym czasie wzrosnąć o 75 pb (tj. do poziomu 2,75%).
#
# Oblicz zmodyfikowany czas twania oraz odpowiedz na pytanie:
# czy (i ewentualnie o ile) zmieni się cena obligacji w przypadku 
# wspomnianego wzrostu stóp procentowych?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
FV <- 1000
q <- 0.021
C <- q*FV
YTM1 <- 0.02
YTM2 <- 0.0275
P1_2 <- pv(YTM1, 5, -FV, -C)
P1_2
t_2 <- c(1,2,3,4,5)
MacD <- function(P, t, C, YTM, t_0) 1/P*(sum(t*C/(1+YTM)^t) + t_0*FV/(1+YTM)^t_0) 
MD_2 <- MacD(P1_2, t_2, C, YTM1, 5)
ModD_2 <- 1/(1+YTM1)*MD_2
ModD_2

##
P2_2 <- pv(YTM2, 5, -FV, -C)
róż_2 <- P2_2 - P1_2

# ale chcę policzyć, wykorzystując ModD, więc
róż_delta_2 <- P1_2 * (-ModD_2 * 0.0075)
róż_delta_2
#_______________________________________________________________________________
#
#------------------------------Zadanie 5.3--------------------------------------
#
#_______________________________________________________________________________
# Załóżmy, że funkcja stopy terminowej miała postać 
#
#         r(t) = 0,04 + 0,001t,
#
# gdy 8-letnia obligacja zerokuponowa została zakupiona. 
# Pół roku później, gdy sprzedano obligację funkcja stopy terminowej 
# miała postać 
#
#         r(t) = 0,03 + 0,0013t.
#
# Jaka była stopa zwrotu z tej inwestycji?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
B1_3 <- exp(-(0.04*8 + 0.001/2*8^2))
B2_3 <- exp(-(0.03*7.5 + 0.0013/2*7.5^2))

stopa_zwrotu <- (B2_3-B1_3)/B1_3

cat("Stopa zwrotu tej inwestycji to", stopa_zwrotu*100, "%")


#_______________________________________________________________________________
#
#------------------------------Zadanie 5.4--------------------------------------
#
#_______________________________________________________________________________
# Załączony plik TermStructureZero.csv zawiera symulowane dane 
# dotyczące 20 obligacji zerokuponowych. 
# W kolejnych polach znajdują się:
#
# Maturity  = liczba lat do wykupu,
# Quote     = kurs obligacji.   
#
# Napisz kod w R, który uzupełni plik o pola:
#
# SpotRate    = stopa spot dla n lat (zerokuponowa)
# ForwardRate = stopa terminowa dla n-tego roku.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
data <- read.csv('TermStructureZero.csv')
data$SpotRate <- (100/data$Quote)^(1/data$Maturity) -1
for (n in 20:2){
  data$ForwardRate[n] <- (1+data$SpotRate[n])^data$Maturity[n]/(1+data$SpotRate[n-1])^data$Maturity[n-1] -1
}
data$SpotRate[10]
data$ForwardRate[10]
#_______________________________________________________________________________
#
#------------------------------Zadanie 5.5--------------------------------------
#
#_______________________________________________________________________________
# Załączony plik TermStructure.csv zawiera symulowane dane 
# dotyczące 20 obligacji o kuponach wypłacanych raz w roku 
# (pierwsza wypłata za rok). W kolejnych polach znajdują się:
#
# Maturity  = liczba lat do wykupu,
# FV        = wartość nominalna,   
# Coupon    = wartość kuponu,
# Price     = cena obligacji,
#
# Napisz kod w R, który uzupełni plik o pola:
#
# SpotRate      = stopa spot dla n lat (zerokuponowa)
# ForwardRate   = stopa terminowa dla n-tego roku.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 



















