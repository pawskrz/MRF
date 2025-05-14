
#-----------------*************** Lista 8 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 8.1--------------------------------------
#
#_______________________________________________________________________________
# Rozważmy portfel inwestycyjny składający się z 1000 akcji o wartości 
# P_0=20 PLN każda. Zakładając, że jednodniowe zwroty R_1 mają rozkład normalny
# o średniej zero i odchyleniu standardowym 3%, oblicz VaR dla jednodniowej 
# straty wartości portfela. Rozważ różne wartości alpha: 0.05, 0.01 oraz 0.005.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
a <- 1000
P_0 <- 20
miu <- 0
sigma <- 0.03
L <- -R_1






#_______________________________________________________________________________
#
#------------------------------Zadanie 8.2--------------------------------------
#
#_______________________________________________________________________________
# Dla danych z pliku Notowania.csv napisz funkcję 
#
# VaR(type = c(”a”, ”b”), year, company, size, alpha=0.05)},
#
# która dla zadanego 
#
# - rodzaju type ("a" lub "b" jak wyżej) rozkładu logarytmicznych stóp zwrotu,
# - roku year (liczba naturalna z zakresu [2009,2018]),
# - rodzaju spółki company (nazwa kolumny z notowaniami wybranej firmy),
# - rozmiaru inwestycji size (liczba akcji),
# - poziomu istotności alpha (domyślnie równy 0.05)
#
# zwraca VaR oszacowany punktowo dla logarytmicznych stóp zwrotu, przy czym 
# rozważamy dwa przypadki, przyjmując że te logarytmiczne zwroty mają rozkład
#
# (a) normalny o nieznanych parametrach,
# 
# (b) nieznany, ale absolutnie ciagły względem miary Lebesgue'a,
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


VaR <- function(type, year, company, size, alpha=0.05){
  

  
}



#_______________________________________________________________________________
#
#------------------------------Zadanie 8.3--------------------------------------
#
#_______________________________________________________________________________
# Przy oznaczeniach z zadania 8.2 napisz funkcję
#
# CVaR(type = c(”a”, ”b”), year, company, size, alpha=0.05)},
#
# zwracającą wartość CVaR oszacowaną punktowo.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

CVaR <- function(type, year, company, size, alpha=0.05){
  
  
}


