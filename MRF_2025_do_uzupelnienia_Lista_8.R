
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
alpha <- c(0.05, 0.01, 0.005)
VaR <- -P_0*a * qnorm(alpha) * sigma





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
  setwd("/Users/mac/Documents/MRF2025")
  data <- read.csv("Notowania.csv")
  data[,"Data"] <- as.Date(data[, "Data"], format="%m/%d/%y")
  cond <- substr(x=data[, "Data"], start=1, stop=4) == as.character(year)
  data1 <- data[cond,company]
  data1.returns <- data1[-1]/data1[-length(data1)]-1 
  data1.log.returns <- log(1+data1.returns)
  if(type == "a"){
    library(MASS)
    #szacujemy parametry rozkładu normalnego
    p <- fitdistr(x=data1.log.returns, densfun="normal")$estimate
    v <- qnorm(p=alpha, mean=p[1], sd=p[2])
  }
  if(type == "b"){
    v <- quantile(probs=alpha, data1.log.returns) 
  }
    #twierdzenie 2 daje:
  var <- size*data1[1] * (1-exp(v))
  return(var)
}
VaR(type="a", year=2009, company="PKOBP", size=1000, alpha=0.05)
VaR(type="b", year=2009, company="PKOBP", size=1000, alpha=0.05)

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
f.podcalkowa <- function(type, year, company, size){
  val <- function(alpha){
    return(VaR(type, year, company, size, alpha))
  }
  return(val)
}

CVaR <- function(type, year, company, size, alpha=0.05){
  calka <- integrate(f=f.podcalkowa(type, year, company, size), 
                     lower=0, 
                     upper=alpha)$value
  return(calka/alpha)
}
CVaR(type="a", year=2009, company="PKOBP", size=1000, alpha=0.05)
CVaR(type="b", year=2009, company="PKOBP", size=1000, alpha=0.05)
