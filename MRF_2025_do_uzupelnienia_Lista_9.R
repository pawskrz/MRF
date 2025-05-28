
#-----------------*************** Lista 9 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 9.1--------------------------------------
#
#_______________________________________________________________________________
# Każda z osób A,B,C,D zaciąga kredyt w wysokości K na n okresów. 
# Spłaty rat kredytu następują na koniec każdego z okresów. 
# Oprocentowanie kredytu w skali okresu wynosi r, przy czym
#
# - A spłaca kredyt w jednakowych ratach,
#
# - B spłaca kredyt metodą "jednakowych rat kapitałowych"
#   tzn. każda rata spłaty jest równa K/n plus odsetki od kwoty pozostałej 
#   do spłaty na początku okresu,
#
# - C w pierwszych n-1 ratach spłaca jedynie odsetki za kończący się okres,
#   a w ostatniej spłaca cały kapitał K plus odsetki za n-ty okres,
#
# - D spłaca całość zobowiązań na koniec n-tego okresu, przy czym 
#   odsetki są kapitalizowane na koniec każdego okresu.
#
#   Napisz funkcję KalkulatorRatalny(TypeFlag = c("A", "B", "C", "D"), K, n, r)}
#   zwracającą wektor wszystkich rat spłaty kredytu dla każdej z osób A,B,C,D.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
KalkulatorRatalny <- function(TypeFlag= c("A", "B", "C", "D"), K, n, r){
  if(TypeFlag=="A"){
    rata <- K*r*(1+r)^n/((1+r)^(n)-1)
    return(rep(rata, n))
  }
  
  rata <- rep(0,n)
  
  if(TypeFlag=="B"){
    stała <- K/n
    for(i in 1:(n)){
      rata[i] <- stała + r*(K-stała*(i-1))
    }
    return(rata)
  }
  
  if(TypeFlag=="C"){
    for(i in 1:n-1){
      rata[i] <- K*r
    }
    rata[n] <- K*(1+r)
    return(rata)
  }
  
  if(TypeFlag=="D"){
    rata[n] <- K*(1+r)^n
    return(rata)
  }
}  

K <- 1000
n <- 4
r <- 0.1
KalkulatorRatalny("A", K, n, r)
KalkulatorRatalny("B", K, n, r) #najmniej do zapłaty
KalkulatorRatalny("C", K, n, r)
KalkulatorRatalny("D", K, n, r)
#oddalanie terminy płatności powiększa koszty!
#_______________________________________________________________________________
#
#------------------------------Zadanie 9.2--------------------------------------
#
#_______________________________________________________________________________
# Bank może nam udzielić kredytu w wysokości 10 000 PLN na okres 4 lat.
# Możemy wybrac pomiędzy jednym z dwóch sposobów spłaty kredytu:
#
# - spłata kredytu w dwóch ratach po 6000 PLN każda 
#   (na koniec drugiego i czwartego roku) 
#
# lub
#
# - spłata kredytu w czterech ratach po 3000 PLN każda.
#
# Ile wyniesie RRSO w obydwu propozycjach i która z propozycji spłaty 
# jest dla nas korzystniejsza?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(FinCal)
RRSO_1 <- irr(c(10000, 0, -6000, 0, -6000))
RRSO_2 <- irr(c(10000, -3000, -3000, -3000, -3000))
RRSO_1 #korzystniej.
RRSO_2
#_______________________________________________________________________________
#
#------------------------------Zadanie 9.3--------------------------------------
#
#_______________________________________________________________________________
# Bank może nam udzielić kredytu w wysokości 200 000 PLN na okres 5 lat.
# Kredyt będzie spłacany metodą równych rat kapitałowych tj. 
# każda w wysokości 40 000 PLN, płatne na początku roku.
#
# Stopa procentowa wynosi 4%.
#
# W chwili t=0 pobierane są następujące opłaty
#
# - prowizja 6000 PLN,
# - opłata przygotowawcza 2000 PLN,
# - wycena zabezpiecenia 1000 PLN.
# 
# Ponadto na początku każdego okresu płacone jest również 
# ubezpieczenie spłaty w wysokości 2% kwoty kredytu pozostałej do spłaty.
#
# Ile wynosi RRSO tego kredytu?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
ciąg_płatności <- numeric(6)
ciąg_płatności[1] <- (200 - 6 - 2 - 1 - 0.02*200) * 10^3
ciąg_płatności[2] <- (-40 - 0.04*200 - 0.02*160) * 10^3
ciąg_płatności[3] <- (-40 - 0.04*160 - 0.02*120) * 10^3
ciąg_płatności[4] <- (-40 - 0.04*120 - 0.02*80) * 10^3
ciąg_płatności[5] <- (-40 - 0.04*80 - 0.02*40) * 10^3
ciąg_płatności[6] <- (-40 - 0.04*40 - 0.02*0) * 10^3
ciąg_płatności
RRSO <- 100 * irr(ciąg_płatności)
RRSO #%