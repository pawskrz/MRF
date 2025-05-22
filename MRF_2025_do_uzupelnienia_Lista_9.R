
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
KalkulatorRatalnt(TypeFlag= c("A", "B", "C", "D"), K, n, r){
  if(TypeFlag="A"){
    rata <- K/(1-(1+r)^(n+1)/(r))
    return(rep(rata, n))
  }
  
  if(TypeFlag="B"){
    rata <- rep(0,n)
    stała <- K/n
    for(i in 0:(n-1)){
      rata[i] <- stała + r*(K-stała*i)
      }
  }
  
  if(TypeFlag="C"){
    
  }
}  




















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






































