
#-----------------*************** Lista 7 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 7.1--------------------------------------
#
#_______________________________________________________________________________
# Portfel inwestycyjny składa się z 5 akcji (od A1 do A5). 
# Współczynnik beta dla portfela wynosi 1,7. 
# Każda z akcji ma równy wartościowy udział w portfelu.
# Współczynnik beta dla akcji A1 wynosi 1,8. 
# Akcja A1 zostałą sprzedana i w jej miejsce została zakupiona akcja X. 
# Ile powinien wynieść współczynnik beta dla nowo zakupionej akcji X, 
# aby współczynnik beta dla portfela osiągnął poziom 1,6?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#0 = Bx*1.34*Cov(KM, Kx)*1/1.8 + (0.2Bx - 1.6)*Cov(KM, Kx)
Bx = 1.3




















#_______________________________________________________________________________
#
#------------------------------Zadanie 7.2--------------------------------------
#
#_______________________________________________________________________________
# Współczynnik beta akcji spółki ETA wynosi 1,3, a stopa zwrotu 
# z portfela rynkowego 9%. Jeżeli oczekiwna stopa zwrotu z akcji 
# spółki ETA wynosi 10,8%, to oszacuj wartość stopy wolnej od ryzyka. 
# Przyjmij, że spełnione są warunki dla stosowania modelu CAPM.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
B_ETA = 1.3
miu_M = 0.09
miu_ETA = 0.108
r_f <- (miu_ETA - B_ETA*miu_M)/(1-B_ETA) 
r_f
#_______________________________________________________________________________
#
#------------------------------Zadanie 7.3--------------------------------------
#
#_______________________________________________________________________________
# Na rynku występują jedynie akcje dwóch spółek Alfa i Omega.
# Cena jednej akcji spółki Alfa wynosi 30 PLN. 
# Liczba akcji spółki Alfa równa jest 10 000. 
# Współczynnik beta akcji spółki Alfa wynosi 1,1. 
# Cena jednej akcji spółki Omega wynosi 10 PLN. 
# Liczba akcji spółki Omega jest równa 20 000. 
# Wyznacz wartość współczynnika beta dla akcji spółki Omega.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 











#_______________________________________________________________________________
#
#------------------------------Zadanie 7.4--------------------------------------
#
#_______________________________________________________________________________
# Za pomoca modelu CAPM oszacuj wielkość stopy zwrotu z akcji Y, 
# wiedząc, że współczynnik beta tej akcji wynosi 1,3. 
# Przyjmij, że wartość stopy wolnej od ryzyka wynosi 3%,
# a stopy zwrotu z portfela rynkowego to 9%.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 














#_______________________________________________________________________________
#
#------------------------------Zadanie 7.5--------------------------------------
#
#_______________________________________________________________________________
# Zakładamy, że oczekiwana stopa zwrotu z portfela rynkowego wynosi mu_M = 15%,
# oraz że bety trzech walorów wynoszą 
#
# beta_1 = 0.7,     beta_2 = 0.3,    beta_3 = 1.6.
#
# Szacujemy, że inwestycja w portfel o wagach 
#
# w = (w1,w2,w3) = (0.3, 0.3, 0.4)
#
# przyniesie nam zwrot w wysokości 13%.
# Czy powinnismy zainwestowac w ten portfel, jeżeli stopa wolna od ryzyka 
# wynosi r_f = 5%, a nasza decyzje podejmujemy w oparciu o model CAPM?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 















#_______________________________________________________________________________
#
#------------------------------Zadanie 7.6--------------------------------------
#
#_______________________________________________________________________________
# Załóżmy, że współczynnik korelacji pomiędzy stopą zwrotu z akcji A,
# a stopą zwrotu z portfela rynkowego wynosi 0.8.
# Jaką część całkowitego ryzyka stopy zwrotu z akcji A
# stanowi tutaj ryzyko niesystematyczne?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 













#_______________________________________________________________________________
#
#------------------------------Zadanie 7.7--------------------------------------
#
#_______________________________________________________________________________
# Oczekiwana stopa zwrotu z portfela rynkowego wynosi 18% a wariancja 
# tej stopy zwrotu jest równa 7%. Oczekiwana stopa zwrotu z akcji 
# spółki A wynosi 24%. Stopa zwrotu z aktywów wolnych od ryzyka to 4%. 
# Wyznacz kowariancję między stopą zwrotu z portfela rynkowego i stopa zwrotu 
# z portfela akcji spółki A.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 








