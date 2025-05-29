
#-----------------*************** Lista 10 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 10.1--------------------------------------
#
#_______________________________________________________________________________
# Rozważmy kontrakt forward na dobro S z terminem rozliczenia za 7 miesięcy, 
# przy czym stopa wolna od ryzyka wynosi 3% a bieżąca (w chwili 0) cena S
# to 1000 PLN. Wyznacz cenę tego kontraktu terminowego, przy założeniu że
#
# (a) dobro S przynosi co miesiąc (w chwilach 1,2,...,7) zysk w wysokości 
#     10 PLN, ale nie wymaga opłat za przechowywanie.
#
# (b) dobro S wymaga co miesiac (w chwilach 0,1,...,6) uiszczenia opłaty 
#     za przechowywanie w wysokości 10 PLN, ale nie przynosi zysków.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# podpunkt a
r <- 0.03/12
S <- 1000
t <- c(1:7)
T <- 7
F.0.1 <- (S - 10*sum(exp(-r*t)))*exp(r*T)

# podpunkt b
t <- c(0:6)
F.0.2 <- (S + 10*sum(exp(-r*t)))*exp(r*T)

#_______________________________________________________________________________
#
#------------------------------Zadanie 10.2--------------------------------------
#
#_______________________________________________________________________________
# Kurs akcji A w dniu 1 stycznia 2023 to 120. Posiadacz akcji A 
# otrzymuje dywidendę wypłacana 1 lipca 2023 w wysokości 1 (na każdą akcję) 
# i ponownie w dniu 1 października 2023 w wysokości 2. 
# Stopa procentowa w roku 2023 wynosi 12% w skali roku dla każdego okresu
# (kapitalizacja ciągła). Cena akcji A w kontrakcie terminowym “forward” 
# z dostawą w dniu 1 listopada 2023 to 131. W obliczeniach załóż, że każdy 
# miesiąc to dokładnie 1/12 roku.
#
# (a) Uzasadnij, że w dniu 1 stycznia 2023 istniała możliwość arbitrażu.
#
# (b) Oblicz zysk z tego arbitrażu na dzień 1 listopada 2023. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# podpunkt a
t.1 <- 6/12
t.2 <- 9/12
t.3 <- 10/12
r <- 0.12
F.0.a <- (120 - exp(-t.1*r) - 2*exp(-t.2*r))*exp(r*t.3)
#F(0, T) > S(0)
# podpunkt b
zysk <- 131 - 120*exp(r*t.3) + exp(r*(t.3-t.1)) + 2*exp((t.3-t.2)*r)

#_______________________________________________________________________________
#
#------------------------------Zadanie 10.3--------------------------------------
#
#_______________________________________________________________________________
# Jest 01.12.2024. Kurs akcji S wynosi 400. Wiadomo, że na każdą akcję S w
# dniu 1.01.2025 będzie wypłacana dywidenda w wysokości 15. Stopa wolna od
# ryzyka (kapitalizacja ciągła) wynosi 6% dla wszystkich okresów.
#
# (a) Jaka jest teoretyczna cena jednorocznego kontraktu forward na tę akcję?
# 
# (b) Pośrednik kwotuje kontakty na forward na tę akcję po 420
#     (prowizja wynosi 1 za kontrakt, płatne z góry). Jesteś arbitrażystą. 
#     Możesz się finansować po stopie 6,25% w skali roku i lokować wolne środki
#     finansowe wg stopy 5,75% w skali roku. Jak wygląda Twoja strategia?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# podpunkt a 
r <- 0.06
F.0.3.a <- (400 - 15*exp(-r*1/12))*exp(r)
# podpunkt b
#tak jak w dowodzie tw. 2
saldo <- 420 - 401*exp(0.0625) + 15*exp(11/12*0.0575)

#_______________________________________________________________________________
#
#------------------------------Zadanie 10.4--------------------------------------
#
#_______________________________________________________________________________
# Załóżmy, że stopa procentowa wynosi 10% na rok (kapitalizacja ciągłą) 
# i że stopa dywidendy dla indeksu wynosi 4% na rok. 
# Aktualna wartość indeksu, to 400 USD a cena indeksu w transakcja futures 
# z dostawą za cztery miesiące to 405 USD. 
# Jaka jest teoretyczna cena tego kontraktu terminowego? 
# Jakie są tu możliwości arbitrażu?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
F.0.4 <- 400*(1 -  1 +exp(0.06*4/12))
#cena teoretyczna wyższa, niż jest w rzeczywistości



#_______________________________________________________________________________
#
#------------------------------Zadanie 10.5--------------------------------------
#
#_______________________________________________________________________________
# Cena spot srebra wynosi 15 USD za uncję. 
# Koszty przechowywania wynoszą 0.24 USD za uncję na rok 
# płatne kwartalnie z góry. 
# Zakładając, że stopa procentowa w USD wynosi 5% na rok (kapitalizacja ciągła)
# dla wszystkich okresów oblicz cenę srebra w kontraktach futures 
# z dostawą za 9 miesięcy.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
T <- 9/12
r <- 0.05
t <- c(0,1,2)
baza.0 <- 15*(1-exp(r*T)) -(0.24/4*sum(exp(-r*t/4)))*exp(r*T)
F.0.5 <- 15 - baza.0