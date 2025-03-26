
#-----------------*************** Lista 3 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 3.1--------------------------------------
#
#_______________________________________________________________________________
# Ile wyniosła cena "brudna" obligacji PS0123 kupionej na GPW w dniu 
# D=12.02.2019 po kursie 102.40? 
# Przyjmujemy tutaj, że dzień rozliczenia transakcji to D+2.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#liczymy ilość dni, które upłynęły od
#12+2.02.2019 do 25.01.2019, czyli ilość dni od ostatnich odsetek
d <- 20
r <- 0.025
odsetki <- 25 * d/365 
P <- 1000
brud <- P*1.0240 + odsetki
brud
#liczymy daty nie na piechotę
t1 <- as.Date("2019-02-14")
t2 <- as.Date("2019-01-25")
t <- as.numeric(t1-t2)
#reszta tak samo
#_______________________________________________________________________________
#
#------------------------------Zadanie 3.2--------------------------------------
#
#_______________________________________________________________________________
# Jedna sztuka obligacji OK0521 została kupiona na Giełdzie Papierów 
# Wartościowych (GPW) w dniu D=20.02.2019 po kursie 96,90. 
# Jaka jest rentowność tej inwestycji dla  kupującego 
# (zakładając, że będzie trzymał obligację do dnia wykupu), 
# jeśli prowizja maklerska jaką płaci kupujący wynosi 0,12% 
# wartości transakcji? Przyjmujemy tutaj, że dzień rozliczenia transakcji 
# to D+2.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
P <- 0.9690 * 1000
FV <- 1000
t <- (730 + 7 + 31+ 30+ 25)/365
M <- 0.0012*1000
r <- ((FV-M)/P)^(1/t) - 1
r

#inaczej (poprawnie) makler bierze procent z ceny obligacji, nie z FV
P <- 0.969*1000 + 0.0012*P
r <- (FV/P)^(1/t) -1
r
#_______________________________________________________________________________
#
#------------------------------Zadanie 3.3--------------------------------------
#
#_______________________________________________________________________________
# Dana jest obligacja zerokuponowa, w przypadku której do terminu wykupu 
# pozostały dwa lata i 115 dni. Jej wartość nominalna wynosi 100 PLN,
# a wymagana stopa dochodu inwestora 5,3%. Dokonaj wyceny obligacji. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
t <- 845/365
FV <- 100
r <- 0.053
P <- FV/((1+r)^t)
P

#_______________________________________________________________________________
#
#------------------------------Zadanie 3.4--------------------------------------
#
#_______________________________________________________________________________
# Pewien inwestor zakupił obligację zerokuponową w przypadku której do
# terminu wykupu pozostało 10 lat. Rynkowa stopa rentowności przy zakupie
# wynosiła 7,25%. Inwestor ten, po 20 miesiącach sprzedał obligację,
# przy stopie rentowności 6,75%. Jaką stopę zwrotu uzyskał inwestor 
# na tej obligacji?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#stopa rentowności to YTM
FV <- 100
r_zakup <- 0.0725
r_sprzedaż <- 0.0675
t_wykup <- 10
t_sprzedaż <- 10 -(20/12)

p_zakup <- FV/(1+r_zakup)^t_wykup
p_sprzedaż <- FV / (1+ r_sprzedaż)^t_sprzedaż

R <- ((p_sprzedaż - p_zakup)/p_zakup) *100
#_______________________________________________________________________________
#
#------------------------------Zadanie 3.5--------------------------------------
#
#_______________________________________________________________________________
# Inwestor zastanawia się nad inwestycją w obligacje zerokuponowe.
# Jego oczekiwana stopa rentowności wynosi 8%. Określ, jaką maksymalną cenę
# jest w stanie zaakceptować inwestor za obligację zerokuponowa o wartości
# nominalnej 10000, w przypadku której pozostało 11 lat i 3 miesiące do wykupu.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
t <- 11 + 3/12
r <- 0.08
FV <- 10000
P <- FV/(1.08)^t
P







#_______________________________________________________________________________
#
#------------------------------Zadanie 3.6--------------------------------------
#
#_______________________________________________________________________________
# Inwestor rozważa zakup obligacji zerokuponowych o terminie do wykupu jeden,
# trzy lub pięć lat. W tym momencie rentowności obligacji zerokuponowych 
# 
# 1-, 2-, 3-, 4-, 5-letnich
# 
# to odpowiednio 
#
# 3,1%, 3,5%, 4%, 4,2%  4,3%
#
# przy czym rentowność jest wyrażona w skali roku z kapitalizacją co pół roku.
#
# Inwestor planuje sprzedaż kupowanych dziś obligacji za rok. 
# Rozważa przy tym dwa następujące scenariusze:
# 
# I. Stopy procentowe za rok będą takie sama jak dziś i dlatego rentowności 
# obligacji 1-, 2-, 3-, 4- i 5-letnich wyniosą odpowiednio 
# 
# 3,1%, 3,5%, 4%, 4,2%  4,3%
#
# II. Stopy procentowe (dla wszystkich okresów) za rok będą o $0,5$ p.p. 
# (punktu procentowego) wyższe niż dziś tzn. 
# rentowność obligacji 1-rocznych wyniesie 3,6%,
# dwuletnich 4% itd, przy czym rentowność jest tu również wyrażona 
# w skali roku z kapitalizacją co pół roku.
#
# Które z obligacji powinien zakupić inwestor w każdym z tych dwóch scenariuszy,
# aby uzyskać jak najwyższą stopę zwrotu z tej rocznej inwestycji?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#wartość nominalna (arbitralna)
fv <- 1000

#stopy
r.0.0 <- c(3.1, 3.5, 4, 4.2, 4.3)/100

#cena obligacji
cena.0 <- numeric(5)
for (i in 1:5){
  cena.0[i] <- fv * (1+r.0.0[i]/2)**(-2*i)
}
#Scenariusz I stop się nie zmieniają =
cena.1 <- numeric(5)
cena.1[1] <- fv
for (i in 2:5){
  cena.1[i] <- fv*(1 + r.0.0[i-1]/2)**(-2*(i-1))
}
#dla 3-letniej bierzemy stopę dwuletniej, bo za rok stanie się dwuletnią obligacją. itd.

#zwroty
zwroty.1 <- (cena.1 /cena.0) -1
100 * zwroty.1

#Scenariusz II 
r.0.5 <- r.0.0 + 0.5/100

cena.2 <- numeric(5)
cena.2[1] <- fv
for (i in 2:5){
  cena.2[i] <- fv*(1+r.0.5[i-1]/2)**(-2*(i-1))
}

#zwroty
zwroty.2 <- (cena.2/cena.0) -1
100 * zwroty.2