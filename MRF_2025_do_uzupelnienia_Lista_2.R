
#-----------------*************** Lista 2 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 2.1--------------------------------------
#
#_______________________________________________________________________________
# Dłużnik spłaca pierwsza ratę długu 
# na koniec pierwszego roku w wysokości 10 000 PLN oraz 
# drugą na koniec drugiego roku w wysokości 8 000$ PLN. 
# Jaka jest obecna wartość długu przy założeniu oprocentowania R = 12% ?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(FinCal)
r <- 0.12
dlug <- 10000
#i wzór z liceum :)

#za pomocą pv
pv(r, n=1, fv=-10000) + pv(r=0.12, n=2, fv=-8000)

#za pomocą npv
npv(r, cf=c(0, 10000, 8000))

#_______________________________________________________________________________
#
#------------------------------Zadanie 2.2--------------------------------------
#
#_______________________________________________________________________________
# Wpłacamy na konto dzisiaj 5000 PLN oraz przez kolejne 11 miesiecy po 
# 2 000 PLN. Jaką kwotę na koncie ujrzymy na koniec roku, jeśli oprocentowanie 
# w skali roku to R = 11% ? Zakładamy kapitalizację miesięczną i wpłaty 
# na początku każdego kolejnego miesiąca.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#bezpośrednio
P0 <- 5000
Pmies <- 2000
R <- 0.11
Rmies <- R/12

FV0 <- P0 * (1 + Rmies)^12
FV_r <- sum(Pmies*(1+Rmies)^(1:11))
FV <- FV0 + FV_r

#za pomocą fv.uneven
cashflow <- c(-5000, rep(-2000, 11), 0)
rate <- 0.11/12
v <- fv.uneven(rate, cashflow)

#za pomocą fv.annuity
r <- 0.11/12
fv_initial <- fv(r, 12, pv = -5000, type=1)
fv_annuity <- fv.annuity(r, 11, pmt = -2000, type=1)

fv_total <- fv_initial + fv_annuity
fv_total

#za pomocą fv
fv(r, n=12, pv=-3000, pmt=-2000, type=1)
#_______________________________________________________________________________
#
#------------------------------Zadanie 2.3--------------------------------------
#
#_______________________________________________________________________________
# Firma zainwestowała 3.0 mln PLN PLN w pewien projekt inwestycyjny, który 
# przynosi następujące przepływy pieniężne: 1.0 mln PLN na koniec trzeciego 
# roku, 1.5 mln PLN na koniec siódmego roku, 2.0 mln PLN na koniec 
# dwunastego roku. Wyznacz IRR tej inwestycji.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
irr(cf=c(-3,0,0,1,0,0,0,1.5,0,0,0,0,2))

#_______________________________________________________________________________
#
#------------------------------Zadanie 2.4--------------------------------------
#
#_______________________________________________________________________________
# Efektywna roczna stopa procentowa wynosi $14\%$. Podaj nominalna równoważną 
# stope procentową przy kapitalizacji 
#
# a) kwartalnej,
# b) miesięcznej,
# c) dziennej,
# d) ciągłej.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# podpunkt (a)
4 * ((1+0.14)**(1/4) -1)
4 * ear(0.14/4, 1/4)
r.norminal(r.continuous(0.14,1), 4)
# podpunkt (b)


# podpunkt (c)


# podpunkt (d)



#_______________________________________________________________________________
#
#------------------------------Zadanie 2.5--------------------------------------
#
#_______________________________________________________________________________
# Na rachunek (z kapitalizacja miesięczną) chcemy wpłacic 5000 PLN teraz oraz
# co miesiac przez pół roku kwoty w wysokości 800 PLN. Jakie najniższe 
# oprocentowanie w skali roku pozwoli nam zgromadzić po pół roku kwotę 
# w wysokości 10000 PLN?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
f1 <- function(x){
  suma <- 10000 - 5000*(1+x)**6 - sum(800*(1+x))
}













#_______________________________________________________________________________
#
#------------------------------Zadanie 2.6--------------------------------------
#
#_______________________________________________________________________________
# Dłużnik ma spłacić dług w wysokości 10000 PLN po dwóch latach, jednak 
# z prawdopodobienstwem 20% będzie niewypłacalny (i nic nie zwróci), 
# a z prawdopodobienstwem 10% wypłaci kwotę 10000 PLN dopiero po 3 latach. 
# Jaka jest oczekiwana dzisiejsza wartość długu przy założeniu oprocentowania 
# R = 10% ?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

dlug <- 10000
niewyp <- 0.2
spłata_3l <- 0.1
spłata_2l <-0.7
oproc_4 <- 0.1
wart_biez_po_2 <- dlug / (1+ oproc_4)^2
wart_biez_po_3 <- dlug / (1+ oproc_4)^3
oczekiwana_długu <- spłata_2l * wart_biez_po_2 + spłata_3l * wart_biez_po_3 + niewyp*0











#_______________________________________________________________________________
#
#------------------------------Zadanie 2.7--------------------------------------
#
#_______________________________________________________________________________
# Wygrałem 1 000 000 EUR (po opodatkowaniu) na loterii sylwestrowej. 
# Postanowiłem wyruszyć w podróż, którą będę finansował wypłacając sobie po
# 100 000 EUR na poczatku każdego roku. Pozostała kwota utrzymywana bedzie na 
# rachunku bankowym oprocentowanym 6% w skali roku z kapitalizacja roczna. 
# W którym roku podrózy skończy mi sie finansowanie?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
P <- 1000000
wypłaty <- 100000
r <- 0.06
rok <- 0

while(P>0){
  P <- (P - wypłaty)*(1+r)
  rok <- rok +1
}
print(rok)