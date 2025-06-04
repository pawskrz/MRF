
#-----------------*************** Lista 11 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 11.1--------------------------------------
#
#_______________________________________________________________________________
# Czy dla podanych kursów EUR/PLN możliwy jest arbitraż? 
# Jeśli tak, to podaj zysk z niego przy inwestycji 1 000 000 PLN.
#
#             Kupno (Bid)   Sprzedaż (Ask) 
#
#   Rynek 1     4.3157        4.3227 
#
#   Rynek 2     4.3257        4.3360 
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
sprzedaż <- 10^6 * 1/4.3227
kupno <- sprzedaż * 4.3257
zysk1 <- kupno - 10^6
#_______________________________________________________________________________
#
#------------------------------Zadanie 11.2--------------------------------------
#
#_______________________________________________________________________________
# Wiadomo, że kursy walut wynoszą
#
#   USD/EUR = 0.9405,
#   USD/JPY = 157.771,
#   EUR/JPY = 168.708.
#
# Czy jest tutaj możliwy arbitraż dla osoby posiadającej dolary? 
# Jeśli tak, to jaki byłby z niego zysk przy inwestycji 100 000 USD?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
kurs_usd_eur_jpy <- 0.9405 * 168.708 # > 157.771 więc arbitraż możliwy
kurs_usd_jpy <- 157.771
inwestycja <- 10^5
zysk2 <- inwestycja*(kurs_usd_eur_jpy *1/kurs_usd_jpy) - inwestycja #JPY
#_______________________________________________________________________________
#
#------------------------------Zadanie 11.3--------------------------------------
#
#_______________________________________________________________________________
# Kontrakt FRA na 90 dniowy WIBOR wygasa za 60 dni. Wartość nominalna
# kontraktu to 10 MLN PLN. Ustalona w kontrakcie FRA wartość stopy
# procentowej wynosi 5%. Aktualna wartość 90 dniowej stopy terminowej w
# dniu wygaśnięcia kontraktu wynosi 5.5%. Stopa spotowa 60-dniowa wynosi
# 5.2%. Wszystkie stopy są podane w skali rocznej. Oblicz wartość bieżącą
# kontraktu FRA.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
N <- 10^7
r_1 <- 0.052
r_1.2 <- 0.055
r_fra <- 0.05
P3 <- N*((r_1.2 - r_fra)*90/360 / ((1+r_1*60/360)*(1+r_1.2*90/360)))
#_______________________________________________________________________________
#
#------------------------------Zadanie 11.4--------------------------------------
#
#_______________________________________________________________________________
# Na rynku dostępne są następujące instrumenty wraz z odpowiadajacymi im stopami
# procentowymi (w skali roku; rok = 365 dni): 
#
# 3M depozyt (91 dni) - 4.0%,
# FRA 3x6 (92 dni)    - 4.5%,
# FRA 6x9 (90 dni)    - 4.9%,
# FRA 9x12 (92 dni)   - 5.6%.
# 
# Ile wynosi roczna stopa procentowa na bazie konwecji ACT/360?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
((1+0.04*91/365)*(1+0.045*92/365)*(1+0.049*90/365)*(1+0.056*92/365) -1 )*100
#_______________________________________________________________________________
#
#------------------------------Zadanie 11.5--------------------------------------
#
#_______________________________________________________________________________
# Zgodnie z warunkami zawartego kontraktu swap instytucja finansowa zgodziła się
# płacić oprocentowanie 3M WIBOR w zamian za oprocentowanie 8%
# rocznie (w warunkach kapitalizacji ciągłej). Wartość nominalna swapu jest
# równa 100 MLN PLN. Okres pozostający do końca ważności kontraktu to
# 11M. Stałe stopy procentowe przy kapitalizacji ciągłej dla 2M, 5M, 8M oraz
# 11M wynoszą odpowiednio 
#
# R_1 = 9.0%,  R_2 = 10.0%,  R_3 = 11.0%,  R_4=12.0%.
#
# 3M WIBOR w terminie ostatniej płatności był równy 9.6% (przy kapitalizacji
# kwartalnej). Dokonaj wyceny tego kontraktu swap. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
N <- 100^6
R_Z <- 0.096
N*(1+R_Z/4)*11/12
R <- c(0.09, 0.1, 0.11, 0.12)
T <- c(2,5,8,11)/12
CF_stały <- rep(0.08*1/4, 4)*100
CF_zmienny <- rep(0, 4)
CF_zmienny[1] <- -9.6*0.25
CF_zmienny[2] <- -100*4*(exp(0.25*((R[2]*T[2] - R[1]*T[1])/(T[2]-T[1]))) -1) * 0.25
CF_zmienny[3] <- -100*4*(exp(0.25*((R[3]*T[3] - R[2]*T[2])/(T[3]-T[2]))) -1) * 0.25
CF_zmienny[4] <- -100*4*(exp(0.25*((R[4]*T[4] - R[3]*T[3])/(T[4]-T[3]))) -1) * 0.25
CF_netto <- CF_stały + CF_zmienny
DF <- exp(-R*T)
PVD <- CF_netto * DF
wartość <- sum(PVD)
wartość * 10^6 #PLN