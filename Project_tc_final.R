library(nlme)
library(lavaan)
library(lattice)
library(ggplot2)
library(psych)
library(knitr)


#Loading data
dat <- read.csv('gn_dat_tc_v2.csv', header=T)

View(dat)


#Visualize data



#Model building

#CFA of subscales

#intentions to confront
intconcfa <-'
intcon =~ ConfWorse + NoLrnConf + ConfBad + NoConf
'

icfit = sem(intconcfa, data=dat)
summary(icfit, fit.measures=TRUE, standardized=T)


#SDO
sdocfa <-'
sdo1 =~ SDO1 + SDO2 + SDO3 + SDO4 + SDO5 + SDO6 + SDO7 + SDO8
sdo2 =~ SDO9 + SDO10 + SDO11 + SDO12 + SDO13 + SDO14 + SDO15 + SDO16'

sdofit = sem(sdocfa, data=dat)
summary(sdofit, fit.measures=TRUE, standardized=T)

#RWA

rwacfa <-'
rwa1 =~ RWA1 + RWA3 + RWA5 + RWA6 + RWA9 + RWA12 + RWA14 + RWA16 + 
RWA17 + RWA19 + RWA22 + RWA23 + RWA26 + RWA28 + RWA30
rws2 =~ RWA2 + RWA4 + RWA 7+ RWA8 + RWA10 + RWA11 + RWA13 + RWA15 +
RWA18 + RWA20 + RWA21 + RWA24 + RWA25 + RWA27 + RWA29 '

rwafit = sem(rwacfa, data=dat)
summary(rwafit, fit.measures=TRUE, standardized=T)


#External Motivation

emotivcfa <- '
emotiv =~ EMotiv1 + EMotiv2 + EMotiv3 + EMotiv4 + EMotiv5'

emfit = sem(emotivcfa, data=dat)
summary(emfit, fit.measures=TRUE, standardized=T)

imotivcfa <-'
imotiv =~ IMotiv1 + Rvs_IMotiv2 + IMotiv3 + IMotiv4 + IMotiv5'

imfit = sem(imotivcfa, data=dat)
summary(imfit, fit.measures=TRUE, standardized=T)


#Believer
brcfa <- '
belreal =~ Real + Believe'

brfit = sem(brcfa, data=dat)
summary(brfit, fit.measures=TRUE, standardized=T)



#Modeling SDO and message type on believe

#Model analysis
modbel <- '
#Factors

sdo1 =~ 1*SDO1 + SDO2 + SDO3 + SDO4 + SDO5 + SDO6 + SDO7 + SDO8

sdo2 =~ 1*SDO9 + SDO10 + SDO11 + SDO12 + SDO13 + SDO14 + SDO15 + SDO16

sdo =~ sdo1 + sdo2

#Regression/Moderation

Believe ~ sdo + Mess_Type_2 + intrxn_mess_sdo

#Var and Covar
intrxn_mess_sdo ~~ intrxn_mess_sdo
Believe ~~ 1*Believe
Mess_Type_2 ~~ Mess_Type_2
sdo1 ~~ sdo1
sdo2 ~~ sdo2
sdo ~~ 1*sdo
SDO1 ~~ SDO1
SDO2 ~~ SDO2
SDO3 ~~ SDO3
SDO4 ~~ SDO4
SDO5 ~~ SDO5
SDO6 ~~ SDO6
SDO7 ~~ SDO7
SDO8 ~~ SDO8
SDO9 ~~ SDO9
SDO10 ~~ SDO10
SDO11 ~~ SDO11
SDO12 ~~ SDO12
SDO13 ~~ SDO13
SDO14 ~~ SDO14
SDO15 ~~ SDO15
SDO16 ~~ SDO16
'

modbelfit = sem(modbel, data=dat)
summary(modbelfit, fit.measures=TRUE, standardized=T)





#Int cont model

modint <- '
#Factors


emotiv =~ 1*EMotiv1 + EMotiv2 + EMotiv3 + EMotiv4 + EMotiv5

intcon =~ 1*ConfWorse + NoLrnConf + ConfBad + NoConf


#Regression/Moderation

intcon ~ Believe + emotiv + intrxn_bel_emotiv

#Var and Covar
intrxn_bel_emotiv ~~ intrxn_bel_emotiv
Believe ~~ emotiv
Believe ~~ Believe
intcon ~~ intcon
emotiv ~~ 1*emotiv
EMotiv1 ~~ EMotiv1
EMotiv2 ~~ EMotiv2
EMotiv3 ~~ EMotiv3
EMotiv4 ~~ EMotiv4
EMotiv5 ~~ EMotiv5
ConfWorse ~~ ConfWorse
NoLrnConf ~~ NoLrnConf
ConfBad ~~ ConfBad
NoConf ~~ NoConf
'

modintfit = sem(modint, data=dat)
summary(modintfit, fit.measures=TRUE, standardized=T)
  
  



# Creating a new product predictor for moderation.. Outside of the model create believe*emotiv and mess_type_graphc*sdo

#Model analysis
mod2 <- '
#Factors

sdo1 =~ 1*SDO1 + SDO2 + SDO3 + SDO4 + SDO5 + SDO6 + SDO7 + SDO8

sdo2 =~ 1*SDO9 + SDO10 + SDO11 + SDO12 + SDO13 + SDO14 + SDO15 + SDO16

sdo =~ sdo1 + sdo2

emotiv =~ 1*EMotiv1 + EMotiv2 + EMotiv3 + EMotiv4 + EMotiv5

intcon =~ 1*ConfWorse + NoLrnConf + ConfBad + NoConf


#Regression/Moderation

intcon ~ Believe + emotiv + intrxn_bel_emotiv
Believe ~ sdo + Mess_Type_2 + intrxn_mess_sdo

#Var and Covar
intrxn_bel_emotiv ~~ intrxn_bel_emotiv
intrxn_mess_sdo ~~ intrxn_mess_sdo
Believe ~~ emotiv
Believe ~~ 1*Believe
Mess_Type_2 ~~ 1*Mess_Type_2
sdo1 ~~ sdo1
sdo2 ~~ sdo2
sdo ~~ 1*sdo
intcon ~~ intcon
emotiv ~~ 1*emotiv
SDO1 ~~ SDO1
SDO2 ~~ SDO2
SDO3 ~~ SDO3
SDO4 ~~ SDO4
SDO5 ~~ SDO5
SDO6 ~~ SDO6
SDO7 ~~ SDO7
SDO8 ~~ SDO8
SDO9 ~~ SDO9
SDO10 ~~ SDO10
SDO11 ~~ SDO11
SDO12 ~~ SDO12
SDO13 ~~ SDO13
SDO14 ~~ SDO14
SDO15 ~~ SDO15
SDO16 ~~ SDO16
EMotiv1 ~~ EMotiv1
EMotiv2 ~~ EMotiv2
EMotiv3 ~~ EMotiv3
EMotiv4 ~~ EMotiv4
EMotiv5 ~~ EMotiv5
ConfWorse ~~ ConfWorse
NoLrnConf ~~ NoLrnConf
ConfBad ~~ ConfBad
NoConf ~~ NoConf
'

mod2fit = sem(mod2, data=dat)
summary(mod2fit, fit.measures=TRUE, standardized=T)

