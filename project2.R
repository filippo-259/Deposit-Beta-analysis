# data used in this script can be found at the following links:
#https://data.ecb.europa.eu/data/data-categories/financial-markets-and-interest-rates/bank-interest-rates/interest-rates/deposits
#https://data.ecb.europa.eu/data/data-categories/ecbeurosystem-policy-and-exchange-rates/official-interest-rates?resetAllFilters=false&filterType=basic&tags_array%5B0%5D=Key+interest+rate&filterSequence=&filtersReset=false&showDatasetModal=false
library(dplyr)
rm(list=ls())
setwd('C:\\Users\\Filippo\\Documents\\Università Cattolica\\4° Anno\\2° Semestre\\Banking and Finance\\Teamwork\\Project 2')

ECB_daily=read.csv('ECB Data key rates.csv')
ECB_daily$DATE=as.Date(ECB_daily$DATE)


#Creation of a dataframe to store deposit betas
Betas=data.frame(Type.of.deposit = rep(NA, 4), Austria.Corporations = NA, Austria.Households = NA,
                 Belgium.Corporations = NA, Belgium.Households = NA,
                 Croatia.Corporations = NA, Croatia.Households = NA,
                 Cyprus.Corporations = NA, Cyprus.Households = NA,
                 Estonia.Corporations = NA, Estonia.Households = NA,
                 Finland.Corporations = NA, Finland.Households = NA,
                 France.Corporations = NA, France.Households = NA,
                 Germany.Corporations = NA, Germany.Households = NA,
                 Greece.Corporations = NA, Greece.Households = NA,
                 Ireland.Corporations = NA, Ireland.Households = NA,
                 Italy.Corporations = NA, Italy.Households = NA,
                 Latvia.Corporations = NA, Latvia.Households = NA,
                 Lithuania.Corporations = NA, Lithuania.Households = NA,
                 Luxembourg.Corporations = NA, Luxembourg.Households = NA,
                 Malta.Corporations = NA, Malta.Households = NA,
                 Netherlands.Corporations = NA, Netherlands.Households = NA,
                 Portugal.Corporations = NA, Portugal.Households = NA,
                 Slovakia.Corporations = NA, Slovakia.Households = NA,
                 Slovenia.Corporations = NA, Slovenia.Households = NA,
                 Spain.Corporations = NA, Spain.Households = NA)
Betas$Type.of.deposit[1] = 'Deposits overnight'
Betas$Type.of.deposit[2] = 'Deposits with an agreed Maturity of up to 1 year'
Betas$Type.of.deposit[3] = 'Deposits with an agreed Maturity of over 2 years'
Betas$Type.of.deposit[4] = 'Deposits redeemable at notice'

#AUSTRIA
d=read.csv('ECB Data Portal_Austria.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Austria', 'Overnight.Households.Austria', 'AgreedMaturityUp1Y.Corporations.Austria',
              'AgreedMaturityUp1Y.Households.Austria','AgreedMaturityOver2Y.Corporations.Austria','AgreedMaturityOver2Y.Households.Austria',
              'RedeemableAtNotice.Austria')
d1=d[d$DATE > as.Date("2022-07-01"), ]

plot(ECB$DATE,ECB$DFR, type='l', main='Deposit Facility Rate', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Austria,
     type='l', main='Deposits overnight, Austria', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Austria, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Austria~ECB1$DFR) 
summary(lr) #deposit beta = 0.22 
Betas$Austria.Corporations[1]=lr$coefficients[2] 
#households
lr=lm(d1$Overnight.Households.Austria~ECB1$DFR)
summary(lr) #deposit beta = 0.219 
Betas$Austria.Households[1]=lr$coefficients[2] 

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Austria,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Austria', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Austria, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Austria~ECB1$DFR)
summary(lr) #deposit beta = 0.85 
Betas$Austria.Corporations[2]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Austria~ECB1$DFR)
summary(lr) #deposit beta = 0.73
Betas$Austria.Households[2]=lr$coefficients[2] 

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Austria,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Austria', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Austria, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Austria~ECB1$DFR)
summary(lr) #deposit beta = 0.75 (high)
Betas$Austria.Corporations[3]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Austria~ECB1$DFR)
summary(lr) #deposit beta = 0.54
Betas$Austria.Households[3]=lr$coefficients[2] 

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Austria,
     type='l', main='Deposits redeeemable at notice, Austria', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Austria~ECB1$DFR)
#summary(lr)
#Betas$Austria.Households[4]=lr$coefficients[2] 

Betas

#BELGIUM
d=read.csv('ECB Data Portal_Belgium.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Belgium', 'Overnight.Households.Belgium', 'AgreedMaturityUp1Y.Corporations.Belgium',
              'AgreedMaturityUp1Y.Households.Belgium','AgreedMaturityOver2Y.Corporations.Belgium','AgreedMaturityOver2Y.Households.Belgium',
              'RedeemableAtNotice.Belgium')
d1=d[d$DATE > as.Date("2022-07-01"), ]

plot(ECB$DATE,ECB$DFR, type='l', main='DFR', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Belgium,
     type='l', main='Deposits overnight, Belgium', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Belgium, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Belgium~ECB1$DFR)
summary(lr) #deposit beta = 0.05 (low deposit beta)
Betas$Belgium.Corporations[1]=lr$coefficients[2] 
#households
lr=lm(d1$Overnight.Households.Belgium~ECB1$DFR)
summary(lr) #deposit beta = 0.019 (low deposit beta)

Betas$Belgium.Households[1]=lr$coefficients[2] 

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Belgium,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Belgium', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Belgium, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Belgium~ECB1$DFR)
summary(lr) #deposit beta = 0.82 (high)
Betas$Belgium.Corporations[2]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Belgium~ECB1$DFR)
summary(lr) #deposit beta = 0.79 (high)
Betas$Belgium.Households[2]=lr$coefficients[2] 

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Belgium,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Belgium', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Belgium, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Belgium~ECB1$DFR)
summary(lr) #deposit beta = 0.47
Betas$Belgium.Corporations[3]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Belgium~ECB1$DFR)
summary(lr) #deposit beta = 0.59
Betas$Belgium.Households[3]=lr$coefficients[2] 

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Belgium,
     type='l', main='Deposits redeeemable at notice, Belgium', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Belgium~ECB1$DFR)
summary(lr) #deposit beta 0.014
Betas$Belgium.Households[4]=lr$coefficients[2] 

Betas

#CROATIA
d=read.csv('ECB Data Portal_Croatia.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Croatia', 'Overnight.Households.Croatia', 'AgreedMaturityUp1Y.Corporations.Croatia',
              'AgreedMaturityUp1Y.Households.Croatia','AgreedMaturityOver2Y.Corporations.Croatia','AgreedMaturityOver2Y.Households.Croatia',
              'RedeemableAtNotice.Croatia')
d1=d[d$DATE > as.Date("2022-07-01"), ]

plot(ECB$DATE,ECB$DFR, type='l', main='DFR', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Croatia,
     type='l', main='Deposits overnight, Croatia', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Croatia, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Croatia~ECB1$DFR)
summary(lr) #deposit beta = 0.015 (low). Adjusted R-squared:  0.475
Betas$Croatia.Corporations[1]=lr$coefficients[2] 
#households
lr=lm(d1$Overnight.Households.Croatia~ECB1$DFR)
summary(lr) #deposit beta = 0.003 (low)
Betas$Croatia.Households[1]=lr$coefficients[2] 


#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Croatia,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Croatia', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Croatia, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Croatia~ECB1$DFR)
summary(lr) #deposit beta = 0.87
Betas$Croatia.Corporations[2]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Croatia~ECB1$DFR)
summary(lr) #deposit beta = 0.55
Betas$Croatia.Households[2]=lr$coefficients[2] 

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Croatia,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Croatia', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Croatia, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Croatia~ECB1$DFR)
summary(lr) #deposit beta = 0.35 . Adjusted R-squared:  0.1929 is low
Betas$Croatia.Corporations[3]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Croatia~ECB1$DFR)
summary(lr) #deposit beta = 0.43
Betas$Croatia.Households[3]=lr$coefficients[2] 

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Croatia,
     type='l', main='Deposits redeeemable at notice, Croatia', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Croatia~ECB1$DFR) #missing data
summary(lr) #we can see that the model is not significant (p-value > .05)
Betas$Croatia.Households[4]=lr$coefficients[2] 

Betas

#CYPRUS
d=read.csv('ECB Data Portal_Cyprus.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Cyprus', 'Overnight.Households.Cyprus', 'AgreedMaturityUp1Y.Corporations.Cyprus',
              'AgreedMaturityUp1Y.Households.Cyprus','AgreedMaturityOver2Y.Corporations.Cyprus','AgreedMaturityOver2Y.Households.Cyprus',
              'RedeemableAtNotice.Cyprus')
d1=d[d$DATE > as.Date("2022-07-01"), ]

plot(ECB$DATE,ECB$DFR, type='l', main='DFR', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Cyprus,
     type='l', main='Deposits overnight, Cyprus', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Cyprus, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Cyprus~ECB1$DFR)
summary(lr) #deposit beta = 0.03
Betas$Cyprus.Corporations[1]=lr$coefficients[2] 
#households
lr=lm(d1$Overnight.Households.Cyprus~ECB1$DFR)
summary(lr)
Betas$Cyprus.Households[1]=lr$coefficients[2] 

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Cyprus,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Cyprus', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Cyprus, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Cyprus~ECB1$DFR)
summary(lr) #deposit beta = 0.52
Betas$Cyprus.Corporations[2]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Cyprus~ECB1$DFR)
summary(lr) #deposit beta = 0.43
Betas$Cyprus.Households[2]=lr$coefficients[2] 

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Cyprus,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Cyprus', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Cyprus, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Cyprus~ECB1$DFR) #no data
#summary(lr)
#Betas$Cyprus.Corporations[3]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Cyprus~ECB1$DFR) #no data
#summary(lr)
#Betas$Cyprus.Households[3]=lr$coefficients[2] 

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Cyprus,
     type='l', main='Deposits redeeemable at notice, Cyprus', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Cyprus~ECB1$DFR)
summary(lr) #deposit beta = 0.008 (low). Adjusted R-squared:  0.1653 (low)
#p-value = 0.0475 (still statistically significant with alpha = .05)
Betas$Cyprus.Households[4]=lr$coefficients[2] 

Betas

#ESTONIA
d=read.csv('ECB Data Portal_Estonia.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Estonia', 'Overnight.Households.Estonia', 'AgreedMaturityUp1Y.Corporations.Estonia',
              'AgreedMaturityUp1Y.Households.Estonia','AgreedMaturityOver2Y.Corporations.Estonia','AgreedMaturityOver2Y.Households.Estonia',
              'RedeemableAtNotice.Estonia')
d1=d[d$DATE > as.Date("2022-07-01"), ]

plot(ECB$DATE,ECB$DFR, type='l', main='DFR', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Estonia,
     type='l', main='Deposits overnight, Estonia', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Estonia, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Estonia~ECB1$DFR)
summary(lr) #deposit beta = 0.3
Betas$Estonia.Corporations[1]=lr$coefficients[2] 
#households
lr=lm(d1$Overnight.Households.Estonia~ECB1$DFR)
summary(lr) #deposit beta = 0.03 (low)
Betas$Estonia.Households[1]=lr$coefficients[2] 

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Estonia,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Estonia', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Estonia, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Estonia~ECB1$DFR)
summary(lr) #deposit beta = 0.9
Betas$Estonia.Corporations[2]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Estonia~ECB1$DFR)
summary(lr) #deposit beta = 0.87
Betas$Estonia.Households[2]=lr$coefficients[2] 

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Estonia,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Estonia', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Estonia, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Estonia~ECB1$DFR)
summary(lr) #deposit beta = 0.4. Adjusted R-squared:  0.382
Betas$Estonia.Corporations[3]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Estonia~ECB1$DFR)
summary(lr) #deposit beta = 0.5
Betas$Estonia.Households[3]=lr$coefficients[2] 

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Estonia,
     type='l', main='Deposits redeeemable at notice, Estonia', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Estonia~ECB1$DFR)
summary(lr) #deposit beta = 0.56
Betas$Estonia.Households[4]=lr$coefficients[2] 

Betas

#FINLAND
d=read.csv('ECB Data Portal_Finland.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Finland', 'Overnight.Households.Finland', 'AgreedMaturityUp1Y.Corporations.Finland',
              'AgreedMaturityUp1Y.Households.Finland','AgreedMaturityOver2Y.Corporations.Finland','AgreedMaturityOver2Y.Households.Finland',
              'RedeemableAtNotice.Finland')
d1=d[d$DATE > as.Date("2022-07-01"), ]

plot(ECB$DATE,ECB$DFR, type='l', main='DFR', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Finland,
     type='l', main='Deposits overnight, Finland', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Finland, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Finland~ECB1$DFR)
summary(lr)
Betas$Finland.Corporations[1]=lr$coefficients[2] 
#households
lr=lm(d1$Overnight.Households.Finland~ECB1$DFR)
summary(lr)
Betas$Finland.Households[1]=lr$coefficients[2] 

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Finland,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Finland', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Finland, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Finland~ECB1$DFR)
summary(lr)
Betas$Finland.Corporations[2]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Finland~ECB1$DFR)
summary(lr)
Betas$Finland.Households[2]=lr$coefficients[2] 

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Finland,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Finland', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Finland, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Finland~ECB1$DFR) #missing data
#summary(lr)
#Betas$Finland.Corporations[3]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Finland~ECB1$DFR)
summary(lr)
Betas$Finland.Households[3]=lr$coefficients[2] 

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Finland,
     type='l', main='Deposits redeeemable at notice, Finland', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Finland~ECB1$DFR)
summary(lr)
Betas$Finland.Households[4]=lr$coefficients[2] 

Betas

#FRANCE
d=read.csv('ECB Data Portal_France.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.France', 'Overnight.Households.France', 'AgreedMaturityUp1Y.Corporations.France',
              'AgreedMaturityUp1Y.Households.France','AgreedMaturityOver2Y.Corporations.France','AgreedMaturityOver2Y.Households.France',
              'RedeemableAtNotice.France')
d1=d[d$DATE > as.Date("2022-07-01"), ]

plot(ECB$DATE,ECB$DFR, type='l', main='DFR', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.France,
     type='l', main='Deposits overnight, France', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.France, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.France~ECB1$DFR)
summary(lr)
Betas$France.Corporations[1]=lr$coefficients[2] 
#households
lr=lm(d1$Overnight.Households.France~ECB1$DFR)
summary(lr)
Betas$France.Households[1]=lr$coefficients[2] 

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.France,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, France', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.France, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.France~ECB1$DFR)
summary(lr)
Betas$France.Corporations[2]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.France~ECB1$DFR)
summary(lr)
Betas$France.Households[2]=lr$coefficients[2] 

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.France,
     type='l', main='Deposits with an agreed Maturity of over 2 years, France', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.France, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.France~ECB1$DFR)
summary(lr)
Betas$France.Corporations[3]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.France~ECB1$DFR)
summary(lr)
Betas$France.Households[3]=lr$coefficients[2] 

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.France,
     type='l', main='Deposits redeeemable at notice, France', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.France~ECB1$DFR)
summary(lr)
Betas$France.Households[4]=lr$coefficients[2] 

Betas

#GERMANY
d=read.csv('ECB Data Portal_Germany.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Germany', 'Overnight.Households.Germany', 'AgreedMaturityUp1Y.Corporations.Germany',
              'AgreedMaturityUp1Y.Households.Germany','AgreedMaturityOver2Y.Corporations.Germany','AgreedMaturityOver2Y.Households.Germany',
              'RedeemableAtNotice.Germany')
d1=d[d$DATE > as.Date("2022-07-01"), ]

plot(ECB$DATE,ECB$DFR, type='l', main='DFR', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Germany,
     type='l', main='Deposits overnight, Germany', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Germany, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Germany~ECB1$DFR)
summary(lr)
Betas$Germany.Corporations[1]=lr$coefficients[2] 
#households
lr=lm(d1$Overnight.Households.Germany~ECB1$DFR)
summary(lr)
Betas$Germany.Households[1]=lr$coefficients[2] 

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Germany,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Germany', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Germany, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Germany~ECB1$DFR)
summary(lr)
Betas$Germany.Corporations[2]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Germany~ECB1$DFR)
summary(lr)
Betas$Germany.Households[2]=lr$coefficients[2] 

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Germany,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Germany', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Germany, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Germany~ECB1$DFR)
summary(lr)
Betas$Germany.Corporations[3]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Germany~ECB1$DFR)
summary(lr)
Betas$Germany.Households[3]=lr$coefficients[2] 

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Germany,
     type='l', main='Deposits redeeemable at notice, Germany', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Germany~ECB1$DFR)
summary(lr)
Betas$Germany.Households[4]=lr$coefficients[2] 

Betas

#GREECE
d=read.csv('ECB Data Portal_Greece.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Greece', 'Overnight.Households.Greece', 'AgreedMaturityUp1Y.Corporations.Greece',
              'AgreedMaturityUp1Y.Households.Greece','AgreedMaturityOver2Y.Corporations.Greece','AgreedMaturityOver2Y.Households.Greece',
              'RedeemableAtNotice.Greece')
d1=d[d$DATE > as.Date("2022-07-01"), ]

plot(ECB$DATE,ECB$DFR, type='l', main='DFR', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Greece,
     type='l', main='Deposits overnight, Greece', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Greece, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Greece~ECB1$DFR)
summary(lr)
Betas$Greece.Corporations[1]=lr$coefficients[2] 
#households
lr=lm(d1$Overnight.Households.Greece~ECB1$DFR)
summary(lr)
Betas$Greece.Households[1]=lr$coefficients[2] 

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Greece,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Greece', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Greece, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Greece~ECB1$DFR)
summary(lr)
Betas$Greece.Corporations[2]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Greece~ECB1$DFR)
summary(lr)
Betas$Greece.Households[2]=lr$coefficients[2] 

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Greece,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Greece', ylim=c(0,5), xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Greece, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Greece~ECB1$DFR) #missing data
#summary(lr)
#Betas$Greece.Corporations[3]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Greece~ECB1$DFR) #missing data
#summary(lr)
#Betas$Greece.Households[3]=lr$coefficients[2] 

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Greece,
     type='l', main='Deposits redeeemable at notice, Greece', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Greece~ECB1$DFR) #missing data
#summary(lr)
#Betas$Greece.Households[4]=lr$coefficients[2] 

Betas

#IRELAND
d=read.csv('ECB Data Portal_Ireland.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Ireland', 'Overnight.Households.Ireland', 'AgreedMaturityUp1Y.Corporations.Ireland',
              'AgreedMaturityUp1Y.Households.Ireland','AgreedMaturityOver2Y.Households.Ireland',
              'RedeemableAtNotice.Ireland')
d1=d[d$DATE > as.Date("2022-07-01"), ]

plot(ECB$DATE,ECB$DFR, type='l', main='DFR', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Ireland,
     type='l', main='Deposits overnight, Ireland', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Ireland, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Ireland~ECB1$DFR)
summary(lr)
Betas$Ireland.Corporations[1]=lr$coefficients[2] 
#households
lr=lm(d1$Overnight.Households.Ireland~ECB1$DFR)
summary(lr)
Betas$Ireland.Households[1]=lr$coefficients[2] 

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Ireland,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Ireland', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Ireland, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Ireland~ECB1$DFR)
summary(lr)
Betas$Ireland.Corporations[2]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Ireland~ECB1$DFR)
summary(lr)
Betas$Ireland.Households[2]=lr$coefficients[2] 

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Households.Ireland,
     type='l', main='Deposits with an agreed Maturity of over 2 years Households, Ireland', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
#corporations...MISSING DATA
#lr=lm(d1$AgreedMaturityOver2Y.Corporations.Ireland~ECB1$DFR)
#summary(lr)
#Betas$Ireland.Corporations[3]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Ireland~ECB1$DFR)
summary(lr)
Betas$Ireland.Households[3]=lr$coefficients[2] 

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Ireland,
     type='l', main='Deposits redeeemable at notice, Ireland', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Ireland~ECB1$DFR)
summary(lr)
Betas$Ireland.Households[4]=lr$coefficients[2] 

Betas


#ITALY
d=read.csv('ECB Data Portal_Italy.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Italy', 'Overnight.Households.Italy', 'AgreedMaturityUp1Y.Corporations.Italy',
              'AgreedMaturityUp1Y.Households.Italy','AgreedMaturityOver2Y.Corporations.Italy','AgreedMaturityOver2Y.Households.Italy',
              'RedeemableAtNotice.Italy','RedeemableAtNoticeOver3M.Italy')
d1=d[d$DATE > as.Date("2022-07-01"), ]

plot(ECB$DATE,ECB$DFR, type='l', main='DFR', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Italy,
       type='l', main='Deposits overnight, Italy', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Italy, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Italy~ECB1$DFR)
summary(lr)
Betas$Italy.Corporations[1]=lr$coefficients[2] #0.227
#households
lr=lm(d1$Overnight.Households.Italy~ECB1$DFR)
summary(lr)
Betas$Italy.Households[1]=lr$coefficients[2] #0.0817

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Italy,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Italy', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Italy, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Italy~ECB1$DFR)
summary(lr)
Betas$Italy.Corporations[2]=lr$coefficients[2] #0.874
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Italy~ECB1$DFR)
summary(lr)
Betas$Italy.Households[2]=lr$coefficients[2] #0.743

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Italy,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Italy', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Italy, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Italy~ECB1$DFR)
summary(lr)
Betas$Italy.Corporations[3]=lr$coefficients[2] #0.541
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Italy~ECB1$DFR)
summary(lr)
Betas$Italy.Households[3]=lr$coefficients[2] #0.62

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Italy,
     type='l', main='Deposits redeeemable at notice, Italy', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Italy~ECB1$DFR)
summary(lr)
Betas$Italy.Households[4]=lr$coefficients[2] #0.037

Betas


#LATVIA
d=read.csv('ECB Data Portal_Latvia.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Latvia', 'Overnight.Households.Latvia', 'AgreedMaturityUp1Y.Corporations.Latvia',
              'AgreedMaturityUp1Y.Households.Latvia','AgreedMaturityOver2Y.Corporations.Latvia','AgreedMaturityOver2Y.Households.Latvia',
              'RedeemableAtNotice.Latvia')
d1=d[d$DATE > as.Date("2022-07-01"), ]

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Latvia,
     type='l', main='Deposits overnight, Latvia', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Latvia, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Latvia~ECB1$DFR)
summary(lr)
Betas$Latvia.Corporations[1]=lr$coefficients[2] #0.152
#households
lr=lm(d1$Overnight.Households.Latvia~ECB1$DFR)
summary(lr)
Betas$Latvia.Households[1]=lr$coefficients[2] #0.0667

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Latvia,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Latvia', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Latvia, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Latvia~ECB1$DFR)
summary(lr)
Betas$Latvia.Corporations[2]=lr$coefficients[2] #0.863
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Latvia~ECB1$DFR)
summary(lr)
Betas$Latvia.Households[2]=lr$coefficients[2] #0.862

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Latvia,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Latvia', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Latvia, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Latvia~ECB1$DFR)
summary(lr)
Betas$Latvia.Corporations[3]=lr$coefficients[2] #0.572
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Latvia~ECB1$DFR)
summary(lr)
Betas$Latvia.Households[3]=lr$coefficients[2] #0.723

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Latvia,
     type='l', main='Deposits redeeemable at notice, Latvia', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Latvia~ECB1$DFR)
summary(lr)
Betas$Latvia.Households[4]=lr$coefficients[2] #0.0634

Betas


#LITHUANIA
d=read.csv('ECB Data Portal_Lithuania.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Lithuania', 'Overnight.Households.Lithuania', 'AgreedMaturityUp1Y.Corporations.Lithuania',
              'AgreedMaturityUp1Y.Households.Lithuania','AgreedMaturityOver2Y.Corporations.Lithuania','AgreedMaturityOver2Y.Households.Lithuania',
              'RedeemableAtNotice.Lithuania')
d1=d[d$DATE > as.Date("2022-07-01"), ]

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Lithuania,
     type='l', main='Deposits overnight, Lithuania', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Lithuania, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Lithuania~ECB1$DFR)
summary(lr)
Betas$Lithuania.Corporations[1]=lr$coefficients[2] #0.187
#households
lr=lm(d1$Overnight.Households.Lithuania~ECB1$DFR)
summary(lr)
Betas$Lithuania.Households[1]=lr$coefficients[2] #0.035

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Lithuania,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Lithuania', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Lithuania, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Lithuania~ECB1$DFR)
summary(lr)
Betas$Lithuania.Corporations[2]=lr$coefficients[2] #0.905
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Lithuania~ECB1$DFR)
summary(lr)
Betas$Lithuania.Households[2]=lr$coefficients[2] #0.875

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Lithuania,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Lithuania', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Lithuania, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Lithuania~ECB1$DFR)
summary(lr)
Betas$Lithuania.Corporations[3]=lr$coefficients[2] #0.347
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Lithuania~ECB1$DFR)
summary(lr)
Betas$Lithuania.Households[3]=lr$coefficients[2] #0.642

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Lithuania,
     type='l', main='Deposits redeeemable at notice, Lithuania', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Lithuania~ECB1$DFR)
summary(lr)
Betas$Lithuania.Households[4]=lr$coefficients[2] #0.141

Betas


#LUXEMBOURG
d=read.csv('ECB Data Portal_Luxembourg.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Luxembourg', 'Overnight.Households.Luxembourg', 'AgreedMaturityUp1Y.Corporations.Luxembourg',
              'AgreedMaturityUp1Y.Households.Luxembourg','AgreedMaturityOver2Y.Corporations.Luxembourg','AgreedMaturityOver2Y.Households.Luxembourg',
              'RedeemableAtNotice.Luxembourg')
d1=d[d$DATE > as.Date("2022-07-01"), ]

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Luxembourg,
     type='l', main='Deposits overnight, Luxembourg', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Luxembourg, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Luxembourg~ECB1$DFR)
summary(lr)
Betas$Luxembourg.Corporations[1]=lr$coefficients[2] #0.180
#households
lr=lm(d1$Overnight.Households.Luxembourg~ECB1$DFR)
summary(lr)
Betas$Luxembourg.Households[1]=lr$coefficients[2] #0.374

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Luxembourg,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Luxembourg', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Luxembourg, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Luxembourg~ECB1$DFR)
summary(lr)
Betas$Luxembourg.Corporations[2]=lr$coefficients[2] #0.845
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Luxembourg~ECB1$DFR)
summary(lr)
Betas$Luxembourg.Households[2]=lr$coefficients[2] #0.751

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Luxembourg,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Luxembourg', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Luxembourg, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Luxembourg~ECB1$DFR) #error, there are only NA
#summary(lr)
#Betas$Luxembourg.Corporations[3]=lr$coefficients[2] 
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Luxembourg~ECB1$DFR)
summary(lr)
Betas$Luxembourg.Households[3]=lr$coefficients[2] #0.475

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Luxembourg,
     type='l', main='Deposits redeeemable at notice, Luxembourg', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Luxembourg~ECB1$DFR) #error, there are only NA
#summary(lr)
#Betas$Luxembourg.Households[4]=lr$coefficients[2] 

Betas


#MALTA
d=read.csv('ECB Data Portal_Malta.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Malta', 'Overnight.Households.Malta', 'AgreedMaturityUp1Y.Corporations.Malta',
              'AgreedMaturityUp1Y.Households.Malta','AgreedMaturityOver2Y.Corporations.Malta','AgreedMaturityOver2Y.Households.Malta',
              'RedeemableAtNotice.Malta')
d1=d[d$DATE > as.Date("2022-07-01"), ]

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Malta,
     type='l', main='Deposits overnight, Malta', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Malta, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Malta~ECB1$DFR)
summary(lr)
Betas$Malta.Corporations[1]=lr$coefficients[2] #0.015
#households
lr=lm(d1$Overnight.Households.Malta~ECB1$DFR)
summary(lr)
Betas$Malta.Households[1]=lr$coefficients[2] #0.0075

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Malta,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Malta', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Malta, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Malta~ECB1$DFR)
summary(lr)
Betas$Malta.Corporations[2]=lr$coefficients[2] #0.287
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Malta~ECB1$DFR)
summary(lr)
Betas$Malta.Households[2]=lr$coefficients[2] #0.764

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Malta,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Malta', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Malta, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Malta~ECB1$DFR)
summary(lr)
Betas$Malta.Corporations[3]=lr$coefficients[2] #1.23, !!some NA, beta isn't accurate
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Malta~ECB1$DFR)
summary(lr)
Betas$Malta.Households[3]=lr$coefficients[2] #0.174

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Malta,
     type='l', main='Deposits redeeemable at notice, Malta', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Malta~ECB1$DFR) #error, there are only NA
#summary(lr)
#Betas$Malta.Households[4]=lr$coefficients[2] 

Betas


#NETHERLANDS
d=read.csv('./ECB Data Portal_Netherlands.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Netherlands', 'Overnight.Households.Netherlands', 'AgreedMaturityUp1Y.Corporations.Netherlands',
              'AgreedMaturityUp1Y.Households.Netherlands','AgreedMaturityOver2Y.Corporations.Netherlands','AgreedMaturityOver2Y.Households.Netherlands',
              'RedeemableAtNotice.Netherlands')
d1=d[d$DATE > as.Date("2022-07-01"), ]

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Netherlands,
     type='l', main='Deposits overnight, Netherlands', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Netherlands, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Netherlands~ECB1$DFR)
summary(lr)
Betas$Netherlands.Corporations[1]=lr$coefficients[2] #0.35
#households
lr=lm(d1$Overnight.Households.Netherlands~ECB1$DFR)
summary(lr)
Betas$Netherlands.Households[1]=lr$coefficients[2] #0.044

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Netherlands,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Netherlands', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Netherlands, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Netherlands~ECB1$DFR)
summary(lr)
Betas$Netherlands.Corporations[2]=lr$coefficients[2] #0.77
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Netherlands~ECB1$DFR)
summary(lr)
Betas$Netherlands.Households[2]=lr$coefficients[2] #0.35

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Netherlands,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Netherlands', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Netherlands, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Netherlands~ECB1$DFR)
summary(lr)
Betas$Netherlands.Corporations[3]=lr$coefficients[2] #0.76
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Netherlands~ECB1$DFR)
summary(lr)
Betas$Netherlands.Households[3]=lr$coefficients[2] #0.33

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Netherlands,
     type='l', main='Deposits redeeemable at notice, Netherlands', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Netherlands~ECB1$DFR)
summary(lr)
Betas$Netherlands.Households[4]=lr$coefficients[2] # 0.38

Betas


#PORTUGAL
d=read.csv('./ECB Data Portal_Portugal.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Portugal', 'Overnight.Households.Portugal', 'AgreedMaturityUp1Y.Corporations.Portugal',
              'AgreedMaturityUp1Y.Households.Portugal','AgreedMaturityOver2Y.Corporations.Portugal','AgreedMaturityOver2Y.Households.Portugal',
              'RedeemableAtNotice.Portugal')
d1=d[d$DATE > as.Date("2022-07-01"), ]

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Portugal,
     type='l', main='Deposits overnight, Portugal', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Portugal, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Portugal~ECB1$DFR)
summary(lr)
Betas$Portugal.Corporations[1]=lr$coefficients[2] #0.026
#households
lr=lm(d1$Overnight.Households.Portugal~ECB1$DFR)
summary(lr)
Betas$Portugal.Households[1]=lr$coefficients[2] #0.003 !! not significative

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Portugal,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Portugal', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Portugal, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Portugal~ECB1$DFR)
summary(lr)
Betas$Portugal.Corporations[2]=lr$coefficients[2] #0.86
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Portugal~ECB1$DFR)
summary(lr)
Betas$Portugal.Households[2]=lr$coefficients[2] #0.68

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Portugal,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Portugal', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Portugal, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Portugal~ECB1$DFR)
summary(lr)
Betas$Portugal.Corporations[3]=lr$coefficients[2] #0.60
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Portugal~ECB1$DFR)
summary(lr)
Betas$Portugal.Households[3]=lr$coefficients[2] #0.59

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Portugal,
     type='l', main='Deposits redeeemable at notice, Portugal', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Portugal~ECB1$DFR) #error, there are only NA
#summary(lr)
#Betas$Portugal.Households[4]=lr$coefficients[2] 

Betas


#SLOVAKIA
d=read.csv('./ECB Data Portal_Slovakia.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Slovakia', 'Overnight.Households.Slovakia', 'AgreedMaturityUp1Y.Corporations.Slovakia',
              'AgreedMaturityUp1Y.Households.Slovakia','AgreedMaturityOver2Y.Corporations.Slovakia','AgreedMaturityOver2Y.Households.Slovakia',
              'RedeemableAtNotice.Slovakia')
d1=d[d$DATE > as.Date("2022-07-01"), ]

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Slovakia,
     type='l', main='Deposits overnight, Slovakia', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Slovakia, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Slovakia~ECB1$DFR)
summary(lr)
Betas$Slovakia.Corporations[1]=lr$coefficients[2] #0.09
#households
lr=lm(d1$Overnight.Households.Slovakia~ECB1$DFR)
summary(lr)
Betas$Slovakia.Households[1]=lr$coefficients[2] #0.008

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Slovakia,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Slovakia', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Slovakia, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Slovakia~ECB1$DFR)
summary(lr)
Betas$Slovakia.Corporations[2]=lr$coefficients[2] #0.86
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Slovakia~ECB1$DFR)
summary(lr)
Betas$Slovakia.Households[2]=lr$coefficients[2] #0.78

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Slovakia,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Slovakia', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Slovakia, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Slovakia~ECB1$DFR)
summary(lr)
Betas$Slovakia.Corporations[3]=lr$coefficients[2] #0.58, !not very signicative
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Slovakia~ECB1$DFR)
summary(lr)
Betas$Slovakia.Households[3]=lr$coefficients[2] #0.34

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Slovakia,
     type='l', main='Deposits redeeemable at notice, Slovakia', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Slovakia~ECB1$DFR) 
summary(lr)
Betas$Slovakia.Households[4]=lr$coefficients[2] #0.25

Betas


#SLOVENIA
d=read.csv('./ECB Data Portal_Slovenia.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Slovenia', 'Overnight.Households.Slovenia', 'AgreedMaturityUp1Y.Corporations.Slovenia',
              'AgreedMaturityUp1Y.Households.Slovenia','AgreedMaturityOver2Y.Corporations.Slovenia','AgreedMaturityOver2Y.Households.Slovenia',
              'RedeemableAtNotice.Slovenia')
d1=d[d$DATE > as.Date("2022-07-01"), ]

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Slovenia,
     type='l', main='Deposits overnight, Slovenia', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Slovenia, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Slovenia~ECB1$DFR)
summary(lr)
Betas$Slovenia.Corporations[1]=lr$coefficients[2] #0.003
#households
lr=lm(d1$Overnight.Households.Slovenia~ECB1$DFR)
summary(lr)
Betas$Slovenia.Households[1]=lr$coefficients[2] #0.034

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Slovenia,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Slovenia', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Slovenia, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Slovenia~ECB1$DFR)
summary(lr)
Betas$Slovenia.Corporations[2]=lr$coefficients[2] #0.61
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Slovenia~ECB1$DFR)
summary(lr)
Betas$Slovenia.Households[2]=lr$coefficients[2] #0.32

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Slovenia,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Slovenia', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Slovenia, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Slovenia~ECB1$DFR)
summary(lr)
Betas$Slovenia.Corporations[3]=lr$coefficients[2] #0.45, !!not significative
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Slovenia~ECB1$DFR)
summary(lr)
Betas$Slovenia.Households[3]=lr$coefficients[2] #0.25 !! not significative

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Slovenia,
     type='l', main='Deposits redeeemable at notice, Slovenia', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Slovenia~ECB1$DFR) 
summary(lr)
Betas$Slovenia.Households[4]=lr$coefficients[2] # 0.73 

Betas

#SPAIN
d=read.csv('./ECB Data Portal_Spain.csv')
d$DATE=as.Date(d$DATE)
#creation of a new df with matching dates
ECB=inner_join(ECB_daily, d, by='DATE')[,1:5]
colnames(ECB)=c(colnames(ECB)[1:2], 'DFR', 'MLF', 'MRO')
#period from July 2022
ECB1=ECB[ECB$DATE > as.Date("2022-07-01"), ]
colnames(d)=c(colnames(d)[1:2], 'Overnight.Corporations.Spain', 'Overnight.Households.Spain', 'AgreedMaturityUp1Y.Corporations.Spain',
              'AgreedMaturityUp1Y.Households.Spain','AgreedMaturityOver2Y.Corporations.Spain','AgreedMaturityOver2Y.Households.Spain',
              'RedeemableAtNotice.Spain')
d1=d[d$DATE > as.Date("2022-07-01"), ]

#OVERNIGHT
plot(d$DATE, d$Overnight.Corporations.Spain,
     type='l', main='Deposits overnight, Spain', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$Overnight.Households.Spain, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('topright',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$Overnight.Corporations.Spain~ECB1$DFR)
summary(lr)
Betas$Spain.Corporations[1]=lr$coefficients[2] #0.15
#households
lr=lm(d1$Overnight.Households.Spain~ECB1$DFR)
summary(lr)
Betas$Spain.Households[1]=lr$coefficients[2] #0.03

#AGREED MATURITY OF UP TO 1 YEAR
plot(d$DATE, d$AgreedMaturityUp1Y.Corporations.Spain,
     type='l', main='Deposits with an agreed Maturity of up to 1 year, Spain', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityUp1Y.Households.Spain, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityUp1Y.Corporations.Spain~ECB1$DFR)
summary(lr)
Betas$Spain.Corporations[2]=lr$coefficients[2] #0.86
#households
lr=lm(d1$AgreedMaturityUp1Y.Households.Spain~ECB1$DFR)
summary(lr)
Betas$Spain.Households[2]=lr$coefficients[2] #0.66

#AGREED MATURITY OF OVER 2 YEARS
plot(d$DATE, d$AgreedMaturityOver2Y.Corporations.Spain,
     type='l', main='Deposits with an agreed Maturity of over 2 years, Spain', xlab='Date (by month)', ylab='Interest rate (%)')
lines(d$DATE,d$AgreedMaturityOver2Y.Households.Spain, col='blue')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
legend('bottomleft',legend=c('Corporations','Households'), fill=c('black','blue'))
#deposit beta from July 2022
#corporations
lr=lm(d1$AgreedMaturityOver2Y.Corporations.Spain~ECB1$DFR)
summary(lr)
Betas$Spain.Corporations[3]=lr$coefficients[2] # -0.06, !!negative, but not significative
#households
lr=lm(d1$AgreedMaturityOver2Y.Households.Spain~ECB1$DFR)
summary(lr)
Betas$Spain.Households[3]=lr$coefficients[2] #0.27

#REDEEMABLE AT NOTICE
plot(d$DATE, d$RedeemableAtNotice.Spain,
     type='l', main='Deposits redeeemable at notice, Spain', xlab='Date (by month)', ylab='Interest rate (%)')
abline(v=as.Date('2022-07-31'), col='red', lty=2)  
#deposit beta from July 2022
lr=lm(d1$RedeemableAtNotice.Spain~ECB1$DFR) #error, there are only NA
#summary(lr)
#Betas$Spain.Households[4]=lr$coefficients[2] 

Betas

write.csv(Betas, file = "Betas.csv", row.names = FALSE)


#Analysis on betas

#Transposition of the matrix
a = t(Betas)[-1,]
row.names(a) <- gsub('.Households','', gsub('.Corporations','',row.names(a)))

#OVERNIGHT
a1 = as.numeric(unlist(a[, 1]))
#Selection of only Corporations
a_c=a1[c(TRUE, FALSE)]
#Barplot
barplot(a_c, main = "Beta of deposits overnight for corporations", ylab = "Value", col = "skyblue", ylim = c(0, 0.5), names.arg = row.names(a)[c(TRUE, FALSE)], las = 2)
#Selection of only Households
a_h=a1[c(FALSE,TRUE)]
#Barplot
barplot(a_h, main = "Beta of deposits overnight for Households", ylab = "Value", col = "skyblue", ylim = c(0, 0.5), names.arg = row.names(a)[c(FALSE, TRUE)], las = 2)

#AGREED MATURITY OF UP TO 1 YEAR
a1 = as.numeric(unlist(a[, 2]))
#Selection of only Corporations
a_c=a1[c(TRUE, FALSE)]
#Barplot
barplot(a_c, main = "Beta of deposits with an agreed maturity of up to 1 year for corporations", ylab = "Value", col = "skyblue", ylim = c(0, 1), names.arg = row.names(a)[c(TRUE, FALSE)], las = 2)
#Selection of only Households
a_h=a1[c(FALSE,TRUE)]
#Barplot
barplot(a_h, main = "Beta of deposits with an agreed maturity of up to 1 year for Households", ylab = "Value", col = "skyblue", ylim = c(0, 1), names.arg = row.names(a)[c(FALSE, TRUE)], las = 2)

#AGREED MATURITY OF OVER 2 YEARS
a1 = as.numeric(unlist(a[, 3]))
#Selection of only Corporations
a_c=a1[c(TRUE, FALSE)]
#Barplot
barplot(a_c, main = "Beta of deposits with an agreed maturity of over 2 years for corporations", ylab = "Value", col = "skyblue", ylim = c(-0.1, 1.5), names.arg = row.names(a)[c(TRUE, FALSE)], las = 2)
#Selection of only Households
a_h=a1[c(FALSE,TRUE)]
#Barplot
barplot(a_h, main = "Beta of deposits with an agreed Maturity of over 2 years for Households", ylab = "Value", col = "skyblue", ylim = c(0, 1), names.arg = row.names(a)[c(FALSE, TRUE)], las = 2)

#REDEEMABLE AT NOTICE
a1 = as.numeric(unlist(a[, 4]))
a_h=a1[c(FALSE,TRUE)]
#Barplot
barplot(a_h, main = "Beta of deposits redeemable at notice", ylab = "Value", col = "skyblue", ylim = c(0, 1), names.arg = row.names(a)[c(FALSE, TRUE)], las = 2)


