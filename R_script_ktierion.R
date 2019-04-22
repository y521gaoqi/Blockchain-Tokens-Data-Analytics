## Read in the BAT token
library(magrittr)
library(dplyr)
library(ggplot2)
library(readr)
library(fitdistrplus)
library(DAAG)
bat <- read_delim('C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/Blockchain-Tokens-Data-Analytics/networktierionTX.txt', delim = " ", col_names = F)
##View(bat)
names(bat) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')
decimals <- 10^8
supply <- 1 * 10^9
dim(bat)

batFiltered <-filter(bat,tokenAmount < decimals * supply)
dim(batFiltered)
batFiltered
summarise(batFiltered)
bat_filteredout<- filter(bat,tokenAmount >= decimals * supply) 
bat_filteredout
dim(bat_filteredout)

#figure out how many users indruced those unnormal transcation
user_outliers <- bat_filteredout %>% group_by(toID) %>% summarise(n = n()) %>% ungroup
dim(user_outliers)
user_outliers
number_users_outliers<-nrow(user_outliers)
number_users_outliers

#Buyers Data
buys<-batFiltered%>% group_by(toID) %>% summarise(n = n()) %>% ungroup #change the supply and decimals amount
dim(buys)
buys
buys_sorted_dec<-buys[order(-buys$n),]
buys_sorted_dec
buys_sorted_asc<-buys[order(buys$n),]
buys_sorted_asc
View(buys_sorted_asc)

#top 10 active buyers
top_10_buyers<-buys_sorted_dec%>%head(10) #sort by decending order and get top 10
top_10_buyers

top_30_buyers<-buys_sorted_dec%>%head(30) #sort by decending order and get top 30

top_100_buyers<-buys_sorted_dec%>%head(100) #sort by decending order and get top 100


buys_less_30<-subset(buys_sorted_asc,n<30)
View(buys_less_30)

group_users<-buys_sorted_dec%>% group_by(n) %>% summarise(amount_users = n()) %>% ungroup #change the supply and decimals amount
group_users


############

#plot the density histgram
his_density<-hist(buys_less_30$n,breaks=20, freq=FALSE,col="red")
a <- 1/(mean(buys_sorted_asc$n))
t<- min(buys_sorted_asc$n):(max(buys_sorted_asc$n)+2)
lines(t,dexp(t,a),col="blue")

plot<-ggplot(buys_less_30, aes(x=n, y=..density..)) +geom_histogram(bins=30,color="black", fill="lightblue")+labs(x="Value", y = "Density")
print(plot)

######################

#try different distribution


exp_dis <- fitdist(buys_less_30$n, 'exp')
exp_dis
gamma_dis <- fitdist(buys_less_30$n, 'gamma')
gamma_dis
lnorm_dis <- fitdist(buys_less_30$n, 'lnorm')
lnorm_dis
gofstat(list(exp_dis, gamma_dis, lnorm_dis))
descdist(buys_sorted_asc$n,boot=1000)

#lognorm
fit_lnorm <- fitdist(buys_less_30$n,"lnorm")
summary(fit_lnorm)
plot(fit_lnorm)
cdfcomp(fit_lnorm)

#exp
fit_exp <- fitdist(buys_less_30$n,"exp")
summary(fit_exp)
plot(fit_exp)
cdfcomp(fit_exp)

#gamma
fit_gamma <- fitdist(buys_less_30$n,"gamma")
summary(fit_gamma)
plot(fit_gamma)
cdfcomp(fit_gamma)

#unif
fit_unif <- fitdist(buys_less_30$n,"unif")
summary(fit_unif)
plot(fit_unif)
cdfcomp(fit_unif)


#fitp <- fitdist(buys_less_30$n,"unif");cdfcomp(list(fitp,fitnb))
#lines(density(buys_sorted_asc$n))
###########################

#####################Plot Distribution curves##############################

#single user
library("ggplot2")
all_density <- ggplot(data=buys_less_30) + 
  geom_histogram(bins=30,aes(x = buys_less_30$n, ..density..)) + 
  stat_function(fun = dlnorm, args = list(meanlog = 0.3797812, sdlog = 0.6111954), 
                colour = "red")+
  stat_function(fun = dgamma, args = list(shape = 2.032322, rate=1.065791), 
                colour = "blue")+
  stat_function(fun=dexp, args=list(rate=0.5244071),colour="green")
all_density

#a pair of users
batFiltered_pairs<-batFiltered%>% group_by(fromID,toID) %>% summarise(n = n()) %>% ungroup 
View(batFiltered_pairs)

############################################################################



##############################Question 2####################################
tierion_prices <- read_delim("C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/Blockchain-Tokens-Data-Analytics/tierion", delim = "\t", col_names = T) #load token price data
names(tierion_prices) <- make.names(names(tierion_prices))
tierion_prices
tierion_prices <- tierion_prices %>% mutate(date = as.Date(Date, format = '%m/%d/%Y'))
tierion_prices
tierion <- read_delim('C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/Blockchain-Tokens-Data-Analytics/networktierionTX.txt', delim = " ", col_names = F)
names(tierion) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')
decimals <- 10^8
supply <- 1 * 10^9
dim(tierion)

tierion_filtered <-filter(tierion,tokenAmount < decimals * supply)
dim(tierion_filtered)
View(tierion_filtered)
str(tierion_filtered)

library(anytime)
## convert data type of unixTime
tierion_filtered <- tierion_filtered %>%
  mutate(date = anydate(unixTime))
names(tierion_filtered) <- c('fromID', 'toID', 'unixTime', 'tokenAmount', 'date')
str(tierion_filtered)
tierion_filtered

## merge the prices and edge
tierion_merged<-merge(x = tierion_prices, y = tierion_filtered, by = "date", all.x = TRUE)
View(tierion_merged)

filter_K_tierion_merged<-filter(tierion_merged,toID %in% top_10_buyers$toID)
View(filter_K_tierion_merged)
View(top_100_buyers$toID)

#get average price from open and close price and add it as a new column
filter_K_tierion_merged=transform(filter_K_tierion_merged,average_price= (Open+Close)/2)
View(filter_K_tierion_merged)
filered<-filter_K_tierion_merged%>% group_by(toID) %>% summarise(n = n(),average_price=mean(average_price)) %>% ungroup 
View(filered)

regression<-lm(filered$average_price~filered$n)
regression
par(mfcol=c(2,2))
plot(regression)
summary(regression)  #explaination of detail result: https://blog.csdn.net/lilanfeng1991/article/details/29627405\
par(mfcol=c(1,1))
plot(filered$n,filered$average_price)
abline(regression)
############################################################################







