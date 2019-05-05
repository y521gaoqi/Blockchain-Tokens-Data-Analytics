library(magrittr)
library(dplyr)
library(ggplot2)
library(readr)
library(fitdistrplus)
library(DAAG)
library("ggplot2")
library(anytime)

tierion <- read_delim('C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/Blockchain-Tokens-Data-Analytics/networktierionTX.txt', delim = " ", col_names = F)
names(tierion) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')
decimals <- 10^8
supply <- 1 * 10^9
tierionFiltered <-filter(tierion,tokenAmount < decimals * supply)  #filter out all outliers

#figure out how many users indruced those unnormal transcation
tierion_outliers<- filter(tierion,tokenAmount >= decimals * supply) 
user_outliers <- tierion_outliers %>% group_by(toID) %>% summarise(n = n()) %>% ungroup
number_users_outliers<-nrow(user_outliers)
number_users_outliers

#get top X buyers data
buys<-tierionFiltered%>% group_by(toID) %>% summarise(n = n()) %>% ungroup #change the supply and decimals amount
buys_sorted_dec<-buys[order(-buys$n),]
#top 30 active buyers and number of buys
top_30_buyers<-buys_sorted_dec%>%head(30) 
top_30_buyers

########################################Question 1############################################

#####group by user pairs#####
buys_pairs<-tierionFiltered%>% group_by(fromID, toID) %>% summarise(n = n()) %>% ungroup 
for (row in 1:nrow(buys_pairs)) {
  a<-buys_pairs[row,"fromID"]
  b<-buys_pairs[row,"toID"]
  for (inner_row in row:nrow(buys_pairs)) {
    c<-buys_pairs[inner_row,"fromID"]
    d<-buys_pairs[inner_row,"toID"]
    if(a==d&&b==c){
      buys_pairs[inner_row,"fromID"]<-d
      buys_pairs[inner_row,"toID"]<-c
    }
  }
}
buys_pairs<-tierionFiltered%>% group_by(fromID*toID+fromID+toID) %>% summarise(n = n()) %>% ungroup 
buys_pairs<-tierionFiltered%>% group_by(fromID, toID) %>% summarise(n = n()) %>% ungroup
buys_pair_sorted_asc<-buys_pairs[order(buys_pairs$n),]
buys_pair_less_30<-subset(buys_pair_sorted_asc,n<30)

#####find out estimates of paramaters of several distribution based on the buys_pairs data set#####
exp_dis <- fitdist(buys_pair_less_30$n, 'exp')
exp_dis
gamma_dis <- fitdist(buys_pair_less_30$n, 'gamma')
gamma_dis
lnorm_dis <- fitdist(buys_pair_less_30$n, 'lnorm')
lnorm_dis
pois_dis <- fitdist(buys_pair_less_30$n, 'pois')
pois_dis
weibull_dis <- fitdist(buys_pair_less_30$n, 'weibull')
weibull_dis

gofstat(list(exp_dis, gamma_dis, lnorm_dis,pois_dis,weibull_dis))
descdist(buys_pair_less_30$n,boot=1000)

#lognorm
fit_lnorm <- fitdist(buys_pair_less_30$n,"lnorm")
summary(fit_lnorm)
plot(fit_lnorm)
cdfcomp(fit_lnorm)

#exp
fit_exp <- fitdist(buys_pair_less_30$n,"exp")
summary(fit_exp)
plot(fit_exp)
cdfcomp(fit_exp)

#gamma
fit_gamma <- fitdist(buys_pair_less_30$n,"gamma")
summary(fit_gamma)
plot(fit_gamma)
cdfcomp(fit_gamma)

#weibull
fit_weibull <- fitdist(buys_pair_less_30$n,"weibull")
summary(fit_weibull)

#normal
fit_normal <- fitdist(buys_pair_less_30$n,"norm")
summary(fit_normal)

#pois
#normal
fit_pois <- fitdist(buys_pair_less_30$n,"pois")
summary(fit_pois)

#unif
fit_unif <- fitdist(buys_pair_less_30$n,"unif")
summary(fit_unif)
plot(fit_unif)
cdfcomp(fit_unif)

######################draw graph#######################
all_density <- ggplot(data=buys_pair_less_30) + 
  geom_histogram(bins=30,aes(x = buys_pair_less_30$n, ..density..)) + 
  stat_function(fun = dlnorm, args = list(meanlog = 0.2373987, sdlog = 0.4791316), 
                colour = "red")+
  stat_function(fun = dgamma, args = list(shape = 3.020395, rate=2.000602), 
                colour = "blue")+
  stat_function(fun=dexp, args=list(rate=0.6623558),colour="green")+
  stat_function(fun=dweibull, args=list(shape=1.360851, scale=1.678697),colour="yellow")+
  stat_function(fun=dpois, args=list(lambda=1.509763),colour="orange")+  xlab("No.Buys")
all_density

########################################Question 1############################################


########################################Question 2############################################
tierion_prices <- read_delim("C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/Blockchain-Tokens-Data-Analytics/tierion", delim = "\t", col_names = T) #load token price data
names(tierion_prices) <- make.names(names(tierion_prices))
tierion_prices <- tierion_prices %>% mutate(date = as.Date(Date, format = '%m/%d/%Y'))

tierion <- read_delim('C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/Blockchain-Tokens-Data-Analytics/networktierionTX.txt', delim = " ", col_names = F)
names(tierion) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')
decimals <- 10^8
supply <- 1 * 10^9
tierion_filtered <-filter(tierion,tokenAmount < decimals * supply)
## convert data type of unixTime
tierion_filtered <- tierion_filtered %>%
  mutate(date = anydate(unixTime))
names(tierion_filtered) <- c('fromID', 'toID', 'unixTime', 'tokenAmount', 'date')

## merge the prices and edge
tierion_merged<-merge(x = tierion_prices, y = tierion_filtered, by = "date", all.x = TRUE)

################Determin K##########################
top_30_buyers<-buys_sorted_dec%>%head(30)

top_K<-c(1:30)
count <- 1
for (val in top_K) {
  top_K_buyers<-buys_sorted_dec%>%head(val)
  filter_K_tierion_merged<-filter(tierion_merged,toID %in% top_K_buyers$toID)
  filter_K_tierion_merged=transform(filter_K_tierion_merged,average_price= (Open+Close)/2)
  filter_K_tierion_merged$num_Date <-  as.numeric(as.POSIXct(filter_K_tierion_merged$date))
  filered<-filter_K_tierion_merged%>% group_by(num_Date) %>% summarise(n = n(),Close=mean(Close),tokenAmount=sum(tokenAmount),Open=mean(Open))
  shift <- function(x, n){
    c(x[-(seq(n))], rep(NA, n))
  }
  filered$new_Close<-shift(filered$Close,1)
  num_rows<-nrow(filered)
  filered[-num_rows,]
  regression<-lm(filered$new_Close~filered$tokenAmount+filered$n+filered$Open)
  
  setwd("C:/Users/ygaoq/Desktop/Tierion")
  yourfilename=paste("W",val,".txt",sep="")
  capture.output(summary(regression),append = TRUE,file = "C:/Users/ygaoq/Desktop/Tierion/Final_Result.txt")
  
  
  summary(regression)
  par(mfcol=c(2,2))
  setwd("C:/Users/ygaoq/Desktop/Tierion")
  yourfilename=paste("A",val,".png",sep="")
  png(file=yourfilename)
  opar <- par(mfrow=c(2,2))
  plot(regression)
  dev.off()
}


