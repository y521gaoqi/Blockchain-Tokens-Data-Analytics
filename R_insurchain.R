library(magrittr)
library(dplyr)
library(ggplot2)
library(readr)
library(fitdistrplus)
library(DAAG)
library("ggplot2")
library(anytime)

insurchain <- read_delim('C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/Blockchain-Tokens-Data-Analytics/networkinsurchainTX.txt', delim = " ", col_names = F)
names(insurchain) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')
decimals <- 10^18
supply <- 2 * 10^10
insurchainFiltered <-filter(insurchain,tokenAmount < decimals * supply)  #filter out all outliers

#figure out how many users indruced those unnormal transcation
insurchain_outliers<- filter(insurchain,tokenAmount >= decimals * supply) 
user_outliers <- insurchain_outliers %>% group_by(toID) %>% summarise(n = n()) %>% ungroup
number_users_outliers<-nrow(user_outliers)
number_users_outliers

#get top X buyers data
buys<-insurchainFiltered%>% group_by(toID) %>% summarise(n = n()) %>% ungroup #change the supply and decimals amount
buys_sorted_dec<-buys[order(-buys$n),]
#top 30 active buyers and number of buys
top_30_buyers<-buys_sorted_dec%>%head(30) 
top_30_buyers

########################################Question 1############################################

#####group by user pairs#####
buys_pairs<-insurchainFiltered%>% group_by(fromID, toID) %>% summarise(n = n()) %>% ungroup 
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
buys_pairs<-insurchainFiltered%>% group_by(fromID*toID+fromID+toID) %>% summarise(n = n()) %>% ungroup 
buys_pairs<-insurchainFiltered%>% group_by(fromID, toID) %>% summarise(n = n()) %>% ungroup
buys_pair_sorted_asc<-buys_pairs[order(buys_pairs$n),]
buys_pair_less_10<-subset(buys_pair_sorted_asc,n<10)

#####find out estimates of paramaters of several distribution based on the buys_pairs data set#####
exp_dis <- fitdist(buys_pair_less_10$n, 'exp')
exp_dis
gamma_dis <- fitdist(buys_pair_less_10$n, 'gamma')
gamma_dis
lnorm_dis <- fitdist(buys_pair_less_10$n, 'lnorm')
lnorm_dis
pois_dis <- fitdist(buys_pair_less_10$n, 'pois')
pois_dis
weibull_dis <- fitdist(buys_pair_less_10$n, 'weibull')
weibull_dis

gofstat(list(exp_dis, gamma_dis, lnorm_dis))
descdist(buys_sorted_asc$n,boot=1000)

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
all_density <- ggplot(data=buys_pair_less_10) + 
  geom_histogram(bins=30,aes(x = buys_pair_less_10$n, ..density..)) + 
  stat_function(fun = dlnorm, args = list(meanlog = 0.03659629, sdlog = 0.17618264), 
                colour = "red")+
  stat_function(fun = dgamma, args = list(shape = 22.78865, rate=21.48970), 
                colour = "blue")+
  stat_function(fun=dexp, args=list(rate=0.9429928),colour="green")+
  stat_function(fun=dweibull, args=list(shape=2.401908, scale=1.165417),colour="yellow")+
  stat_function(fun=dpois, args=list(lambda=1.060454),colour="orange")
all_density

########################################Question 1############################################


########################################Question 2############################################
insurchain_prices <- read_delim("C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/Blockchain-Tokens-Data-Analytics/insurchain", delim = "\t", col_names = T) #load token price data
names(insurchain_prices) <- make.names(names(insurchain_prices))
insurchain_prices <- insurchain_prices %>% mutate(date = as.Date(Date, format = '%m/%d/%Y'))

insurchain <- read_delim('C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/Blockchain-Tokens-Data-Analytics/networkinsurchainTX.txt', delim = " ", col_names = F)
names(insurchain) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')
decimals <- 10^8
supply <- 1 * 10^9
insurchain_filtered <-filter(insurchain,tokenAmount < decimals * supply)
## convert data type of unixTime
insurchain_filtered <- insurchain_filtered %>%
  mutate(date = anydate(unixTime))
names(insurchain_filtered) <- c('fromID', 'toID', 'unixTime', 'tokenAmount', 'date')

## merge the prices and edge
insurchain_merged<-merge(x = insurchain_prices, y = insurchain_filtered, by = "date", all.x = TRUE)

################Determin K##########################
top_30_buyers<-buys_sorted_dec%>%head(30)

top_K<-c(1:30)
count <- 1
for (val in top_K) {
  top_K_buyers<-buys_sorted_dec%>%head(val)
  filter_K_insurchain_merged<-filter(insurchain_merged,toID %in% top_K_buyers$toID)
  filter_K_insurchain_merged=transform(filter_K_insurchain_merged,average_price= (Open+Close)/2)
  filter_K_insurchain_merged$num_Date <-  as.numeric(as.POSIXct(filter_K_insurchain_merged$date))
  filered<-filter_K_insurchain_merged%>% group_by(num_Date) %>% summarise(n = n(),Close=mean(Close),tokenAmount=sum(tokenAmount),Open=mean(Open))
  shift <- function(x, n){
    c(x[-(seq(n))], rep(NA, n))
  }
  filered$new_Close<-shift(filered$Close,1)
  num_rows<-nrow(filered)
  filered[-num_rows,]
  regression<-lm(filered$new_Close~filered$tokenAmount+filered$n+filered$Open)
  
  setwd("C:/Users/ygaoq/Desktop/insurchain")
  yourfilename=paste("W",val,".txt",sep="")
  capture.output(summary(regression),append = TRUE,file = "C:/Users/ygaoq/Desktop/insurchain/Final_Result.txt")
  
  
  summary(regression)
  par(mfcol=c(2,2))
  setwd("C:/Users/ygaoq/Desktop/insurchain")
  yourfilename=paste("A",val,".png",sep="")
  png(file=yourfilename)
  opar <- par(mfrow=c(2,2))
  plot(regression)
  dev.off()
}


