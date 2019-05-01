library(magrittr)
library(dplyr)
library(ggplot2)
library(readr)
library(fitdistrplus)
library(DAAG)
tierion <- read_delim('C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/Blockchain-Tokens-Data-Analytics/networktierionTX.txt', delim = " ", col_names = F)
##View(tierion)
names(tierion) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')
decimals <- 10^8
supply <- 1 * 10^9
dim(tierion)

tierionFiltered <-filter(tierion,tokenAmount < decimals * supply)
dim(tierionFiltered)
tierionFiltered
summarise(tierionFiltered)
tierion_filteredout<- filter(tierion,tokenAmount >= decimals * supply) 
tierion_filteredout
dim(tierion_filteredout)

#figure out how many users indruced those unnormal transcation
user_outliers <- tierion_filteredout %>% group_by(toID) %>% summarise(n = n()) %>% ungroup
dim(user_outliers)
user_outliers
number_users_outliers<-nrow(user_outliers)
number_users_outliers

#######################################single user###############################
#Buyers Data
buys<-tierionFiltered%>% group_by(toID) %>% summarise(n = n()) %>% ungroup #change the supply and decimals amount
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
top_30_buyers
top_100_buyers<-buys_sorted_dec%>%head(100) #sort by decending order and get top 100


buys_less_30<-subset(buys_sorted_asc,n<30)
View(buys_less_30)

group_users<-buys_sorted_dec%>% group_by(n) %>% summarise(amount_users = n()) %>% ungroup #change the supply and decimals amount
group_users
########################################single user##############################

########################################pair user################################
for (row in 1:nrow(tierionFiltered_pairs)) {
  a<-tierionFiltered_pairs[row,"fromID"]
  b<-tierionFiltered_pairs[row,"toID"]
  for (inner_row in row:nrow(tierionFiltered_pairs)) {
    c<-tierionFiltered_pairs[inner_row,"fromID"]
    d<-tierionFiltered_pairs[inner_row,"toID"]
    if(a==d&&b==c){
      tierionFiltered_pairs[inner_row,"fromID"]<-d
      tierionFiltered_pairs[inner_row,"toID"]<-c
    }
  }
}


buys_pairs<-tierionFiltered%>% group_by(fromID*toID+fromID+toID) %>% summarise(n = n()) %>% ungroup 
View(buys_pairs)
buys_pair_sorted_asc<-buys_pairs[order(buys_pairs$n),]
View(buys_pair_sorted_asc)
buys_pair_less_30<-subset(buys_pair_sorted_asc,n<30)
buys_pair_less_30


########################################pair user################################

############

#plot the density histgram
his_density<-hist(buys_less_30$n,breaks=20, freq=FALSE,col="red")
a <- 1/(mean(buys_sorted_asc$n))
t<- min(buys_sorted_asc$n):(max(buys_sorted_asc$n)+2)
lines(t,dexp(t,a),col="blue")

plot<-ggplot(buys_less_30, aes(x=n, y=..density..)) +geom_histogram(bins=30,color="black", fill="lightblue")+labs(x="Value", y = "Density")
print(plot)

######################

#get parameter from different distribution for single user
exp_dis <- fitdist(buys_less_30$n, 'exp')
exp_dis
gamma_dis <- fitdist(buys_less_30$n, 'gamma')
gamma_dis
lnorm_dis <- fitdist(buys_less_30$n, 'lnorm')
lnorm_dis
gofstat(list(exp_dis, gamma_dis, lnorm_dis))
#descdist(buys_sorted_asc$n,boot=1000)

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

#weibull
fit_weibull <- fitdist(buys_less_30$n,"weibull")
summary(fit_weibull)

#normal
fit_normal <- fitdist(buys_less_30$n,"norm")
summary(fit_normal)

#pois
#normal
fit_pois <- fitdist(buys_less_30$n,"pois")
summary(fit_pois)

#unif
fit_unif <- fitdist(buys_less_30$n,"unif")
summary(fit_unif)
plot(fit_unif)
cdfcomp(fit_unif)


#fitp <- fitdist(buys_less_30$n,"unif");
#cdfcomp(list(fitp,fitnb))
#lines(density(buys_sorted_asc$n))
###########################

###########pair user###########

#get parameter from different distribution for pair user
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

#weibull
fit_weibull <- fitdist(buys_less_30$n,"weibull")
summary(fit_weibull)

#normal
fit_normal <- fitdist(buys_less_30$n,"norm")
summary(fit_normal)

#pois
#normal
fit_pois <- fitdist(buys_less_30$n,"pois")
summary(fit_pois)

#unif
fit_unif <- fitdist(buys_less_30$n,"unif")
summary(fit_unif)
plot(fit_unif)
cdfcomp(fit_unif)


#fitp <- fitdist(buys_less_30$n,"unif");cdfcomp(list(fitp,fitnb))
#lines(density(buys_sorted_asc$n))
###########pair user################

#####################Plot Distribution curves##############################

#single user
library("ggplot2")
all_density <- ggplot(data=buys_pair_less_30) + 
  geom_histogram(bins=30,aes(x = buys_pair_less_30$n, ..density..)) + 
  stat_function(fun = dlnorm, args = list(meanlog = 0.2373987, sdlog = 0.4791316), 
                colour = "red")+
  stat_function(fun = dgamma, args = list(shape = 3.020395, rate=2.000602), 
                colour = "blue")+
  stat_function(fun=dexp, args=list(rate=0.6623558),colour="green")+
  stat_function(fun=dweibull, args=list(shape=1.360851, scale=1.678697),colour="yellow")+
  stat_function(fun=dpois, args=list(lambda=1.509763),colour="orange")
all_density

#a pair of users


library("ggplot2")
all_density <- ggplot(data=buys_pair_less_30) + 
  geom_histogram(bins=30,aes(x = buys_pair_less_30$n, ..density..)) + 
  stat_function(fun = dlnorm, args = list(meanlog = 0.3797812, sdlog = 0.6111954), 
                colour = "red")+
  stat_function(fun = dgamma, args = list(shape = 2.032322, rate=1.065791), 
                colour = "blue")+
  stat_function(fun=dexp, args=list(rate=0.5244071),colour="green")+
  stat_function(fun=dweibull, args=list(shape=1, scale=0.3457897),colour="yellow")+
  stat_function(fun=dpois, args=list(lambda=1.906915),colour="orange")+
  labs(x="Number of Sells of Pair Users", y="Density")
all_density



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

str(tierion_merged)
View(tierion_merged)

filter_K_tierion_merged<-filter(tierion_merged,toID %in% top_30_buyers$toID)
View(filter_K_tierion_merged)
View(top_100_buyers$toID)

#get average price from open and close price and add it as a new column
filter_K_tierion_merged=transform(filter_K_tierion_merged,average_price= (Open+Close)/2)
View(filter_K_tierion_merged)

###########################################group by date############################################
filter_K_tierion_merged$num_Date <-  as.numeric(as.POSIXct(filter_K_tierion_merged$date))
View(filter_K_tierion_merged)
str(filter_K_tierion_merged)
filered<-filter_K_tierion_merged%>% group_by(num_Date) %>% summarise(n = n(),Close=mean(Close),tokenAmount=sum(tokenAmount),Open=mean(Open))
View(filered)
#filered<-aggregate(cbind(filter_K_tierion_merged$tokenAmount, by=list(filter_K_tierion_merged$date), FUN=c(sum,mean))
names(filered)<-c("date","total_tokenAmount")
View(filered)
tierion_merged<-merge(x = filered, y = tierion_prices, by = "date", all.x = FALSE)
View(tierion_merged)

filered<-filter_K_tierion_merged%>% group_by(toID) %>% summarise(n = n(),average_price=mean(average_price),tokenAmount=sum(tokenAmount), average_MC=mean(Market.Cap)) %>% ungroup 
View(filered)
filered_sorted_des<-filered[order(-filered$tokenAmount),]
filered<-filered_sorted_des[-1:-10,]
View(filered)

df = data.frame(x=filered$tokenAmount+filered$Open, y=filered$Close)
regression<-lm(filered$Close~filered$Open+filered$tokenAmount)
#regression <- lm(filered$average_price ~ I(filered$tokenAmount+filered$n)) 
#df = data.frame(x=filered$tokenAmount+filered$n, y=filered$average_price)
#regression = lm(y~x, df)
regression
par(mfcol=c(2,2))
plot(regression)
summary(regression)  #explaination of detail result: https://blog.csdn.net/lilanfeng1991/article/details/29627405\
par(mfcol=c(1,1))
plot(df$x,df$y)
abline(regression, col=2, lwd3)
abline(regression)
############################################################################


##################################test##############################
data<-data.frame(c(1,2,3,4,5,6,7,8,9),c(1,2,3,4,5,6,7,9,10))
names(data)<-c("x","y")
regression<-lm(data$y~data$x)
regression
summary(regression)
par(mfcol=c(1,1))
plot(data$x,data$y)
abline(regression)

#test_dataframe=data.frame(fromID=c(1,2), toID=c(2,1))
#for (row in 1:nrow(test_dataframe)) {
#  a<-test_dataframe[row,"fromID"]
#  b<-test_dataframe[row,"toID"]
#  for (inner_row in row:nrow(test_dataframe)) {
#    c<-test_dataframe[inner_row,"fromID"]
#    d<-test_dataframe[inner_row,"toID"]
#    if(a==d&&b==c){
#      test_dataframe[inner_row,"fromID"]<-d
#      test_dataframe[inner_row,"toID"]<-c
#    }
#  }
#}
#test_dataframe
#a
#b
####################################################################





