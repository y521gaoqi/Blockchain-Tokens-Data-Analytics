library(magrittr)
library(dplyr)
library(ggplot2)
library(readr)
library(fitdistrplus)
library(DAAG)
library(data.table)





##############################Question 2####################################
tierion_prices <- read_delim("C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/Blockchain-Tokens-Data-Analytics/aragon", delim = "\t", col_names = T) #load token price data
names(tierion_prices) <- make.names(names(tierion_prices))
tierion_prices
tierion_prices <- tierion_prices %>% mutate(date = as.Date(Date, format = '%m/%d/%Y'))
tierion_prices
tierion <- read_delim('C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/Blockchain-Tokens-Data-Analytics/networkaragonTX.txt', delim = " ", col_names = F)
names(tierion) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')
decimals <- 10^18
supply <- 39609523
tierion_filtered <-filter(tierion,tokenAmount < decimals * supply)
buys<-tierion_filtered%>% group_by(toID) %>% summarise(n = n()) %>% ungroup #change the supply and decimals amount
buys_sorted_dec<-buys[order(-buys$n),]
top_30_buyers<-buys_sorted_dec%>%head(30) #sort by decending order and get top 30

library(anytime)
## convert data type of unixTime
tierion_filtered <- tierion_filtered %>%
  mutate(date = anydate(unixTime))
names(tierion_filtered) <- c('fromID', 'toID', 'unixTime', 'tokenAmount', 'date')
tierion_filtered

## merge the prices and edge
tierion_merged<-merge(x = tierion_prices, y = tierion_filtered, by = "date", all.x = TRUE)
tierion_filtered
str(tierion_filtered)
str(tierion_prices)
tierion_prices
tierion_merged

filter_K_tierion_merged<-filter(tierion_merged,toID %in% top_30_buyers$toID)

#get average price from open and close price and add it as a new column
filter_K_tierion_merged=transform(filter_K_tierion_merged,average_price= (Open+Close)/2)
View(filter_K_tierion_merged)

###########################################group by date############################################
filter_K_tierion_merged$num_Date <-  as.numeric(as.POSIXct(filter_K_tierion_merged$date))
View(filter_K_tierion_merged)
filered<-filter_K_tierion_merged%>% group_by(num_Date) %>% summarise(n = n(),Close=mean(Close),tokenAmount=sum(tokenAmount),Open=mean(Open))
View(filered)

shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}
filered$new_Close<-shift(filered$Close,1)
filered[353,6]=4.51
#shift(filered$Close, n=1, fill=NA, type="lead")
#df = data.frame(x=filered$tokenAmount+filered$Open, y=filered$Close)
regression<-lm(filered$new_Close~filered$tokenAmount+filered$n+filered$Open)
summary(regression)
plot(filered$tokenAmount+filered$n+filered$Open,filered$Close)
abline(regression)


regression
par(mfcol=c(2,2))
plot(regression)
summary(regression)  #explaination of detail result: https://blog.csdn.net/lilanfeng1991/article/details/29627405\
par(mfcol=c(1,1))
plot(filered$tokenAmount+filered$n+filered$Open,filered$Close)
abline(regression, col=2, lwd3)
abline(regression)
############################################################################






