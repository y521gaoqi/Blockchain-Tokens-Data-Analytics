library(magrittr)
library(dplyr)
library(ggplot2)
library(readr)
library(fitdistrplus)
library(DAAG)

#######################price############################
aragon_prices <- read_delim("C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/Blockchain-Tokens-Data-Analytics/aragon", delim = "\t", col_names = T) #load token price data
names(aragon_prices) <- make.names(names(aragon_prices))
colnames(aragon_prices)[6] <- "MarketCap"
aragon_prices
aragon_prices <- aragon_prices %>% mutate(date = as.Date(Date, format = '%m/%d/%Y'))
aragon_prices
as.numeric(as.character(aragon_prices$MarketCap))
aragon_prices = subset(aragon_prices, select = -c(Market.Cap) )
aragon_prices
str(aragon_prices)

#######################edge###########################
aragon_edge <- read_delim('C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/Blockchain-Tokens-Data-Analytics/networkaragonTX.txt', delim = " ", col_names = F)
names(aragon_edge) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')
decimals <- 10^18
supply <- 4 * 10^7
aragon_filtered <-filter(aragon_edge,tokenAmount < decimals * supply)
aragon_filtered
## convert data type of unixTime
aragon_filtered <- aragon_filtered %>%
  mutate(date = anydate(unixTime))
names(aragon_filtered) <- c('fromID', 'toID', 'unixTime', 'tokenAmount', 'date')
aragon_filtered


#########################merge price and edge################
library(anytime)
aragon_filtered
View(aragon_prices)
## merge the prices and edge
aragon_merged<-merge(x = aragon_prices, y = aragon_filtered, by = "date", all.x = TRUE)
str(aragon_merged)
View(aragon_merged)

#top 30 users
buys<-aragon_filtered%>% group_by(toID) %>% summarise(n = n()) %>% ungroup #change the supply and decimals amount
buys_sorted_dec<-buys[order(-buys$n),]
top_30_buyers<-buys_sorted_dec%>%head(30) #sort by decending order and get top 10

filter_K_aragon_merged<-filter(aragon_merged,toID %in% top_30_buyers$toID)
View(filter_K_aragon_merged)


#get average price from open and close price and add it as a new column
filter_K_aragon_merged=transform(filter_K_aragon_merged,average_price= (Open+Close)/2)
filered<-filter_K_aragon_merged%>% group_by(toID) %>% summarise(n = n(),average_price=mean(average_price),tokenAmount=sum(tokenAmount)) %>% ungroup 
View(filered)

regression<-lm(filered$average_price~filered$tokenAmount)
regression
par(mfcol=c(2,2))
plot(regression)
summary(regression)  #explaination of detail result: https://blog.csdn.net/lilanfeng1991/article/details/29627405\
par(mfcol=c(1,1))
plot(filered$tokenAmount,filered$average_price)
abline(regression)
############################################################################