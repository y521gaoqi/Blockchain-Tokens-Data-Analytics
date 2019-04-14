## Read in the BAT token
library(magrittr)
library(dplyr)
library(readr)
bat <- read_delim('C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/Blockchain-Tokens-Data-Analytics/networktierionTX.txt', delim = " ", col_names = F)
##View(bat)
names(bat) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')
decimals <- 10^8
supply <- 1 * 10^9
dim(bat)

batFiltered <-filter(bat,tokenAmount < decimals * supply)
dim(batFiltered)
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
buys_sorted_ase<-buys[order(-buys$n),]
buys_sorted_ase
top_10_buyers<-buys_sorted%>%head(10) #sort by decending order and get top 10
top_10_buyers
buys_sorted_des<-buys[order(buys$n),]
buys_sorted_des


############

#plot the distribution
users<-buys_sorted%>% group_by(n) %>% summarise(num_users = n()) %>% ungroup #change the supply and decimals amount
users
hist(buys_sorted$n,breaks=10, col="red")


######################
trainSet2 <-data.frame(
  size=c(1,2,3,2,3,4),
  weight=c(222,333,444,555,666,444),
  color=c("abc","aaa","ddd","eee","fds","aaa"),
  taste=c("good","good","bad","bad","bad","good")
)
trainSet2
buys.distribution <- trainSet2 %>% group_by(taste) %>% summarise(n = n()) %>% ungroup
buys.distribution

