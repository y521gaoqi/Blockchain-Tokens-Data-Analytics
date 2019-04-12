## Read in the BAT token
library(magrittr)
library(dplyr)
library(readr)
bat <- read_delim('C:/Users/ygaoq/OneDrive/MyDocuments/2019 Spring/Statistics/Project/networktierionTX.txt', delim = " ", col_names = F)
##View(bat)
names(bat) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')
decimals <- 10^18
supply <- 1.5 * 10^9
bat

batFiltered <-filter(bat,tokenAmount < decimals * supply)
bat_filteredout<- filter(bat,tokenAmount >= decimals * supply)
bat_filteredout
