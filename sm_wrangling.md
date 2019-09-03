IRA Social Media Activity - Data Wrangling
================
jlukito
September 3, 2019

``` r
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
options(scipen=999) #disables notation
```

Import Data
===========

``` r
tweets <- read.csv("ira_tweets_csv_hashed.csv", header = TRUE)
```

``` r
redditc <- read.csv("redditc.csv", header = TRUE)
redditog <- read.csv("redditog.csv", header = TRUE)
```

``` r
fb <- read.csv("fb_ira.csv", header = TRUE, sep = ",")
```

Wrangle Data
============

Facebook
--------

``` r
fb$date <- as.Date(fb$date, format="%m/%d/%Y")
all_dates <- seq(as.Date(min(fb$date)), as.Date(max(fb$date)), by="day")

date1 <- fb %>% group_by(date) %>% summarise(frequency = n()) #count data

date2 <- merge(data.frame(date = all_dates),
                   date1,
                   by.x = 'date',
                   all.x=T)
date2$frequency[is.na(date2$frequency)] <- 0 #fills empty dates
date3 <- date2[1:780,]

fb_tsdata <- ts(date3$frequency, frequency = 1)
```

Twitter
-------

``` r
en_data <- subset(tweets, tweet_language == "en") 

en_data_time_order <- en_data[rev(order(as.Date(en_data$tweet_time))),]
en_data_time <- data.frame(time = en_data_time_order$tweet_time)
en_data_time$time <- as.POSIXct(en_data_time$time)
en_data_timeframe <- subset(en_data_og2_time, time > as.POSIXct("2015-07-01")) %>% 
  subset(time < as.POSIXct("2017-06-01"))#subsets to time frame of interest

tw_tsdata <- table(en_data_timeframe$time) %>% as.data.frame() #count data
```

Reddit
------

``` r
redditc$datetime <- as.Date(as.POSIXct(redditc$created_utc, origin="1970-01-01"))
redditog$datetime <- as.Date(as.POSIXct(redditog$created_utc, origin="1970-01-01"))

redditc <- subset(redditc, select = c("id", "created_utc", "datetime"))
redditog <- subset(redditog, select = c("id", "created_utc", "datetime"))
reddit <- rbind(redditc, redditog) #merges comment data with original posts data
                
data_filtered <- subset(reddit, reddit$datetime > as.Date("2015-07-01")) %>% 
  subset(reddit$datetime < as.Date("2017-06-01")) #subsets to time frame of interest

reddit_data <- table(data_filtered$datetime) %>% as.data.frame() #count data
reddit_data$Var1 <- as.Date(reddit_data$Var1)

reddit_fulldate <- data.frame(Var1 = seq(as.Date("2015-07-02"), as.Date("2017-05-31"), by="day"))
reddit_fulldate$Var1 <- as.Date(reddit_fulldate$Var1)

reddit_filleddate <- merge(reddit_fulldate, reddit_data,
                           by= 'Var1', all.x = T)
reddit_filleddate$Freq[is.na(reddit_filleddate$Freq)] = 0 #fills empty dates
```
