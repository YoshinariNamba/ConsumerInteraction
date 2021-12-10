
# library
library(tidyverse)
library(twitteR)
library(magrittr)
library(ROAuth)

# get API keys
source("./Rscript/myAPI.R")

## setup
options(httr_oauth_cache = TRUE)
setup_twitter_oauth(ApiKey, ApiSecret, AccessToken, AccessSecret)


## scrape Tweets
ls_stb <- userTimeline(user = "Starbucks_J", n = 200)

## convert the list to a dataframe
df_stb <- data.frame(NULL)
for(i in 1:length(ls_stb)){
  df_tmp <- data.frame(text          = ls_stb[[i]]$text, 
                       favoriteCount = ls_stb[[i]]$favoriteCount, 
                       retweetCount  = ls_stb[[i]]$retweetCount)
  df_stb <- rbind(df_stb, df_tmp)
}

## subset
df_stb_nrp <- df_stb %>% 
  filter(!str_detect(.$text, pattern = "リツイートありがとうございます"))

df_stb_rp <- df_stb %>% 
  filter(str_detect(.$text, pattern = "リツイートありがとうございます"))

## descriptive stats
df_stb %>% summary()
df_stb_nrp %>% summary()
df_stb_rp %>% summary()




