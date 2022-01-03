
# library
library(readr)
library(twitteR)
library(ROAuth)

# get API keys
source("./Rscript/myAPI.R")

## setup
options(httr_oauth_cache = TRUE)
setup_twitter_oauth(ApiKey, ApiSecret, AccessToken, AccessSecret)


## scrape Tweets
ls_stb <- userTimeline(user = "Starbucks_J", n = 200)


## save
write_rds(ls_stb, "./dat/ls_stb.rds")

## remove unnecessary objects
rm(list = c("ApiKey", "ApiSecret", "AccessToken", "AccessSecret"))
