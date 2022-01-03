
# library
library(tidyverse)
library(twitteR)
library(magrittr)
library(RMeCab)
library(lubridate)

# source
ls_stb <- readRDS("./dat/ls_stb.rds")

## dataframe
df_stb_nrp <- 
  ls_stb %>% 
  map_df(as.data.frame) %>% 
  filter(is.na(replyToSN), 
         !str_detect(.$text, pattern = "リツイートありがとうございます")) %>% 
  mutate(text = stri_trans_nfkc(text))

## time series fluctuation
df_stb_nrp %>% 
  ggplot(aes(x = created, y = retweetCount)) +
  geom_line(colour = "blue") + 
  scale_y_log10() + 
  labs(x = "Date", y = "Retweet Count", 
       title = "Time Series Fluctuation (Starbucks)") + 
  theme_minimal()


