
# library
library(tidyverse)
library(twitteR)
library(dlookr)
library(magrittr)
library(RMeCab)
library(stringi)
library(wordcloud)
library(emoji)



# data construction -------------------------------------------------------

## source
ls_stb <- readRDS("./dat/ls_stb.rds")

## convert the list to a dataframe
df_stb <- 
  ls_stb %>% 
  map_df(as.data.frame) %>% 
  mutate(emojiCount = emoji_count(.$text)) # emoji count

## diagnose
df_stb %>% 
  diagnose()

## subset #####
### not reply
df_stb_nrp <- 
  df_stb %>% 
  filter(is.na(replyToSN), 
         !str_detect(.$text, pattern = "リツイートありがとうございます")) %>% 
  mutate(text = stri_trans_nfkc(text))

### over 3th quantile
df_stb_ManyRtw <- 
  df_stb_nrp %>% 
  filter(retweetCount >  quantile(.$retweetCount)[[4]])

### under 1th quantile
df_stb_FewRtw <- 
  df_stb_nrp %>% 
  filter(retweetCount <  quantile(.$retweetCount)[[2]])



## descriptive stats ####
df_stb %>% summary()
df_stb_nrp %>% summary()
df_stb_ManyRtw %>% summary()
df_stb_FewRtw %>% summary()


## save as text ####
writeLines(text = use_series(df_stb_nrp, text), "./dat/txt_stb_nrp.txt")
writeLines(text = use_series(df_stb_ManyRtw, text), "./dat/txt_stb_many.txt")
writeLines(text = use_series(df_stb_FewRtw, text), "./dat/txt_stb_few.txt")



# frequency ---------------------------------------------------------------

## subset
df_stb_fq <- RMeCabFreq(filename = "./dat/txt_stb_nrp.txt") %>% 
  filter(str_detect(Term, pattern = "\\w+"), 
         str_detect(Term, pattern = "\\p{ASCII}", negate = TRUE)) %>% 
  arrange(- Freq, Info1, Info2, Term) %>% 
  filter(Info1 %in% c("名詞", "形容詞", "動詞"), 
         !Info2 %in% c("接尾", "非自立", "数"))

df_stb_fq_many <- RMeCabFreq(filename = "./dat/txt_stb_many.txt") %>% 
  filter(str_detect(Term, pattern = "\\w+"), 
         str_detect(Term, pattern = "\\p{ASCII}", negate = TRUE)) %>% 
  arrange(- Freq, Info1, Info2, Term) %>% 
  filter(Info1 %in% c("名詞", "形容詞", "動詞"), 
         !Info2 %in% c("接尾", "非自立", "数"))


df_stb_fq_few <- RMeCabFreq(filename = "./dat/txt_stb_few.txt") %>% 
  filter(str_detect(Term, pattern = "\\w+"), 
         str_detect(Term, pattern = "\\p{ASCII}", negate = TRUE)) %>% 
  arrange(- Freq, Info1, Info2, Term) %>% 
  filter(Info1 %in% c("名詞", "形容詞", "動詞"), 
         !Info2 %in% c("接尾", "非自立", "数"))

### word cloud
df_stb_fq %>% 
  filter(Term != "する") %>% 
  with(
    wordcloud(Term, Freq, 
              min.freq = quantile(x = Freq, probs = seq(0, 1, 0.1))[[9]], 
              color = brewer.pal(8, "Dark2")) 
    )

df_stb_fq_many %>% 
  filter(Term != "する") %>% 
  with(
    wordcloud(Term, Freq, 
              min.freq = quantile(x = Freq, probs = seq(0, 1, 0.1))[[9]], 
              color = brewer.pal(8, "Dark2")) 
  )


df_stb_fq_few %>% 
  filter(Term != "する") %>% 
  with(
    wordcloud(Term, Freq, 
              min.freq = quantile(x = Freq, probs = seq(0, 1, 0.1))[[9]], 
              color = brewer.pal(8, "Dark2")) 
  )



# emoji count -------------------------------------------------------------

## correlation matrix
df_stb_nrp %>% 
  mutate(lnRetweetCount = log(retweetCount), 
         lnFavoriteCount = log(favoriteCount)) %>% 
  select(emojiCount, lnRetweetCount, lnFavoriteCount) %>% 
  cor() %>% 
  corrplot(method = "color", addCoef.col = TRUE)

## regression
mdl_emoji <- lm(data = df_stb_nrp, log(retweetCount) ~ emojiCount)
mdl_emoji %>% 
  summary()


## plot
df_stb_nrp %>% 
  ggplot(aes(x = emojiCount, y = log(retweetCount))) + 
  geom_point() + 
  geom_smooth(method = "lm", colour = "blue") + 
  theme_minimal() + 
  labs(x = "Emoji Count", y = "Retweet Count (logarithm)", 
       title = "Correlation between Emoji and Retweet") + 
  annotate(geom = "text", x = 5, y = 8.2, 
           label = paste0("y = ", 
                          round(coef(mdl_emoji)[1], digits = 4), 
                          " + ", 
                          round(coef(mdl_emoji)[2], digits = 4), 
                          "x")) + 
  annotate(geom = "text", x = 5, y = 7.8,
           label = paste0("Adjusted R2 = ", 
                          round(summary(mdl_emoji)$adj.r.squared, digits = 3)))

