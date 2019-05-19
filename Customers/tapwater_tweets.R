## Tap Water Sentiment Analysis
## https://r.prevos.net/tap-water-sentiment-analysis/

## Init
library(tidyverse)
library(tidytext)
library(twitteR)

# Extract tap water tweets
source("twitteR_API.R")
setup_twitter_oauth(api_key, api_secret, token, token_secret)

tapwater_tweets <- searchTwitter("tap water", n = 1000, lang = "en") %>%
  twListToDF() %>%
  select(id, text)
tapwater_tweets <- subset(tapwater_tweets, !duplicated(tapwater_tweets$text))
tapwater_tweets$text <- gsub("’", "'", tapwater_tweets$text)

# write_csv(tapwater_tweets, "Hydroinformatics/tapwater_tweets.csv")
tapwater_tweets <- read_csv("Hydroinformatics/tapwater_tweets.csv")

# Tokenisation
tidy_tweets <- tapwater_tweets %>%
  unnest_tokens(word, text)

data(stop_words)
tidy_tweets <- tidy_tweets %>%
  anti_join(stop_words) %>%
    filter(!word %in% c("tap", "water", "rt", "https", "t.co", "gt", "amp",
                        as.character(0:9)))

tidy_tweets %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col(fill = "dodgerblue4") +
    xlab(NULL) + coord_flip() +
    ggtitle("Most common words in tap water tweets")

ggsave("Hydroinformatics/tapwater_words.png", dpi = 300)

# Sentiment analysis
sentiment_bing <- tidy_tweets %>%
  inner_join(get_sentiments("bing"))
sentiment_bing %>%
  summarise(Negative = sum(sentiment == "negative"), 
            positive = sum(sentiment == "positive"))
sentiment_bing %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + 
    coord_flip() + facet_wrap(~sentiment, scales = "free_y") + 
    ggtitle("Contribution to sentiment") + xlab(NULL) + ylab(NULL)

ggsave("Hydroinformatics/tapwater_sentiment.png", dpi = 300)

## Network analysis
tidy_tweets %>% cast_dtm()
