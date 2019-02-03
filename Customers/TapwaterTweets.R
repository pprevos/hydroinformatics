## Tap Water tweet sentiment analysis

# Snippet 1
library(tidyverse)
library(twitteR)
source("Customers/twitteR_API.R")
setup_twitter_oauth(api_key, api_secret, token, token_secret)

tapwater_tweets <- searchTwitter("tap water", n = 1000, lang = "en") %>%
  twListToDF() %>%
  select(id, text)
tapwater_tweets <- subset(tapwater_tweets, !duplicated(tapwater_tweets$text))
tapwater_tweets$text <- gsub("’", "'", tapwater_tweets$text)
write_csv(tapwater_tweets, paste0("Customers/tapwater_tweets_", format(Sys.time(), "%F_%H%M"), ".csv"))

# Snippet 2
library(tidytext)
tapwater_tweets <- read_csv("Customers/tapwater_tweets_2018-04-07_0648.csv")
tidy_tweets <- tapwater_tweets %>%
  unnest_tokens(word, text)

data(stop_words)
tidy_tweets <- tidy_tweets %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("tap", "water", "rt", "https", "t.co", "gt", 
                      "amp", as.character(0:9)))

tidy_tweets %>%
  count(word, sort = TRUE) %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col(fill = "dodgerblue4") +
    xlab(NULL) + coord_flip() + ggtitle("Most common words in tap water tweets")
ggsave("Images/tapwater_words.png", dpi = 300)

# Snippet 3
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


# Snippet 4
sentiment_nrc <- tidy_tweets %>%
  inner_join(get_sentiments("nrc"))

sentiment_nrc %>%
  group_by(sentiment) %>%
  count(sentiment, sort = TRUE) %>%
  ggplot(aes(sentiment, n)) + geom_col(show.legend = FALSE) + 
  coord_flip() + xlab(NULL) + ylab(NULL) + 
  ggtitle("Contribution to sentiment")
ggsave("Hydroinformatics/tapwater_sentiment.png", dpi = 300)

# Snippet 5
sentiment_afinn <- tidy_tweets %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(id) %>%
  summarise(Sentiment = mean(score)) %>%
  arrange(Sentiment)

inner_join(tapwater_tweets, sentiment_afinn) %>%
  filter(Sentiment <= -2) %>%
  select(text)
