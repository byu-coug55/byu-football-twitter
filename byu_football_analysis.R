library(tidyverse)
library(textclean)
library(tidytext)
library(tm)
library(lubridate)
library(stringr)
library(jsonlite)
library(ggpubr)
library(topicmodels)

# login
twitter_creds <- jsonlite::fromJSON("api_keys_twitter.json")
twitter_url <- "https://api.twitter.com/2/"

bearer_token <- twitter_creds$bearer_token
headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

params <- list(`max_results` = 100,
               `start_time` = '2022-09-06T00:00:00.000Z',
               `tweet.fields` = 'author_id,created_at')

url_handle <- 'https://api.twitter.com/2/users/874824792/mentions'

response <-
  httr::GET(url = url_handle,
            httr::add_headers(.headers = headers),
            query = params)
obj <- httr::content(response, as = "text")

json_data <- fromJSON(obj, flatten = TRUE) %>% as.data.frame

data = json_data %>% select(tweet_id = data.id, text = data.text, author_id = data.author_id, created_at = data.created_at)


for (i in 1:5){
  next_token = json_data$meta.next_token[1]
  params <- list(`max_results` = 100,
                 `start_time` = '2022-09-06T00:00:00.000Z',
                 `tweet.fields` = 'author_id,created_at',
                 `pagination_token` = next_token)
  response <-
    httr::GET(url = url_handle,
              httr::add_headers(.headers = headers),
              query = params)
  obj <- httr::content(response, as = "text")
  json_data <- fromJSON(obj, flatten = TRUE) %>% as.data.frame
  next_token = json_data$meta.next_token[1]
  int_data = json_data %>% 
    select(tweet_id = data.id, text = data.text, author_id = data.author_id, created_at = data.created_at)
  data = bind_rows(data,int_data)
  
}

tweet_data = data %>% mutate(created_at = created_at %>% ymd_hms()) %>%
  mutate(day_hour = paste0(format.Date(created_at,"%d"),format.Date(created_at,"%H")))

tweet_data = tweet_data %>% mutate(clean_text = text %>% replace_emoji() %>% 
                                     replace_hash() %>% replace_non_ascii() %>%
                                     removePunctuation() %>% removeNumbers())
test = tweet_data %>% filter(day_hour== 1017)
## extra text cleaning

tweet_data = tweet_data %>% mutate(no_mentions = gsub('@\\S+', '', text) %>% replace_emoji() %>% 
                                     replace_hash() %>% replace_non_ascii() %>%
                                     removePunctuation() %>% removeNumbers())

## most common words

tidy_twitter = tweet_data %>% select(tweet_id, day_hour, clean_text)

count_words = tidy_twitter %>% unnest_tokens(word, clean_text) %>%
  count(word, sort = T) %>% filter(nchar(word)>1) %>% 
  anti_join(stop_words %>% filter(!word %in% c("no", "not") ))

head(count_words, n=15)

## ti-idf words
## tf_idf shows relevance/importance

content_words = tidy_twitter %>% unnest_tokens(word, clean_text) %>% count(day_hour, word, sort = T) %>% 
  filter(nchar(word)>1) %>% anti_join(stop_words %>% filter(!word %in% c("no", "not") ))

total_content_words = content_words %>% group_by(day_hour) %>% mutate(total = sum(n()))

content_tf_idf = content_words %>% bind_tf_idf(word, day_hour, n) %>% arrange(desc(tf_idf))

content_filtered = content_tf_idf %>% filter(day_hour>1014)

content_filtered %>%
  group_by(day_hour) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = day_hour)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~day_hour, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

## sentiment analysis

tweets_per_hour = tidy_twitter %>% group_by(day_hour) %>% summarize(tweet_count = n())

## most common sentiments

twitter_sentiment_words = tidy_twitter %>% unnest_tokens(word, clean_text) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment)

twitter_sentiment_words %>% group_by(sentiment) %>%
  slice_max(n, n = 15) %>%
  ungroup() %>%
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, ncol = 2, scales = "free") +
  labs(x = "count", y = NULL)

## most positive and most negative tweets

## bing

positive_sentiments = get_sentiments("bing") %>% filter(sentiment == 'positive') %>%
  select(word) %>% as_vector() %>%paste0(collapse = "|")

negative_sentiments = get_sentiments("bing") %>% filter(sentiment == 'negative') %>%
  select(word) %>% as_vector() %>% removePunctuation() %>% removeNumbers() %>%paste0(collapse = "|")


twitter_sentiment_counts = tweet_data %>%
  mutate(count_pos = str_count(tolower(no_mentions),positive_sentiments)) %>%
  mutate(count_neg = str_count(tolower(no_mentions),negative_sentiments)) %>%
  mutate(count_total = count_pos-count_neg)

twitter_positive = twitter_sentiment_counts %>% arrange(desc(count_total)) %>% head()
twitter_positive

twitter_negative = twitter_sentiment_counts %>% arrange(count_total) %>% head()
twitter_negative

## nrc

get_sentiments("nrc") %>% select(sentiment) %>% unique()

positive_sentiments = get_sentiments("nrc") %>% filter(sentiment %in% c("trust", "positive","joy","anticipation")) %>%
  select(word) %>% as_vector() %>%paste0(collapse = "|")

negative_sentiments = get_sentiments("nrc") %>% filter(sentiment %in% c("fear","negative","sadness","anger","disgust")) %>%
  select(word) %>% as_vector() %>% removePunctuation() %>% removeNumbers() %>%paste0(collapse = "|")


twitter_sentiment_counts = tweet_data %>%
  mutate(count_pos = str_count(tolower(no_mentions),positive_sentiments)) %>%
  mutate(count_neg = str_count(tolower(no_mentions),negative_sentiments)) %>%
  mutate(count_total = count_pos-count_neg)

twitter_positive = twitter_sentiment_counts %>% arrange(desc(count_total)) %>% head()
twitter_positive

twitter_negative = twitter_sentiment_counts %>% arrange(count_total) %>% head()
twitter_negative



## timeline

twitter_sentiment = tidy_twitter %>% unnest_tokens(word, clean_text) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(day_hour, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>%
  inner_join(y = tweets_per_hour %>% filter(tweet_count >=5), by = "day_hour") %>%
  mutate(negative = negative/tweet_count,
         positive = positive/tweet_count,
         sentiment = sentiment/tweet_count)


total_plot = ggplot(twitter_sentiment, aes(day_hour, sentiment, fill = sentiment)) +
  geom_col(show.legend = FALSE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

neg_plot = ggplot(twitter_sentiment, aes(day_hour, -negative, fill = negative)) +
  geom_col(show.legend = FALSE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

pos_plot = ggplot(twitter_sentiment, aes(day_hour, positive, fill = positive)) +
  geom_col(show.legend = FALSE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggarrange(pos_plot,neg_plot,total_plot, 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)


## topic modeling

twitter_dtm = tweet_data %>% select(tweet_id, no_mentions) %>% unnest_tokens(word, no_mentions) %>%
  anti_join(stop_words %>% filter(!word %in% c("no","not"))) %>% count(tweet_id,word) %>% cast_dtm(tweet_id, word, n)

twitter_lda = LDA(twitter_dtm, k = 3, control = list(seed = 1234))

twitter_topics = tidy(twitter_lda, matrix = "beta")

twitter_top_terms <- twitter_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

twitter_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

## topic 1 = food/sights, topic 2 = game, topic 3 = tickets

tweet_assignments = tidy(twitter_lda, matrix = "gamma") %>%
  group_by(document) %>% slice_max(gamma) %>% ungroup()

tweet_data_topic = tweet_data %>% left_join(y = tweet_assignments, by = c("tweet_id" = "document"))

topic1_tweets = tweet_data_topic %>% filter(topic == 1) %>% arrange(desc(gamma)) %>% select(tweet_id, text, topic, created_at, day_hour)
head(topic1_tweets, n=10)

topic2_tweets = tweet_data_topic %>% filter(topic == 2) %>% arrange(desc(gamma)) %>% select(tweet_id, text, topic, created_at, day_hour)
head(topic2_tweets, n=10)

topic3_tweets = tweet_data_topic %>% filter(topic == 3) %>% arrange(desc(gamma)) %>% select(tweet_id, text, topic, created_at, day_hour)
head(topic3_tweets, n=10)

