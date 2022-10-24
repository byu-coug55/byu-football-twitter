# byu-football-twitter
Trying out the Twitter API

https://www.linkedin.com/pulse/byu-football-twitter-analysis-m-lance-christian/

One of the most anticipated games in the second week of college football play was the BYU-Baylor game. BYU will be moving into the Big 12 Conference in 2023 and Baylor will be an in-conference rival. Last year, BYU lost to Baylor in Texas 38 to 24 in a game where Baylor dominated in the run and BYU struggled. This year's match-up looked even more bleak for BYU as two of their strongest playmakers were injured (G. Romney and P. Nacua) in addition to being ranked lower on the AP poll. Before the game BYU was ranked 21st, while Baylor was ranked 9th.
With all this in play, I was interested in how the BYU fanbase felt about the game and what they were saying on Twitter about it. So naturally, I opened up my laptop and went directly to R! I recently read through Julia Silge's and David Robinson's book Text Mining with R [https://www.tidytextmining.com/]. This book goes into detail about different text mining techniques including summary metrics, sentiment analysis, term frequency-inverse document frequency (tf-idf), and topic modeling. I decided to use these techniques to perform my BYU Football Twitter Analysis.
So let's dig in. First up, loading the libraries:

```
library(tidyverse) 
library(textclean)
library(tidytext)
library(tm)
library(lubridate)
library(stringr)
library(jsonlite)
library(ggpubr)
library(topicmodels)
```
I then set up my API call so I can receive Twitter data. Twitter does a very good job of documenting its API endpoints and helping users accomplish what they set out to do. This article helped me quickly get up and running with Twitter's API. For my use-case I used the mentions endpoint and found @BYUfootball's user ID (874824792) using tweeterid.com.
```
twitter_creds <- jsonlite::fromJSON("api_keys_twitter.json")
twitter_url <- "https://api.twitter.com/2/"

bearer_token <- twitter_creds$bearer_token
headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

params <- list(`max_results` = 100,
               `start_time` = '2022-09-06T00:00:00.000Z',
               `tweet.fields` = 'author_id,created_at')

url_handle <- 'https://api.twitter.com/2/users/874824792/mentions'
```

I passed three parameters into my query: max_results, start_time, and tweet.fields. If I remember right, Twitter's default is to only send 5 or 10 tweets through the API call unless a "max_results" is specified. This API endpoint returns the most recent tweets, so I defined the boundary "start_time" to ensure I was only getting tweets from the days immediately leading up to the game and not going back any further. I also wanted to know when the tweet was created so I could use this data as part of my analysis.
I then collected data from my first API call:

```
response <-
  httr::GET(url = url_handle,
            httr::add_headers(.headers = headers),
            query = params)
obj <- httr::content(response, as = "text")

json_data <- fromJSON(obj, flatten = TRUE) %>% as.data.frame

data = json_data %>% select(tweet_id = data.id, text = data.text, author_id = data.author_id, created_at = data.created_at)
```
And then 9 more pages for a total of 1,000 tweets:
```
for (i in 1:9){
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
 
  int_data = json_data %>% 
    select(tweet_id = data.id, text = data.text, author_id = data.author_id, created_at = data.created_at)
  data = bind_rows(data,int_data)
  
}
```

Twitter's API uses pagination via the "pagination_token". You pass "next_token" to that parameter to collect the next page of data. For each page, I bind the rows of the previously collected data with the data from the new page.
The next step was to clean and prep the data. I first created a new column called "day_hour" so I can group the data by the hour the tweet was created. Next I cleaned the text column by using a few functions from "textclean" (replace_emoji, replace_hash, and replace_non_ascii) and a few functions from "tm" (removePunctuation and removeNumbers). These functions make it easier to create a meaningful analysis. I also added a column that removed all mentions as this was interfering with part of my sentiment analysis later.
```
tweet_data = data %>% mutate(created_at = created_at %>% ymd_hms()) %>%
  mutate(day_hour = paste0(format.Date(created_at,"%d"),format.Date(created_at,"%H")))

tweet_data = tweet_data %>% mutate(clean_text = text %>% replace_emoji() %>% 
                                     replace_hash() %>% replace_non_ascii() %>%
                                     removePunctuation() %>% removeNumbers())

tweet_data = tweet_data %>% mutate(no_mentions = gsub('@\\S+', '', text) %>% replace_emoji() %>% 
                                     replace_hash() %>% replace_non_ascii() %>%
                                     removePunctuation() %>% removeNumbers())
```

So now that the cleaning and prep is done, it's time for some analysis!
Summary Metrics
First let's see the most common words used in our collection of tweets.
```
tidy_twitter = tweet_data %>% select(tweet_id, day_hour, clean_text)

count_words = tidy_twitter %>% unnest_tokens(word, clean_text) %>%
  count(word, sort = T) %>% filter(nchar(word)>1) %>% 
  anti_join(stop_words %>% filter(!word %in% c("no", "not") ))

head(count_words, n=15)
```

![image](https://user-images.githubusercontent.com/56312233/197543486-86a07aaf-43ee-4c82-b626-94f2af980f12.png)


As you would expect, "byufootball" is our most commonly used word (@BYUfootball's handle) along with Baylor football team's handle coming in second. @RGIII is Robert Griffin III's twitter handle. RG3 was one of the commentators for the game which many people were excited about as he is one of Baylor's most accomplished alumni (at least in the football world). I ran my final script at the end of the second quarter so many people were tweeting about Chase Roberts's toe-tapping corner of the endzone touchdown! One thing to note here is that "joy" actually refers to this emoji 😂.

## Term Frequency-Inverse Document Frequency
I then used the tf-idf technique to find the most important/relevant words tweeted throughout the timeframe of my analysis:

```
content_words = tidy_twitter %>% unnest_tokens(word, clean_text) %>% count(day_hour, word, sort = T) %>%
  filter(nchar(word)>1) %>% anti_join(stop_words %>% filter(!word %in% c("no", "not") )) 

total_content_words = content_words %>% group_by(day_hour) %>% mutate(total = sum(n()))

content_tf_idf = content_words %>% bind_tf_idf(word, day_hour, n) %>% arrange(desc(tf_idf))

content_filtered = content_tf_idf %>% filter(day_hour>=1100)

content_filtered %>%
  group_by(day_hour) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = day_hour)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~day_hour, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
```

I limited the below chart to the 5 most recent hours before halftime of the BYU-Baylor game:

![image](https://user-images.githubusercontent.com/56312233/197543693-fc55b82a-2ce9-412d-9f7e-476cbbeeb241.png)

"1104" is the day (11) and hour (04) in UTC, the same hour as the end of the second quarter when Chase Roberts had his amazing touchdown grab. The hour before a few users were tweeting about how the refs were favoring BYU. During hour "1101" several people included their row and seat number in their tweets.

## Sentiment Analysis

### Most Common Sentiments

Let's dive into a sentiment analysis now;
```
tweets_per_hour = tidy_twitter %>% group_by(day_hour) %>% summarize(tweet_count = n()

twitter_sentiment_words = tidy_twitter %>% unnest_tokens(word, clean_text) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment))

twitter_sentiment_words %>% group_by(sentiment) %>
  slice_max(n, n = 15) %>%
  ungroup() %>%
  ggplot(aes(n, fct_reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, ncol = 2, scales = "free") +
  labs(x = "count", y = NULL)%
```
Here I show the most common words used to express positive/negative sentiment:

![image](https://user-images.githubusercontent.com/56312233/197545044-8153c478-e93f-4ed8-911e-1d06466db55e.png)

You can see that overall, positive sentiments are used much more frequently than negative sentiments. In fact, the 15th most common positive sentiment "enjoy" was used more often than the most common negative sentiment "hard".

### Most Positive and Negative Tweets

Sentiment analysis also allows us to find the most positive and most negative tweets:
```
positive_sentiments = get_sentiments("bing") %>% filter(sentiment == 'positive') %>%
  select(word) %>% as_vector() %>%paste0(collapse = "|")

negative_sentiments = get_sentiments("bing") %>% filter(sentiment == 'negative') %>%
  select(word) %>% as_vector() %>% removePunctuation() %>% removeNumbers() %>%paste0(collapse = "|")


twitter_sentiment_counts = tweet_data %>%
  mutate(count_pos = str_count(tolower(no_mentions),positive_sentiments)) %>%
  mutate(count_neg = str_count(tolower(no_mentions),negative_sentiments)) %>%
  mutate(count_total = count_pos-count_neg)
```

Here are the most positive tweets using the Bing sentiment lexicon:
```
twitter_positive = twitter_sentiment_counts %>% arrange(desc(count_total)) %>%
  select(tweet_id,text,count_total) %>% head()
twitter_positive 
```
![image](https://user-images.githubusercontent.com/56312233/197545205-c09a6ecc-6601-4431-9edf-ef3eebdb63d9.png)

You can tell these people were very excited and very positive about this game!
Here are the top negative tweets:
```
twitter_negative = twitter_sentiment_counts %>% arrange(count_total) %>%
  select(tweet_id,text,count_total) %>% head()
twitter_negative 
```
![image](https://user-images.githubusercontent.com/56312233/197545300-5a382ab5-bbe3-4932-8296-cf37d90ab6bf.png)

I did not feel that the Bing sentiment lexicon accurately captured the most negative tweets, so I performed the analysis again but with the NRC lexicon this time.
Set-up with NRC lexicon:
```
positive_sentiments = get_sentiments("nrc") %>% filter(sentiment %in% c("trust", "positive","joy","anticipation")) %>%
  select(word) %>% as_vector() %>%paste0(collapse = "|")

negative_sentiments = get_sentiments("nrc") %>% filter(sentiment %in% c("fear","negative","sadness","anger","disgust")) %>%
  select(word) %>% as_vector() %>% removePunctuation() %>% removeNumbers() %>%paste0(collapse = "|")


twitter_sentiment_counts = tweet_data %>%
  mutate(count_pos = str_count(tolower(no_mentions),positive_sentiments)) %>%
  mutate(count_neg = str_count(tolower(no_mentions),negative_sentiments)) %>%
  mutate(count_total = count_pos-count_neg)
```
Most positive tweets using the NRC sentiment lexicon:
```
twitter_positive = twitter_sentiment_counts %>% arrange(desc(count_total)) %>%
  select(tweet_id,text,count_total) %>% head()
twitter_positive 
```
![image](https://user-images.githubusercontent.com/56312233/197545452-bb6394b9-fe5d-4a61-a7c1-a613bce16658.png)

You'll notice that this analysis picked up on the positive emoji's.
Most negative NRC tweets:
```
twitter_negative = twitter_sentiment_counts %>% arrange(count_total) %>%
  select(tweet_id,text,count_total) %>% head()
twitter_negative 
```
![image](https://user-images.githubusercontent.com/56312233/197545524-f5ab6ed3-ebad-44a6-b55c-67ee35988bb1.png)

So I don't think either lexicon perfectly captured the most negative tweets, but by viewing both we can get a good idea of the negative sentiments surrounding the game. One point that could be improved here are the counting of double negatives like "not disapoint".

## Timeline

Now that we have an idea of what the most positive and negative tweets are, let's analyze the timeline of the tweets.
```
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
```

Here I count the number of positive/negative sentiment words used each hour and then normalize for the number of tweets in that hour. This shows the most positive/negative counts per tweet per hour.

![image](https://user-images.githubusercontent.com/56312233/197545704-791bfe5a-fa31-4de7-a0e5-9b084d4d5a78.png)

You can see people are most positive the night before the game and a few hours before the game starts.

## Topic Modeling

So far I have been using "tidy text" dataframes that are easily used in the tidyverse. To perform topic modeling, I will need to create a document-term-matrix.
```
twitter_dtm = tweet_data %>% select(tweet_id, no_mentions) %>%
  unnest_tokens(word, no_mentions) %>% anti_join(stop_words %>% 
  filter(!word %in% c("no","not"))) %>% count(tweet_id,word) %>% cast_dtm(tweet_id, word, n) 
```

I will use LDA as my method for topic modeling. Let's now find the words that have the highest association with the 3 topics we will analyze:
```
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
```
![image](https://user-images.githubusercontent.com/56312233/197545905-aabbed16-8c2c-4c9c-8496-f918f2d6407d.png)

There are two key points to understand about the LDA method of topic analysis:
- Every document is a mixture of topics. This means that each tweet can have a certain percentage of association with each of the three topics
- Every topic is a mixture of words and each word has a certain association with each of the three topics.

These two key points help us to understand that the topics will have some overlap, as does natural language. For example, the word "game" shows up in each of the three topics, but most likely has different context within the three topics.
The above graph gives us an idea of what the topics may be, but it becomes more clear once we look at the most highly associated tweets per topic.

## Topic Assignments
```
tweet_assignments = tidy(twitter_lda, matrix = "gamma") %>%
  group_by(document) %>% slice_max(gamma) %>% ungroup()

tweet_data_topic = tweet_data %>% left_join(y = tweet_assignments, by = c("tweet_id" = "document"))
```

### Topic 1
```
topic1_tweets = tweet_data_topic %>% filter(topic == 1) %>% arrange(desc(gamma)) %>% select(tweet_id, text, topic, created_at, day_hour)
head(topic1_tweets, n=10)
```
![image](https://user-images.githubusercontent.com/56312233/197546159-d805dfd6-de4a-4373-980c-feac2465346b.png)

So what is Topic 1? I would classify Topic 1 as tweets having to do with food and sight-seeing.

### Topic 2
```
topic2_tweets = tweet_data_topic %>% filter(topic == 2) %>% arrange(desc(gamma)) %>% select(tweet_id, text, topic, created_at, day_hour)
head(topic2_tweets, n=10)
```
![image](https://user-images.githubusercontent.com/56312233/197546264-23c962c0-db06-40f4-852a-b6d55093e30f.png)

I would classify topic two as game details and general hype.

### Topic 3
```
topic3_tweets = tweet_data_topic %>% filter(topic == 3) %>% arrange(desc(gamma)) %>% select(tweet_id, text, topic, created_at, day_hour)
head(topic3_tweets, n=10)
```
![image](https://user-images.githubusercontent.com/56312233/197546360-d7c75f08-2218-4d03-91c7-1af5b32aaba8.png)

Topic 3 seems to be a mix of everything else. The first few tweets for this topic are about the BYU Jaren Hall to Chase Roberts touchdown to end the second quarter or BYU's offense.

## Conclusion

Overall we discovered that most tweets mentioning @BYUfootball were positive over the timeframe and that the most positive tweets per hour happened the night before the game as well as a few hours before the game started. The topic of many tweets surrounded the food scene and sight-seeing in Provo as well as general game details and hype for the game.

If you enjoyed this analysis or if you have any feedback, please let me know!

