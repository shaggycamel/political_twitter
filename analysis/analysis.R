
# Oliver Eaton
# Begun: 2021-05-27

# Analysis of political twitter data

# Environ -----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(cld2)
library(SnowballC)
library(textstem)
library(wordcloud2)
library(wordcloud)
library(textclean)
library(tm)
library(doc2vec)

# Database functions
source(here::here("data", "preprocessing", "database_functions.R"))

# Read in data
tweets <- read_table("influencer_tweets")
mentions <- read_table("influencer_mentions")
friends <- read_table("influencer_friends")
details <- read_table("influencer_twitter_details") |> 
  group_by(user_id) |> 
  slice_max(as_of_date)

pols <- read_csv(
  here::here("data", "influencer_political_details.csv")
  , col_types = cols(twitter_id = col_character())
)

# Think about removing retweets from database
table(mentions$is_retweet)
table(tweets$is_retweet)

# Mention Sentiment -------------------------------------------------------
# Remove URLs, remove foreign language tweets.
# Concat tweets that have the same tweet_id but different text, try find out 
# which tweet should come first in that sequence 

# To detect language cld2 package (could also try franc)

mnt_anl <- filter(mentions,  is_retweet==FALSE) |> 
  select(status_id, created_at, mentions_user_id_single, text) |> 
  mutate(
    # Remove handles mentioned
    text = str_to_lower(str_squish(str_remove_all(text, "@[\\w\\d]+")))
    # Remove URLS
    , text = str_squish(str_remove_all(text, "https?\\S*"))
    # Detect language
    , language = detect_language(text)
  ) |> 
  filter(language == "en")




  # unnest_tokens(word, text, token = "sentences") |> 
  # anti_join(get_stopwords()) |> 
  # inner_join(get_sentiments("bing"), by = "word")


sentiment_ratio <- mnt_anl |> 
  group_by(mentions_user_id_single) |> 
  count(sentiment) |> 
  left_join(
    details |> select(user_id, name)
    , by = c("mentions_user_id_single"="user_id")
  )


# Tweet topic modelling ---------------------------------------------------

twt_anl <- filter(tweets, is_retweet == FALSE) |> 
  filter(created_at > max(created_at) - lubridate::days(30)) |> 
  select(status_id, text) |> 
  mutate(
    text = str_to_lower(text)
    # Replace ampersands with "and"
    , text = str_replace_all(text, "&amp;", "and")
    # Remove twitter handles and hashtags
    , text = str_remove_all(text, "(@|#)[_a-z0-9]+")
    # Remove URLS
    , text = str_remove_all(text, "https?\\S*")
    # Replace contractions - turns "it's" into "it is"
    , text = replace_contraction(text)
    # Replace internet slang - omg = oh my god
    , text = replace_internet_slang(text)
    # Replace word elongation
    , text = replace_word_elongation(text)
    # Remove emoji (ascii characters)
    , text = replace_non_ascii(text)
    # Remove punctuation
    , text = str_remove_all(text, "[:punct:]")
    # Remove numbers
    , text = str_remove_all(text, "[:digit:]")
    # Final chance at non-word characters
    , text = str_replace_all(text, "\\W", " ")
    # Clean up
    , text = str_squish(text)
  ) |>
  filter(text != "") |> 
  mutate(text = str_replace_all(text, c(
    "\\bnz"="new-zealand"
    , "new zealand"="new-zealand"
    , "\\baus\\b"="autralia"
    , "\\bgovt"="government" 
  ))) |> 
  # Unnest by space, unnestting by word split apart "new-zealand"
  unnest_tokens(text, text, drop = FALSE, token = "regex", pattern = " ") |>
  filter(!text %in% stopwords::stopwords("en", "stopwords-iso")) |> 
  mutate(text = wordStem(text)) |> 
  filter(!str_detect(text, c("govern|morrison|scott|australia|minist")))
  
wordcloud2(slice_max(count(twt_anl, text), order_by=n, n=100))

# tf-idf
tf_idf <- count(twt_anl, status_id, text) |> 
  # get rid of small tweets after cleaning
  anti_join(filter(count(twt_anl, status_id), n <= 5), by = "status_id") |> 
  bind_tf_idf(text, status_id, n) |> 
  group_by(status_id) |> 
  slice_max(order_by = tf_idf, n = 3) |> 
  ungroup() |> 
  count(text, sort = TRUE)

# topic modelling
library(stm)
library(quanteda)

twt_dfm <- count(twt_anl, status_id, text, sort = TRUE) |> 
  cast_dfm(status_id, text, n)

topic_model <- stm(twt_dfm, K = 3, init.type = "LDA")

td_beta <- tidy(topic_model) |> 
  group_by(topic) |> 
  slice_max(order_by = beta, n = 10)
head(td_beta, 30)

td_gamma <- tidy(topic_model, matrix = "gamma", document_names = rownames(twt_dfm))

twt_topics <- group_by(td_gamma, document) |> 
  slice_max(order_by = gamma, n = 1)
view(twt_topics)
