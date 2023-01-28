
# Oliver Eaton
# Begun: 2021-09-08

# Hashtag analysis

# Environ -----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(textclean)
library(janitor)
library(widyr)
library(tidymodels)
library(igraph)
library(ggraph)

source(here::here("functions", "database_functions.R")) # Database functions
source(here::here("functions", "help.R")) # Helper functions

# Read in data
influencer_tweets <- read_table("influencer_tweets")
influencer_mentions <- read_table("influencer_mentions")


# Set dataframes for analysis ---------------------------------------------

df_prep <- function(df, hash) {
  tmp <- hash_at_link_ratio(df) |> 
    filter(created_at >= (Sys.Date() - 30), str_detect(text, "#", negate = !hash)) |> 
    distinct(status_id, text)
}

# Working df
df <- bind_rows(df_prep(influencer_tweets, TRUE)
                , df_prep(influencer_mentions, TRUE))

# hashtag df
hashtag_df <- unnest_tweets(df, input = text, output = hashtag) |> 
  filter(str_starts(hashtag, "#")) |> 
  distinct() |> 
  mutate(hashtag = str_remove_all(hashtag, pattern = "[:emoji:]"))

# word df
word_df <- clean_text(df) |> 
  mutate(word = textstem::stem_words(word)) |> 
  filter(word != "")      


# Hashtag Similarity ------------------------------------------------------

# tf_idf
tfidf <- left_join(hashtag_df, word_df, by ="status_id") |>
  select(-status_id) |>
  na.omit() |> 
  with_groups(c(hashtag, word), ~ summarise(.x, n = n())) |> 
  bind_tf_idf(word, hashtag, n) |> 
  anti_join(stop_words, by = "word")

# cosine similarity - Hashtags that use similar words
# 1 = similar; 0 = not similar
similarity <- pairwise_similarity(tfidf, hashtag, word, tf_idf) |> 
  rename(.h_compared = item1, .h_to = item2) |>
  replace_na(list(similarity = 0)) |> 
  arrange(.h_compared, desc(similarity))

slim_sim <- filter(similarity, similarity >= 0.7) |> 
  rowwise() |> 
  mutate(c = as.character(list(sort(c(.h_compared, .h_to))))) |> 
  with_groups(c, ~ mutate(.x, c_c = row_number())) |> 
  filter(c_c == 1) |> 
  select(-ends_with("c"))


# Similar Hashtag Network -------------------------------------------------

# create undirected graph object
grph <- graph_from_data_frame(slim_sim, directed = FALSE)
plot(grph, layout = layout_with_gem(grph))


# K-means -----------------------------------------------------------------

library(parameters)

tfidf_wide <- rename(tfidf, .word=word) |> 
  pivot_wider(id_cols = .word, names_from = hashtag, values_from = tf_idf, names_repair = "minimal", values_fill = 0)
# SCALE

n_clust <- n_clusters(tfidf_wide[,-1])
kmeans(tfidf_wide[,-1], centers = 3)



