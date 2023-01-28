
# Oliver Eaton
# Begun: 2021-05-22

# File to gather tweets from chosen political leaders.

# Init message ------------------------------------------------------------

print("Influencer twitter tweets...")
print("")

# Get Tweets --------------------------------------------------------------

# First read in tweets from database
tw_old <- read_table("influencer_tweets")

# Second get latest tweet_id for each user from old_tw
tw_id <- left_join(influencer_id["user_id"], tw_old, by = "user_id") |>
  group_by(user_id) |> 
  slice_max(order_by = ~ -status_id, with_ties = FALSE) |> 
  select(user_id, status_id, created_at)

# Lastly limit tweet collection from last tweet sent by each user
tw_new <- map2_df(
  .x = tw_id$user_id
  , .y = tw_id$status_id
  , ~ get_timelines(user = .x, retryonratelimit = TRUE, since_id = .y)
) |> 
  mutate(across(where(is.list), as.character))


# Influencer tweets
influencer_tweets <- bind_rows(tw_new, tw_old)

# Remove unused objects
rm(tw_old, tw_new, tw_id)
