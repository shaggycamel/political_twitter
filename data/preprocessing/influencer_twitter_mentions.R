
# Oliver Eaton
# Begun: 2021-05-22

# File to obtain twitter mentions of chosen political leaders.

# Init message ------------------------------------------------------------

print("Influencer twitter mentions...")
print("")

# Get Mentions ------------------------------------------------------------

# First read in mentions from database
mnt_old <- read_table("influencer_mentions")

# Second get latest mention_id for each user from mnt_old
mnt_id <- left_join(select(influencer_id, mentions_user_id_single=user_id), mnt_old, by = "mentions_user_id_single") |> 
  group_by(mentions_user_id_single) |>
  slice_max(order_by = ~ -status_id, with_ties = FALSE) |> 
  select(mentions_user_id_single, mention_status_id = status_id, mention_created_at = created_at)

# Get screen names
screen_names_mnt_id <- filter(influencer_twitter_details, as_of_date == max(as_of_date)) |> 
  select(as_of_date, user_id, screen_name) |> 
  left_join(mnt_id, by =  c("user_id"="mentions_user_id_single"))

# Lastly limit mention collection from last mention of each user
mnt_new <- map2_df(
  .x = paste0("@", screen_names_mnt_id$screen_name)
  , .y = screen_names_mnt_id$mention_status_id
  , ~ tweets_with_users(search_tweets(.x, since_id = .y, parse = FALSE))
) |> 
  mutate(
    # Convert lists to character
    across(where(is.list), as.character)
    
    # Split user_ids mentioned into a single cell
    , mentions_user_id_single = str_replace_all(
      string = mentions_user_id
      , pattern = c(
        "^c" = ""
        , "\\(" = ""
        , "\\)$" = ""
        , '"' = ""
      )
    ) 
  ) |> 
  #Seperate rows of user_ids into single cell
  separate_rows(mentions_user_id_single) |>
  relocate(mentions_user_id_single, .after = mentions_user_id) |> 
  
  # Filter to exclude non-influencers and retweets
  filter(
    mentions_user_id_single %in% (influencer_id$user_id)
    , is_retweet == FALSE
  ) |> 
  distinct()


# Influencer mentions
influencer_mentions <- bind_rows(mnt_old, mnt_new) |> 
  group_by(status_id, mentions_user_id_single) |> 
  slice_max(
    order_by = c(statuses_count, quoted_retweet_count, quoted_statuses_count) 
    , with_ties = FALSE
  )

# Remove unused objects
rm(mnt_old, mnt_new, mnt_id, screen_names_mnt_id)
