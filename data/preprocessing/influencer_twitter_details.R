
# Oliver Eaton
# Begun: 2021-05-22

# File to gather twitter details from chosen political leaders.

# Init message ------------------------------------------------------------

print("Influencer twitter details...")
print("")

# Get Twitter Details -----------------------------------------------------

# First read in details from db
dt_old <- read_table("influencer_twitter_details")

# Second gather details from twitter
dt_new <- lookup_users(influencer_id$user_id) |>
  select(
    user_id
    , account_created_at
    , screen_name
    , name
    , location
    , description
    , latest_tweet_id = status_id
    , followers_count
    , friends_count
    , listed_count
    , statuses_count
    , favourites_count
  ) |>
  mutate(as_of_date = as.POSIXct(Sys.Date())) |>
  relocate(as_of_date)

# Influencer twitter details
influencer_twitter_details <- bind_rows(dt_new, dt_old)

# Drop unused objects
rm(dt_old, dt_new)


