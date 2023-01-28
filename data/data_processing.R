
# Oliver Eaton
# Begun: 2021-05-14
#
# Driver file to gather tweets from political leaders across
# Australia and New Zealand

# Environ -----------------------------------------------------------------

# Start timer
start_time <- Sys.time()

# Package management
packages <- read.delim("./package_management/data_processing.dep", header = FALSE)[[1]]
source("./package_management/pacman_script.R") |> suppressWarnings()

# rtweet configuration
source(here("data", "config", ".rtweet_config.R"))

# database functions
source(here("functions", "database_functions.R"))

# Preprocessing -----------------------------------------------------------

source(here("data", "preprocessing", "influencer_twitter_id.R")) |> try()
source(here("data", "preprocessing", "influencer_twitter_details.R")) |> try()
source(here("data", "preprocessing", "influencer_twitter_tweets.R")) |> try()
# source(here("data", "preprocessing", "influencer_twitter_friends.R")) |>  try()
source(here("data", "preprocessing", "influencer_twitter_mentions.R")) |> try()

# Write to Database -------------------------------------------------------

if(exists("influencer_twitter_details")) write_table(influencer_twitter_details)
if(exists("influencer_tweets")) write_table(influencer_tweets)
if(exists("influencer_friends")) write_table(influencer_friends)
if(exists("influencer_mentions")) write_table(influencer_mentions)

# Log File ----------------------------------------------------------------

# End timer
end_time <- Sys.time()
total_time <- round(difftime(end_time, start_time, units = "secs"), 2)

# Print to log file
write(
  paste(
    start_time
    , end_time
    , total_time
    , (if(exists("influencer_id")) nrow(influencer_id) else 0)
    , (if(exists("influencer_twitter_details")) nrow(influencer_twitter_details) else 0)
    , (if(exists("influencer_tweets")) nrow(influencer_tweets) else 0)
    , (if(exists("influencer_friends")) nrow(influencer_friends) else 0)
    , (if(exists("influencer_mentions")) nrow(influencer_mentions) else 0)
    , sep = "\t"
  )
  , file = here(".logs/r.log")
  , append = TRUE
)

# Print complete message
print("")
print(paste0(Sys.Date(), ": political_twitter database updated..."))
print(total_time)
