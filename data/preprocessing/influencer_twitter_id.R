
# Oliver Eaton
# Begun: 2021-05-22

# File to read in twitter ids of political leaders.
# ID's were handpicked and originate from ./data/config/influencer_config.R

# # Init message ------------------------------------------------------------

print("Influencer twitter id...")
print("")

# Get Twitter id ----------------------------------------------------------

influencer_id <- read_table("influencer_id") |> 
  filter(is.na(research_exclusion_date)) |>
  arrange(desc(user_id))

# Figure out how to handle influencers that have been
# removed from the study

