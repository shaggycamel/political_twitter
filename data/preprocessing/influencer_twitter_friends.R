
# Oliver Eaton
# Begun: 2021-05-22

# File to gather twitter friends of chosen political leaders.

# Init message ------------------------------------------------------------

print("Influencer twitter friends...")
print("")

# Get Friends -------------------------------------------------------------

# Gather Friends function
g_frnds <- function(u_id){
  
  n_friends = lookup_users(u_id)$friends_count
  curr_page = -1
  fetched_friends = 0
  i = 0
  all_friends = NULL
  
  if(n_friends == 0){
    
    print(paste0("Zero friends for user: ", u_id))
    tibble(user = as.character(u_id), user_id = NA_character_)
    
  } else {
    while(fetched_friends < n_friends)  {

      if(rate_limit("get_friends")$remaining == 0) {
        print(paste0("API limit reached. Reseting at ", rate_limit("get_friends")$reset_at))
        Sys.sleep(as.numeric((rate_limit("get_friends")$reset + 0.1) * 60))
      }

      i <- i + 1
      curr_friends <- get_friends(u_id, n = 5000, retryonratelimit = TRUE, page = curr_page)
      all_friends <- bind_rows(all_friends, curr_friends)
      fetched_friends <- nrow(all_friends)
      print(paste0(i, ". ", fetched_friends, " out of ", n_friends, " fetched for user: ", u_id))
      
      # Error handling
      if(nrow(curr_friends) == 0){
        print("Error handling...")
        break
      }
      curr_page <- next_cursor(curr_friends)
      
    }
    distinct(all_friends)
  }
}

# First read in friends from db
read_table("influencer_friends") |> {\(.){
    fr_csd <<- dplyr::filter(., !is.na(date_friendship_ceased))
    fr_old <<- dplyr::filter(., is.na(date_friendship_ceased))
}}()
# Ceased friendships are separated so it doesn't affect the situation
# where a user re-kindles that friendship.

# Second gather friends from twitter

fr_new_tmp <- map_df(.x = influencer_id$user_id, ~ g_frnds(u_id = .x)) |>  
  rename(user_id = user, friend_user_id = user_id)

# Third compare lists from first and second step
## New friendships
fr_new <- map_df(influencer_id$user_id, ~ {
  
  setdiff(
    fr_new_tmp |> filter(user_id == .x)
    , fr_old |> select(contains("user_id")) |> filter(user_id == .x)
  ) |> 
    mutate(
      date_friendship_began = Sys.Date()
      , date_friendship_ceased = NA
    )
  
})

## Removed friendships
walk(influencer_id$user_id, ~{
  
  fr_rem <- setdiff(
    fr_old |> filter(user_id == .x) |> select(contains("user_id"))
    , fr_new_tmp |> filter(user_id == .x)
  )
  
  fr_old$date_friendship_ceased[
    fr_old$user_id == .x & 
      fr_old$friend_user_id %in% fr_rem$friend_user_id
  ] <<- Sys.Date()
  
})

# Influencer friends
influencer_friends <- bind_rows(fr_csd, fr_old, fr_new)

# Remove unused objects
rm(fr_csd, fr_old, fr_new, fr_new_tmp, g_frnds)
