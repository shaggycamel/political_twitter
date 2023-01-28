
# Oliver Eaton
# Begun: 2021-09-11

# Helper functions for the Twitter analysis

# Hash, @, link ratio -----------------------------------------------------

hash_at_link_ratio <- function(df){
  
  df |> 
    mutate(
      token_count = str_count(str_squish(text), " ")+1
      , hash_at_link_count = str_count(text, "#|@|http")
      , hash_at_link_ratio = hash_at_link_count / token_count
    ) |> 
    filter(token_count >= 10, hash_at_link_ratio <= 0.60)
  
}


# General text cleaning ---------------------------------------------------

clean_text <- function(df){
  
  df <- mutate(df, language = cld2::detect_language(text)) |> 
    filter(language == "en") |> 
    mutate(
      text = str_replace_all(text, "&amp;", "and")
      , text = str_remove_all(text, "(@|#)[_a-z0-9]+")
      # , text = replace_names(text)
      , text = replace_email(text)
      , text = replace_word_elongation(text)
      , text = replace_contraction(text)
      , text = replace_non_ascii(text)
      # , text = replace_hash(text)
      , text = str_remove_all(text, "[:digit:]")
    ) |> 
    na.omit() |> 
    filter(strip(text) != "") |> 
    select(-language)
  
}