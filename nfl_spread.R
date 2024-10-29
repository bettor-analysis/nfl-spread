# NFL Favorite and Underdog Win Rates by Spread Line
# Adam Wickwire - Bettor Analysis - 10/29/2024
#
# This script calculates the win rates for favorites and underdogs in 
# NFL games based on the spread line. The data is sourced from the nflreadr 
# package, which provides historical NFL game data. The script calculates 
# the win rates for favorites and underdogs at each spread line, as well as
# the corresponding American odds for those win rates. 
#
# The script can be used to analyze the performance of favorites and underdogs
# at different spread lines and shows the fair market odds, providing insights for bettors and 
# sports analysts. The output includes the spread line, total games tested,
# favorite win rate, favorite odds, underdog win rate, and underdog odds.
# a total of 4804 games were tested in this analysis.
#


# load libraries
library(tidyverse)
library(nflreadr)


# Load the data
data <- load_schedules(seasons = TRUE)


# Select relevant columns and filter out games with missing home moneyline
data <- data %>% 
  select(away_score, home_score, away_moneyline, home_moneyline, spread_line) %>% 
  filter(!is.na(home_moneyline)) 


# Calculate margin of victory and determine winner
data <- data %>% 
  mutate(
    home_mov = home_score - away_score,
    home_win = if_else(home_mov > 0, 1, 0),
    away_win = if_else(home_mov < 0, 1, 0)
  )


# Take absolute value of spread_line if needed
data <- data %>%
  mutate(spread_line = abs(spread_line))


# Determine if favorite or underdog won
data <- data %>% 
  mutate(
    favorite_win = if_else(
      (home_win == 1 & home_moneyline < 0) | 
        (away_win == 1 & away_moneyline < 0), 
      1, 
      0
    ),
    underdog_win = if_else(
      (home_win == 1 & home_moneyline > 0) | 
        (away_win == 1 & away_moneyline > 0), 
      1, 
      0
    )
  )


# Select relevant columns
spread_data <- data %>% 
  select(spread_line, favorite_win, underdog_win)


# remove any na values for favorite_win and underdog_win
spread_data <- spread_data %>% 
  filter(!is.na(favorite_win), !is.na(underdog_win)) %>% 
  arrange(spread_line)


# combine spreads that are greater than 14.5 into one category 
spread_data <- spread_data %>%
  mutate(spread_category = if_else(spread_line > 14.5, "15+", as.character(spread_line)))

spread_summary <- spread_data %>%
  group_by(spread_category) %>%
  summarise(
    total_favorite_win = sum(favorite_win, na.rm = TRUE),
    total_underdog_win = sum(underdog_win, na.rm = TRUE),
    .groups = 'drop'  # This ensures the grouping is removed after summarising
  )


# Calculate win rates for favorite and underdog at each spread line
spread_win_rates <- spread_summary %>%
  mutate(
    total_games = total_favorite_win + total_underdog_win,
    favorite_win_rate = round(total_favorite_win / total_games, 3),
    underdog_win_rate = round(total_underdog_win / total_games, 3)
  )


# function to convert probabilities to american odds
percentage_to_american_odds <- function(p) {
  # Initialize the odds vector with NA
  odds <- rep(NA_integer_, length(p))
  
  # Calculate odds for favorites (p > 50%)
  favorite_idx <- which(p > 0.5)
  odds[favorite_idx] <- -round((p[favorite_idx] / (1 - p[favorite_idx])) * 100)
  
  # Calculate odds for underdogs (p < 50%)
  underdog_idx <- which(p < 0.5)
  odds[underdog_idx] <- round(((1 - p[underdog_idx]) / p[underdog_idx]) * 100)
  
  # Handle even odds (p == 50%)
  even_idx <- which(p == 0.5)
  odds[even_idx] <- 100
  
  # Optionally, set odds to NA for p = 0 or p = 100
  odds[p == 0 | p == 100] <- NA_integer_
  
  return(odds)
}


# Convert win rates to american odds
spread_win_rates <- spread_win_rates %>% 
  mutate(
    favorite_odds = percentage_to_american_odds(favorite_win_rate),
    underdog_odds = percentage_to_american_odds(underdog_win_rate)
  )


# sum the total games tested
total_games <- sum(spread_win_rates$total_games)
# total_games tested is 4804


# arrange the spread categories in order
spread_win_rates <- spread_win_rates %>%
  mutate(
    spread_category_num = case_when(
      spread_category == "15+" ~ Inf,                    
      TRUE ~ as.numeric(spread_category)                
    )
  )

spread_win_rates <- spread_win_rates %>%
  arrange(spread_category_num) %>%                         
  select(-spread_category_num)                            


# Set the final output data frame 
final_output <- spread_win_rates %>%
  select(spread_category, total_games, favorite_win_rate, 
         favorite_odds, underdog_win_rate, underdog_odds) %>% 
  rename(spread = spread_category)

print(final_output, n = Inf)







#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#




margin_of_victory <- data %>%
  select(away_score, home_score) %>% 
  mutate(mov = abs(home_score - away_score)) %>% 
  group_by(mov) %>% 
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  arrange(mov) %>% 
  filter(!is.na(mov))


margin_of_victory <- margin_of_victory %>% 
  mutate(percent = round(count / sum(count) * 100, 2))



  



