# Load necessary libraries
library(nnet)
library(dplyr)

# Load the batter data from CSV, edit to where "data.csv" is found
batter_data <- read.csv("/Users/CameronPoppen/Downloads/data.csv")

# Categorize pitches into FB, BB, and OS
batter_data <- batter_data %>%
  mutate(PITCH_CATEGORY = case_when(
    PITCH_TYPE %in% c("FF", "FC", "SI", "FA") ~ "FB",
    PITCH_TYPE %in% c("CU", "SL", "KC", "ST", "SV", "CS", "SC") ~ "BB",
    PITCH_TYPE %in% c("CH", "EP", "KN", "FO", "FS") ~ "OS"
  )) %>%
  filter(!is.na(PITCH_CATEGORY))  # Remove rows with missing PITCH_CATEGORY

# Train the multinomial logistic regression model
batter_data_agg <- batter_data %>%
  group_by(BATTER_ID, BAT_SIDE) %>%
  summarise(
    BALLS = mean(BALLS, na.rm = TRUE),   # Average balls count for the batter
    STRIKES = mean(STRIKES, na.rm = TRUE),  # Average strikes count for the batter
    INNING = mean(INNING, na.rm = TRUE),  # Average inning for the batter
    THROW_SIDE = first(THROW_SIDE)  # Ensure THROW_SIDE remains consistent
  ) %>%
  ungroup()

# Train the multinomial logistic regression model
multinom_model <- multinom(PITCH_CATEGORY ~ BATTER_ID + BAT_SIDE + THROW_SIDE + BALLS + STRIKES + INNING, 
                           data = batter_data, trace = FALSE)

# Load the prediction dataset from CSV (edit the file path accordingly)
predictions <- read.csv("/Users/CameronPoppen/Downloads/predictions.csv")

# Merge necessary features from batter_data_agg into predictions
predictions <- predictions %>%
  left_join(batter_data_agg %>% select(BATTER_ID, BAT_SIDE, BALLS, STRIKES, INNING, THROW_SIDE), by = "BATTER_ID")

# Make predictions using the multinomial logistic regression model
predicted_probs <- predict(multinom_model, newdata = predictions, type = "probs")

# Convert the predicted probabilities to proportions for FB, BB, and OS
predictions <- predictions %>%
  mutate(
    PITCH_TYPE_FB = round(predicted_probs[, "FB"], 4),  # Proportion of fastballs
    PITCH_TYPE_BB = round(predicted_probs[, "BB"], 4),  # Proportion of breaking balls
    PITCH_TYPE_OS = round(predicted_probs[, "OS"], 4),  # Proportion of off-speed pitches
    GAME_YEAR = 2024  # Add the 2024 year to the output
  )

# Aggregate by BATTER_ID, combining left and right batting side predictions
submission <- predictions %>%
  group_by(BATTER_ID) %>%
  summarise(
    PLAYER_NAME = first(PLAYER_NAME),  # Take the first player name if duplicated
    GAME_YEAR = first(GAME_YEAR),      # Ensure GAME_YEAR is 2024
    PITCH_TYPE_FB = mean(PITCH_TYPE_FB), # Average proportions
    PITCH_TYPE_BB = mean(PITCH_TYPE_BB), 
    PITCH_TYPE_OS = mean(PITCH_TYPE_OS)
  ) %>%
  ungroup()

# Save the final output to a CSV file
write.csv(submission, "/Users/CameronPoppen/Downloads/predictions.csv", row.names = FALSE)