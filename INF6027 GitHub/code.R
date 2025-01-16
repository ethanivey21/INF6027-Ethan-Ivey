#TEST 1 



library(ggplot2)  
library(dplyr)    
library(lubridate) 

# Read the dataset 
data <- read.csv("C:/Users/ethan/Downloads/cleaned_data/cleaned_match_info.csv")

View(data)

# Filter matches where England is either the home or away team
england_matches <- subset(data, hometeamname == "England" | awayteamname == "England")


england_matches <- england_matches %>%
  mutate(
    england_goals = ifelse(hometeamname == "England", scorehome, scoreaway),  # Goals scored by England
    opponent_goals = ifelse(hometeamname == "England", scoreaway, scorehome), # Goals scored by the opponent
    aggregate_score = england_goals - opponent_goals,  # Goal difference (aggregate score)
    point_color = case_when(
      aggregate_score > 0 ~ "green",  # Green for wins
      aggregate_score == 0 ~ "grey",  # Grey for draws
      TRUE ~ "red"  # Red for losses
    )
  )

# Convert 'dateandtimecet' to a Date object for easier date handling
england_matches <- england_matches %>%
  mutate(match_date = as.Date(dateandtimecet))

# Aggregate England's goal difference (aggregate score) by match date
england_aggregate_scores <- england_matches %>%
  group_by(match_date) %>%  # Group data by match date
  summarise(
    aggregate_score = sum(aggregate_score),  # Sum of aggregate scores per match date
    final_score = paste(england_goals, ":", opponent_goals),  # Combine scores into "England's Goals:Opponent's Goals"
    point_color = case_when(
      aggregate_score > 0 ~ "green",  # Green for wins
      aggregate_score == 0 ~ "grey",  # Grey for draws
      TRUE ~ "red"  # Red for losses
    )
  )

# Add a matchday number for England's matches (sequential numbering of matches)
england_aggregate_scores <- england_aggregate_scores %>%
  mutate(matchday = seq_along(match_date))

# Filter matches where Italy is either the home or away team
italy_matches <- subset(data, hometeamname == "Italy" | awayteamname == "Italy")

# Add columns for:
# - Italy's goals
# - Opponent's goals
# - Aggregate score (Italy's goals - opponent's goals)
# - Point color (green for win, grey for draw, red for loss)
italy_matches <- italy_matches %>%
  mutate(
    italy_goals = ifelse(hometeamname == "Italy", scorehome, scoreaway),  # Goals scored by Italy
    opponent_goals_italy = ifelse(hometeamname == "Italy", scoreaway, scorehome), # Goals scored by the opponent
    aggregate_score_italy = italy_goals - opponent_goals_italy,  # Goal difference (aggregate score)
    point_color_italy = case_when(
      aggregate_score_italy > 0 ~ "green",  # Green for wins
      aggregate_score_italy == 0 ~ "grey",  # Grey for draws
      TRUE ~ "red"  # Red for losses
    )
  )

# Convert 'dateandtimecet' to a Date object for Italy's matches
italy_matches <- italy_matches %>%
  mutate(match_date_italy = as.Date(dateandtimecet))

# Aggregate Italy's goal difference (aggregate score) by match date
italy_aggregate_scores <- italy_matches %>%
  group_by(match_date_italy) %>%  # Group data by match date
  summarise(
    aggregate_score_italy = sum(aggregate_score_italy),  # Sum of aggregate scores per match date
    final_score_italy = paste(italy_goals, ":", opponent_goals_italy),  # Combine scores into "Italy's Goals:Opponent's Goals"
    point_color_italy = case_when(
      aggregate_score_italy > 0 ~ "green",  # Green for wins
      aggregate_score_italy == 0 ~ "grey",  # Grey for draws
      TRUE ~ "red"  # Red for losses
    )
  )

# Add a matchday number for Italy's matches (sequential numbering of matches)
italy_aggregate_scores <- italy_aggregate_scores %>%
  mutate(matchday_italy = seq_along(match_date_italy))

# Rename Italy's columns to match England's column names for consistency
italy_aggregate_scores <- italy_aggregate_scores %>%
  rename(
    match_date = match_date_italy,
    aggregate_score = aggregate_score_italy,
    final_score = final_score_italy,
    point_color = point_color_italy,
    matchday = matchday_italy
  )

# Add a new column to differentiate between England and Italy
england_aggregate_scores <- england_aggregate_scores %>%
  mutate(team = "England")

italy_aggregate_scores <- italy_aggregate_scores %>%
  mutate(team = "Italy")

# Combine both teams' data into a single dataset
combined_data <- rbind(england_aggregate_scores, italy_aggregate_scores)

# Add the new dashed line values (matchday and corresponding values)
dashed_line_values <- data.frame(
  matchday = 1:7,  # Matchday numbers
  dashed_score = c(1.5, 1.25, 1.7, 1.9, 0.75, 0.5, 0)  # The provided dashed line values
)

# Merge the dashed line values with the combined data
combined_data_with_dashed_line <- merge(combined_data, dashed_line_values, by = "matchday", all.x = TRUE)

# Create a time-series plot with matchday numbers on the x-axis
ggplot(combined_data_with_dashed_line, aes(x = matchday, y = aggregate_score, color = team)) + 
  geom_line(size = 1) +  # Plot lines for England and Italy
  geom_point(size = 3) +  # Add points for each matchday
  geom_text(aes(label = final_score), vjust = -0.5, hjust = 0.5, color = "black") +  # Label points with the final score
  scale_color_manual(values = c("England" = "blue", "Italy" = "red")) +  # Blue for England, red for Italy
  geom_line(aes(y = dashed_score, color = "Average Aggregate Score"), linetype = "dashed", size = 1) +  # Add a dashed line
  scale_color_manual(values = c("England" = "blue", "Italy" = "red", "Average Aggregate Score" = "purple")) + # Purple for dashed line
  ggtitle("England and Italy's Aggregate Score per Matchday with Dashed Line") +  # Plot title
  xlab("Matchday") +  # Label for x-axis
  ylab("Aggregate Score ") +  # Label for y-axis
  theme_minimal() +  # Use a clean minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    legend.title = element_blank(),  # Remove the legend title
    legend.position = "right"  # Place the legend on the right
  )






#TEST 2 







# Read the dataset 
data <- read.csv("C:/Users/ethan/Downloads/cleaned_data/cleaned_match_info.csv")

# Filter the data for matches involving England
england_data <- data %>%
  filter(hometeamname == "England" | awayteamname == "England")  # Filter for matches where England is the home or away team

# Filter the data for matches involving Spain
spain_data <- data %>%
  filter(hometeamname == "Italy" | awayteamname == "Italy")  # Filter for matches where Spain is the home or away team

# Calculate the average injury time for each match week for England
england_avg_injurytime <- england_data %>%
  group_by(matchday) %>%  # Group data by match week (match day)
  summarize(
    avg_injurytime = mean(injurytime, na.rm = TRUE),  # Calculate the mean injury time, ignoring missing values
    team = "England"  # Add a column indicating the team
  )

# Calculate the average injury time for each match week for Spain
spain_avg_injurytime <- spain_data %>%
  group_by(matchday) %>%  # Group data by match week (match day)
  summarize(
    avg_injurytime = mean(injurytime, na.rm = TRUE),  # Calculate the mean injury time, ignoring missing values
    team = "Italy"  # Add a column indicating the team
  )

# Calculate the average injury time for each match week across all teams
all_teams_avg_injurytime <- data %>%
  group_by(matchday) %>%  # Group data by match week (match day)
  summarize(
    avg_injurytime = mean(injurytime, na.rm = TRUE),  # Calculate the mean injury time, ignoring missing values
    team = "All Teams"  # Add a column indicating that this is for all teams
  )

# Combine the data for England, Spain, and All Teams into one data set
combined_avg_injurytime <- bind_rows(england_avg_injurytime, spain_avg_injurytime, all_teams_avg_injurytime)

# Create a bar plot comparing the average injury time for England, Spain, and All Teams per matchweek
ggplot(combined_avg_injurytime, aes(x = matchday, y = avg_injurytime, fill = team)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Create a dodged bar plot with black borders
  labs(
    title = "Average Injury Time vs Match Week for England, Italy, and All Teams",  # Add a plot title
    x = "Match Week",  # Label for the x-axis
    y = "Average Injury Time (minutes)"  # Label for the y-axis
  ) +
  scale_fill_manual(values = c("England" = "blue", "Italy" = "red", "All Teams" = "green")) +  # Custom colors for the teams
  theme_minimal() +  # Use a minimal theme for clean aesthetics
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )




# TEST 2 Regression








data <- read.csv("C:/Users/ethan/Downloads/cleaned_data/cleaned_match_info.csv")



# Filter the data for England's matches (both home and away)
england_data <- data %>%
  filter(hometeamname == "England" | awayteamname == "England")

# Selection of dependent and independent variables (Injury time, wind speed, humidity, temperature, matchminute)
england_regression_data <- england_data %>%
  select(injurytime, windspeed, humidity, temperature, matchminute)

# Fit a multiple linear regression model for England's matches
england_model <- lm(injurytime ~ windspeed + humidity + temperature + matchminute, data = england_regression_data)

# Display the summary of the model to check the significance
summary(england_model)






# TEST 3 - CLUSTERING 









library(dplyr)  
library(ggplot2) 
library(cluster) 


df <- read.csv("C:/Users/ethan/Downloads/england_with_assists_total_attempts_passes_tackles.csv")

# Display the data to ensure it loaded correctly
View(df)

# Step 3: Data Cleaning and Summing Duplicate Rows for the Same Player
# Group by 'Official Surname' and retain rows with the maximum goals scored for each player
df_clean <- df %>%
  group_by(OfficialSurname) %>%
  # Filter to keep only the row where the player scored the maximum goals
  filter(GoalScored == max(GoalScored, na.rm = TRUE)) %>%
  summarise(
    GoalScored = max(GoalScored, na.rm = TRUE),  # Keep the maximum goal scored
    PassesCompleted = first(PassesCompleted),  # Retain corresponding passes from the same row
    TacklesWon = first(TacklesWon),  # Retain corresponding tackles from the same row
    Role = first(Role),  # Retain the first role value (assuming it corresponds to the filtered row)
    .groups = "drop"  # Drop the grouping structure to simplify further operations
  ) %>%
  ungroup()  # Ensure the data is no longer grouped

# View the cleaned data
View(df_clean)

# Step 4: Handle Missing Values
# Replace NA values with 0 for all columns in the cleaned data
df_clean <- df_clean %>%
  mutate_all(~ifelse(is.na(.), 0, .))  # Applies to all columns in the data frame

# Step 5: K-means Clustering
# Perform clustering based on numerical features (excluding 'Official Surname')
# Clustering is done on Goal Scored, Passes Completed, and Tackles Won
kmeans_result <- kmeans(df_clean[, -1], centers = 3)  # Excludes the first column (OfficialSurname) for clustering

# Step 6: Add the cluster assignments back to the data frame
df_clean$Cluster <- kmeans_result$cluster

# Display the updated data with cluster assignments
View(df_clean)

# Step 7: Visualize the Clusters
# Create a scatter plot of Goal Scored vs. Passes Completed, colored by cluster
ggplot(df_clean, aes(x = GoalScored, y = PassesCompleted, color = as.factor(Cluster), label = OfficialSurname)) +
  geom_point(size = 3) +  # Add points to represent players
  geom_text(aes(label = OfficialSurname), vjust = 1, hjust = 1, size = 3, check_overlap = TRUE) +  # Add player names
  labs(
    title = "Clustering Results based on Goals, Passes, and Tackles",  # Plot title
    x = "Goals Scored",  # X-axis label
    y = "Passes Completed",  # Y-axis label
    color = "Cluster"  # Legend title for clusters
  ) +
  theme_minimal()  

# Step 8: Analyze Cluster Composition
# Check the count of players in each cluster
table(df_clean$Cluster)









# CREATING NEW DATASET









library(dplyr)  

# Load the data sets
better_dataset <- read.csv("C:/Users/ethan/Downloads/assist_data.csv")  
match_stats <- read.csv("C:/Users/ethan/Downloads/cervus-uefa-euro-2020/Match player statistics.csv")  

# Clean up the better data set to include only England players with goals scored
better_dataset <- better_dataset %>%
  select(Country, ID, OfficialName, OfficialSurname, GoalScored, Role) %>%  # Select relevant columns
  filter(Country == "England") %>%  # Filter to include only England players
  filter(!is.na(GoalScored))  # Remove rows where Goal Scored is NA

# Ensure the Value column in match_stats is numeric
match_stats$Value <- as.numeric(as.character(match_stats$Value))  # Convert the Value column to numeric

# Summarize match statistics to get total assists per player
assist_summary <- match_stats %>%
  filter(StatsName == "Assists") %>%  # Filter for assist statistics
  group_by(PlayerID) %>%  # Group by Player Id to summarize assists
  summarise(TotalAssists = sum(Value, na.rm = TRUE), .groups = "drop")  # Sum assists, ignoring NA values

# Summarize match statistics to get total attempts per player
total_attempts_summary <- match_stats %>%
  filter(StatsName == "Total Attempts") %>%  # Filter for total attempts
  group_by(PlayerID) %>%  # Group by Player Id
  summarise(TotalAttempts = sum(Value, na.rm = TRUE), .groups = "drop")  # Sum total attempts, ignoring NA values

# Summarize match statistics to get passes completed per player
passes_completed_summary <- match_stats %>%
  filter(StatsName == "Passes completed") %>%  # Filter for passes completed
  group_by(PlayerID) %>%  # Group by PlayerID
  summarise(PassesCompleted = sum(Value, na.rm = TRUE), .groups = "drop")  # Sum passes completed, ignoring NA values

# Summarize match statistics to get tackles won per player
tackles_won_summary <- match_stats %>%
  filter(StatsName == "Tackles won") %>%  # Filter for tackles won
  group_by(PlayerID) %>%  # Group by PlayerID
  summarise(TacklesWon = sum(Value, na.rm = TRUE), .groups = "drop")  # Sum tackles won, ignoring NA values

# Summarize match statistics to get tackles lost per player
tackles_lost_summary <- match_stats %>%
  filter(StatsName == "Tackles lost") %>%  # Filter for tackles lost
  group_by(PlayerID) %>%  # Group by PlayerID
  summarise(TacklesLost = sum(Value, na.rm = TRUE), .groups = "drop")  # Sum tackles lost, ignoring NA values

# Summarize match statistics to get attempts on target per player
attempts_on_target_summary <- match_stats %>%
  filter(StatsName == "Attempts on target") %>%  # Filter for attempts on target
  group_by(PlayerID) %>%  # Group by Player Id
  summarise(Attemptsontarget = sum(Value, na.rm = TRUE), .groups = "drop")  # Sum attempts on target, ignoring NA values

# Summarize match statistics to get total played time per player
played_time_summary <- match_stats %>%
  filter(StatsName == "Played time") %>%  # Filter for played time
  group_by(PlayerID) %>%  # Group by Player Id
  summarise(PlayedTime = sum(Value, na.rm = TRUE), .groups = "drop")  # Sum played time, ignoring NA values

# Merge the summarized data with the England players data set
merged_data <- left_join(better_dataset, assist_summary, by = c("ID" = "PlayerID")) %>%  # Merge assist summary
  left_join(total_attempts_summary, by = c("ID" = "PlayerID")) %>%  # Merge total attempts summary
  left_join(passes_completed_summary, by = c("ID" = "PlayerID")) %>%  # Merge passes completed summary
  left_join(tackles_won_summary, by = c("ID" = "PlayerID")) %>%  # Merge tackles won summary
  left_join(tackles_lost_summary, by = c("ID" = "PlayerID")) %>%  # Merge tackles lost summary
  left_join(attempts_on_target_summary, by = c("ID" = "PlayerID")) %>%  # Merge attempts on target summary
  left_join(played_time_summary, by = c("ID" = "PlayerID"))  # Merge played time summary

# Step 1: Remove duplicates by summing values for the same player
merged_data_clean <- merged_data %>%
  group_by(ID, OfficialSurname, OfficialName) %>%  # Group by player ID and name
  summarise(
    GoalScored = sum(GoalScored, na.rm = TRUE),  # Sum goals scored
    TotalAssists = sum(TotalAssists, na.rm = TRUE),  # Sum assists
    TotalAttempts = sum(TotalAttempts, na.rm = TRUE),  # Sum total attempts
    PassesCompleted = sum(PassesCompleted, na.rm = TRUE),  # Sum passes completed
    TacklesWon = sum(TacklesWon, na.rm = TRUE),  # Sum tackles won
    TacklesLost = sum(TacklesLost, na.rm = TRUE),  # Sum tackles lost
    Attemptsontarget = sum(TacklesWon, na.rm = TRUE),  # Sum attempts on target (potential error here)
    PlayedTime = sum(TacklesLost, na.rm = TRUE),  # Sum played time (potential error here)
    .groups = "drop"  # Ungroup after summarization
  )

# View the cleaned merged data set to check for duplicates and aggregated values
View(merged_data_clean)

# Save the cleaned merged data to a new CSV file
write.csv(merged_data_clean, "C:/Users/ethan/OneDrive/Intro to Data Science/Creating new dataset/england_with_cleaned_stats.csv", row.names = FALSE)

# Optionally, print the cleaned data to the console
print(merged_data_clean)







# TEST 5 - Regression








if (!require(caTools)) install.packages("caTools", dependencies = TRUE)
library(caTools)  


if (!require(Metrics)) install.packages("Metrics", dependencies = TRUE)
library(Metrics)  


england_data <- read.csv("C:/Users/ethan/OneDrive/Intro to Data Science/Creating new dataset/england_with_cleaned_stats - Copy.csv", header = TRUE)


# Convert 'Official Surname' and 'Official Name' to factors (categorical variables)
england_data$OfficialSurname <- as.factor(england_data$OfficialSurname)
england_data$OfficialName <- as.factor(england_data$OfficialName)


# Splits the data set into training (70%) and testing (30%) sets

set.seed(42)  # For reproducibility
split <- sample.split(england_data$GoalScored, SplitRatio = 0.7)  # Split based on the 'Goal Scored' column
train <- subset(england_data, split == TRUE)  # Training data (70% of the total data)
test <- subset(england_data, split == FALSE)  # Testing data (30% of the total data)


# Train a linear regression model to predict 'Goal Scored' using the features: 
# 'Total Attempts', 'Attemptsontarget', and 'Played Time'
model <- lm(GoalScored ~ TotalAttempts + Attemptsontarget + PlayedTime, data = train)

# Display the summary of the model to check the coefficients and performance statistics
summary(model)


# Use the trained model to make predictions on the test data
predictions <- predict(model, newdata = test)  # Predict Goal Scored using test data

# Step 6: Evaluate the Model
# Calculate the Mean Squared Error (MSE) between the actual and predicted Goal Scored values
mse_value <- mse(test$GoalScored, predictions)  # 'mse' function from the 'Metrics' package
print(paste("Mean Squared Error (MSE):", mse_value))  # Print the MSE value to assess model accuracy

# Extract the residuals from the model (difference between actual and predicted values)
residuals_model <- residuals(model)

# Plot a histogram of the residuals to check the distribution and whether they are randomly distributed
hist(residuals_model, 
     main = "Histogram of Residuals",  # Title of the histogram
     xlab = "Residuals",  # Label for x-axis
     col = "lightblue",  # Color of the bars
     border = "black",  # Border color of the bars
     breaks = 10)  









#TEST 6 CLEAN SHEETS








library(dplyr)   
library(ggplot2) 


dataset <- read.csv("C:/Users/ethan/Downloads/cervus-uefa-euro-2020/Pre-match information.csv")

# Filter goalkeepers and select relevant columns (Jersey Name, Clean Sheet, Role)
goalkeepers <- dataset %>%
  filter(Role == "Goalkeeper") %>%
  select(JerseyName, CleanSheet, Role)

# Clean data by removing duplicates and keeping the maximum Clean Sheet value for each goalkeeper
goalkeepers_clean <- goalkeepers %>%
  group_by(JerseyName) %>%
  summarise(CleanSheet = max(CleanSheet), Role = first(Role), .groups = "drop")

# Identify the top 5 goalkeepers with the most clean sheets
top_5_goalkeepers <- goalkeepers_clean %>%
  arrange(desc(CleanSheet)) %>%
  slice_head(n = 5)

# Create a bar plot of the top 5 goalkeepers with the most clean sheets
ggplot(top_5_goalkeepers, aes(x = reorder(JerseyName, CleanSheet), y = CleanSheet, fill = JerseyName)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Goalkeepers with the Most Clean Sheets",
       x = "Goalkeeper",
       y = "Clean Sheets") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#69b3a2", "#404080", "#ff6347", "#ffcc00", "#8e44ad")) +
  coord_flip() 











