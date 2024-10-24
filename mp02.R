library(tidyverse)
get_imdb_file <- function(fname){
  BASE_URL <- "https://raw.githubusercontent.com/michaelweylandt/STA9750/main/miniprojects/mini02_preprocessed/"
  fname_ext <- paste0(fname, ".csv.zip")
  if(!file.exists(fname_ext)){
    FILE_URL <- paste0(BASE_URL, fname_ext)
    download.file(FILE_URL, 
                  destfile = fname_ext)
  }
  as.data.frame(readr::read_csv(fname_ext, lazy=FALSE))
}

NAME_BASICS      <- get_imdb_file("name_basics_small")
TITLE_BASICS     <- get_imdb_file("title_basics_small")
TITLE_EPISODES   <- get_imdb_file("title_episodes_small")
TITLE_RATINGS    <- get_imdb_file("title_ratings_small")
TITLE_CREW       <- get_imdb_file("title_crew_small")
TITLE_PRINCIPALS <- get_imdb_file("title_principals_small")


#ensuring characters are numerical
NAME_BASICS <- NAME_BASICS |>
  mutate(birthYear = as.numeric(birthYear),
         deathYear = as.numeric(deathYear))
#TASK 1 Correct the column types of the TITLE tables using a combination of 
#mutate and the coercion functions as.numeric and as.logical.
TITLE_BASICS <- TITLE_BASICS |>
  mutate(
    startYear = as.numeric(startYear),
    endYear = as.numeric(endYear),
    runtimeMinutes = as.numeric(runtimeMinutes)
  )
TITLE_EPISODES <- TITLE_EPISODES |>
  mutate(
    seasonNumber = as.numeric(seasonNumber),
    episodeNumber = as.numeric(episodeNumber)
  )
TITLE_PRINCIPALS <- TITLE_PRINCIPALS |>
  mutate(
    ordering = as.integer(ordering)
  )
glimpse(NAME_BASICS)
NAME_BASICS |> separate_longer_delim(knownForTitles, ",") |> slice_head(n=10)
# TASK 2 
# How many movies are in our data set? How many TV series? How many TV episodes?
TITLE_BASICS |>
  group_by(titleType) |>
  summarise(Count = n(), .groups = 'drop') |>
  filter(titleType %in% c("movie", "tvSeries", "tvEpisode")) |>
  print()
#2 Who is the oldest living person in our data set?
#Multiple names appear without a deathyear such as Orest Alikin, 
#Zohrabai Ambalawali, Nemesio Antúnez, Mario Maffei , Mario Bertolazzi. 
#The oldest person without a deathdate is Robert De Visee who's birth year is 1655.
#More research led me to conclude that Elisabeth Waldo is the oldest CONFIRMED living person.

NAME_BASICS |>
  filter(is.na(deathYear)) |>
  arrange(birthYear) |>
  slice_head(n = 1) |>
  print()
#3 There is one TV Episode in this data set with a perfect 10/10 rating and at least 200,000 IMDb ratings.
# What is it? What series does it belong to?
# It is Breaking Bad with a 
perfect_episodes <- TITLE_RATINGS %>%
  filter(averageRating == 10, numVotes >= 200000) %>%
  inner_join(TITLE_EPISODES, by = "tconst")

# Then, join with TITLE_BASICS to find the series each episode belongs to
result <- perfect_episodes %>%
  inner_join(TITLE_BASICS, by = c("parentTconst" = "tconst")) %>%
  select(episode_tconst = "tconst", series_name = "primaryTitle", episode_name = "primaryTitle", rating = "averageRating", votes = "numVotes")

# Print the result
print(result)

#4 What four projects is the actor Mark Hamill most known for?
# Star Wars: Episode IV - A New Hope
#Star Wars: Episode VI - Return of the Jedi
#Star Wars: Episode VIII - The Last Jedi
mark_hamill_known_titles <- NAME_BASICS %>%
  filter(primaryName == "Mark Hamill") %>%
  pull(knownForTitles) %>%
  strsplit(split = ",") %>%
  unlist()

# Use the extracted title IDs to get the names of these titles from TITLE_BASICS
mark_hamill_projects <- TITLE_BASICS %>%
  filter(tconst %in% mark_hamill_known_titles) %>%
  select(primaryTitle)  # You can adjust this select statement to include more details if necessary

# Display the project names
print(mark_hamill_projects)

#5 What TV series, with more than 12 episodes, has the highest average rating?
#Suprisingly, it is Craft Games
tv_series <- TITLE_BASICS |>
  filter(titleType == "tvSeries")

# Get episode details from TITLE_EPISODES and count episodes per series
episode_counts <- TITLE_EPISODES |>
  inner_join(tv_series, by = c("parentTconst" = "tconst")) |>
  group_by(parentTconst) |>
  summarise(episodeCount = n(), .groups = "drop")

# Join episode counts with TITLE_RATINGS
series_ratings <- episode_counts |>
  filter(episodeCount > 12) |>
  inner_join(TITLE_RATINGS, by = c("parentTconst" = "tconst"))

# Find the highest rated series with more than 12 episodes
highest_rated_series <- series_ratings |>
  arrange(desc(averageRating)) |>
  slice_head(n = 1)

# Display the highest rated series
print(highest_rated_series)

#6
happy_days_tconst <- TITLE_BASICS |>
  filter(titleType == "tvSeries" & primaryTitle == "Happy Days") |>
  pull(tconst)

# Filter episodes from TITLE_EPISODES that belong to "Happy Days"
happy_days_episodes <- TITLE_EPISODES |>
  filter(parentTconst == happy_days_tconst)
happy_days_ratings <- happy_days_episodes |>
  inner_join(TITLE_RATINGS, by = "tconst")

# Calculate the average rating for each season
# Average ratings significantly dipped post season 6, 
# however it took almost three seasons to reach above an average rating of 7.0
average_ratings_by_season <- happy_days_ratings |>
  group_by(seasonNumber) |>
  summarise(AverageRating = mean(averageRating, na.rm = TRUE), .groups = "drop") |>
  arrange(seasonNumber) 
ggplot(average_ratings_by_season, aes(x = as.numeric(seasonNumber), y = AverageRating)) +
  geom_line(group = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Average Ratings of 'Happy Days' by Season",
       x = "Season Number",
       y = "Average Rating")

#Task2
#1 Create a success measure. 

TITLE_RATINGS <- TITLE_RATINGS %>%
  mutate(
    SuccessMetric = 0.7 * averageRating + 0.3 * log(numVotes + 1)
  )

# View the updated dataset with the new 'SuccessMetric' column
print(TITLE_RATINGS)

#2. Choose the top 5-10 movies on your metric and confirm that they were indeed box office successes.
#They were all indeed Boxoffice successes.
ratings_with_titles <- TITLE_RATINGS |>
  inner_join(TITLE_BASICS, by = "tconst") |>
  filter(titleType == "movie")
top_movies <- ratings_with_titles |>
  arrange(desc(SuccessMetric)) |>
  slice_head(n = 10) 
top_movies |>
  select(tconst, primaryTitle, SuccessMetric) |>
  print()
#2
#
#Choose 3-5 movies with large numbers of IMDb votes that score poorly on your success metric
#These are all low quality movies and box office flops
poor_scoring_movies <- ratings_with_titles |>
  filter(numVotes > 100000) %>%  # Adjust this threshold as needed
  arrange(SuccessMetric) |>
  slice_head(n = 5)  # Select the bottom 5 movies based on the success metric

# Display the relevant details of the selected movies
poor_scoring_movies %>%
  select(tconst, primaryTitle, averageRating, numVotes, SuccessMetric) %>%
  print()
#3
#Lets see if Christopher Nolan has successful movies
nolan_nconst <- NAME_BASICS %>%
  filter(primaryName == "Christopher Nolan") %>%
  pull(nconst)

# Get all titles where Nolan is listed as a director or writer
nolan_titles <- TITLE_CREW %>%
  filter(directors %in% nolan_nconst | writers %in% nolan_nconst) |>
  select(tconst)

# Join with TITLE_BASICS and TITLE_RATINGS to get the titles and their ratings
nolan_projects_with_ratings <- nolan_titles |>
  inner_join(TITLE_BASICS, by = "tconst") |>
  filter(titleType == "movie") |>  
  inner_join(TITLE_RATINGS, by = "tconst") |>
  arrange(desc(SuccessMetric))

# Display the top projects by success metric
top_nolan_projects <- nolan_projects_with_ratings |>
  filter(SuccessMetric > 7)  

# Print the results
print(top_nolan_projects)
#5. Success Rating High will be 7.

#Task 4
#What was the genre with the most “successes” in each decade?
TITLE_BASICS <- TITLE_BASICS |>
  mutate(startYear = as.numeric(startYear)) |>
  filter(startYear >= 1920, startYear <= 2020)

# Join datasets and calculate Decade
title_data <- TITLE_BASICS %>%
  inner_join(TITLE_RATINGS, by = "tconst") |>
  mutate(Decade = floor(startYear / 10) * 10)  # Creating a Decade column from startYear

# Filter for success and calculate counts by decade and genre
genre_successes_per_decade <- title_data |>
  filter(SuccessMetric > 7) |>
  separate_rows(genres, sep = ",") |>
  group_by(Decade, genres) |>
  summarise(SuccessCount = n(), .groups = "drop") |>
  arrange(Decade, desc(SuccessCount))

# Identify the top genre per decade
top_genre_per_decade <- genre_successes_per_decade |>
  group_by(Decade) %>%
  slice_max(order_by = SuccessCount, n = 1, with_ties = FALSE)

# Visualize the results
ggplot(top_genre_per_decade, aes(x = as.factor(Decade), y = SuccessCount, fill = genres)) +
  geom_col(show.legend = TRUE) +
  labs(title = "Top Genre by Decade Based on Success Metric (1920-2020)",
       x = "Decade",
       y = "Number of Successful Titles",
       fill = "Genre")

# Print the top genres per decade
print(top_genre_per_decade)

#genre change? A crime drama has been successful
genre_trends <- TITLE_BASICS %>%
  inner_join(TITLE_RATINGS, by = "tconst") |>
  mutate(Decade = floor(startYear / 10) * 10) |>
  filter(Decade >= 1980, Decade <= 2020) |>  # Focus on recent decades
  separate_rows(genres, sep = ",")  # Handle multiple genres per title

# Calculate the count of successful titles per genre per decade
genre_popularity <- genre_trends |>
  filter(SuccessMetric > 7) |>
  group_by(Decade, genres) |>
  summarise(SuccessCount = n(), .groups = "drop") |>
  arrange(genres, Decade)

# Visualize the trends in genre popularity
ggplot(genre_popularity, aes(x = as.factor(Decade), y = SuccessCount, group = genres, color = genres)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Trends in Genre Popularity (1980-2020)",
       x = "Decade",
       y = "Number of Successful Titles",
       color = "Genre")

# Print the results for inspection
print(genre_popularity)

#Task 5 Lets Identify our two leads and a director
current_year <- 2024

# Filter for crime movies in TITLE_BASICS
crime_movies <- TITLE_BASICS %>%
  filter(grepl("Crime", genres), titleType == "movie")

# Join with TITLE_PRINCIPALS to find actors
crime_movie_actors <- TITLE_PRINCIPALS |>
  filter(category %in% c("actor", "actress")) |>
  inner_join(crime_movies, by = "tconst")

# Join with NAME_BASICS to get actor details and filter for those alive
successful_crime_actors <- crime_movie_actors %>%
  inner_join(NAME_BASICS, by = "nconst") %>%
  filter((deathYear == "\\N" | is.na(deathYear)) & 
           (birthYear == as.character(current_year - 35) | 
              birthYear == as.character(current_year - 34)))  # Considering 25 or 26 years old for flexibility

# Join with TITLE_RATINGS to get success metrics
actor_success_metrics <- successful_crime_actors |>
  inner_join(TITLE_RATINGS, by = "tconst") |>
  group_by(nconst, primaryName) |>
  summarise(AverageRating = mean(averageRating), TotalVotes = sum(numVotes), .groups = "drop") %>%
  arrange(desc(AverageRating), desc(TotalVotes))

# Select top young actor based on ratings and votes
top_young_crime_actor <- actor_success_metrics |>
  slice_max(order_by = AverageRating, n = 1)

# For the overall top actors including one not age-restricted
top_crime_actors <- actor_success_metrics |>
  filter(nconst != top_young_crime_actor$nconst) |>
  slice_max(order_by = AverageRating, n = 1)

# Combine the results
final_top_actors <- bind_rows(top_young_crime_actor, top_crime_actors)

# Print the results
print(final_top_actors)
#this does not help Im going to pick who i liked
# Checking to see if taxi driver is good
taxi_driver_details <- TITLE_BASICS |>
  inner_join(TITLE_RATINGS, by = "tconst") |>
  filter(tconst == "tt0075314") |>
  select(tconst, primaryTitle, averageRating, numVotes, SuccessMetric) 

# Print the details for "Taxi Driver"
print(taxi_driver_details)