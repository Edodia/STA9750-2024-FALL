library(tidyverse)
library(dplyr)
library(ggplot2)
library(DT)
library(sf)
MIT_House_Data <- read.csv("~/Downloads/dataverse_files/1976-2022-house.csv")
Presidential_Vote_Count <- read.csv("~/Downloads/1976-2020-president.csv")
#geting congressional map
get_congress_map <- function(cong=113) {
  tmp_file <- tempfile()
  tmp_dir  <- tempdir()
  zp <- sprintf("http://cdmaps.polisci.ucla.edu/shp/districts%03i.zip",cong)
  download.file(zp, tmp_file)
  unzip(zipfile = tmp_file, exdir = tmp_dir)
  fpath <- paste(tmp_dir, sprintf("districtShapes/districts%03i.shp",cong), sep = "/")
  st_read(fpath)
}
cd114 <- get_congress_map(114)

census_sf <- c("https://www2.census.gov/geo/tiger/TIGER2022/CD/tl_2022_us_cd116.zip",
                 "https://www2.census.gov/geo/tiger/TIGER2021/CD/tl_2021_us_cd116.zip",
                 "https://www2.census.gov/geo/tiger/TIGER2020/CD/tl_2020_us_cd116.zip",
                 "https://www2.census.gov/geo/tiger/TIGER2019/CD/tl_2019_us_cd116.zip",
                 "https://www2.census.gov/geo/tiger/TIGER2018/CD/tl_2018_us_cd116.zip",
                 "https://www2.census.gov/geo/tiger/TIGER2017/CD/tl_2017_us_cd115.zip",
                 "https://www2.census.gov/geo/tiger/TIGER2016/CD/tl_2016_us_cd115.zip",
                 "https://www2.census.gov/geo/tiger/TIGER2015/CD/tl_2015_us_cd114.zip",
                 "https://www2.census.gov/geo/tiger/TIGER2014/CD/tl_2014_us_cd114.zip")

get_census_sf <- function(URLs) {
  for (i in URLs) {
    file_name <- basename(i)
    
    if(!file.exists(file_name)) {
      download.file(i, file_name, mode = "wb", quiet = TRUE)
    }
  }
}

get_census_sf(census_sf)
library(sf)

# Function to download, unzip, and read shapefiles
get_census_sf <- function(URLs) {
  shapefile_list <- list() # Initialize a list to store sf objects
  
  for (i in URLs) {
    file_name <- basename(i)
    dir_name <- gsub(".zip", "", file_name) # Directory name for unzipped files
    
    # Download the file if it doesn't exist
    if(!file.exists(file_name)) {
      download.file(i, file_name, mode = "wb", quiet = TRUE)
    }
    
    # Unzip the file if the directory doesn't exist
    if(!dir.exists(dir_name)) {
      unzip(file_name, exdir = dir_name)
    }
    
    # Assume shapefile has the same base name as the zip file
    shp_files <- list.files(dir_name, pattern = "\\.shp$", full.names = TRUE)
    
    # Read the shapefile if it exists
    if(length(shp_files) > 0) {
      sf_object <- st_read(shp_files[1], quiet = TRUE)
      shapefile_list[[dir_name]] <- sf_object
    }
  }
  
  return(shapefile_list)
}

# URLs for the shapefiles
census_sf <- c(
  "https://www2.census.gov/geo/tiger/TIGER2022/CD/tl_2022_us_cd116.zip",
  "https://www2.census.gov/geo/tiger/TIGER2021/CD/tl_2021_us_cd116.zip",
  "https://www2.census.gov/geo/tiger/TIGER2020/CD/tl_2020_us_cd116.zip",
  "https://www2.census.gov/geo/tiger/TIGER2019/CD/tl_2019_us_cd116.zip",
  "https://www2.census.gov/geo/tiger/TIGER2018/CD/tl_2018_us_cd116.zip",
  "https://www2.census.gov/geo/tiger/TIGER2017/CD/tl_2017_us_cd115.zip",
  "https://www2.census.gov/geo/tiger/TIGER2016/CD/tl_2016_us_cd115.zip",
  "https://www2.census.gov/geo/tiger/TIGER2015/CD/tl_2015_us_cd114.zip",
  "https://www2.census.gov/geo/tiger/TIGER2014/CD/tl_2014_us_cd114.zip"
)

# Get shapefiles
shapefiles <- get_census_sf(census_sf)

#Task3
seats_1976 <- MIT_House_Data %>% filter(year == 1976) %>% group_by(state) %>% summarise(Seats_1976 = n_distinct(district))
seats_2022 <- MIT_House_Data %>% filter(year == 2022) %>% group_by(state) %>% summarise(Seats_2022 = n_distinct(district))

# Combine the data and calculate net change
seat_changes <- left_join(seats_1976, seats_2022, by = "state")
seat_changes <- seat_changes %>% mutate(Net_Change = Seats_2022 - Seats_1976)

# Order data for plotting
seat_changes <- seat_changes %>% arrange(desc(Net_Change))

# Plotting the results
ggplot(seat_changes, aes(x = reorder(state, Net_Change), y = Net_Change, fill = Net_Change > 0)) +
  geom_col() +
  coord_flip() +  # Flip the coordinates to make it easier to read the state names
  labs(title = "Net Change in U.S. House Seats from 1976 to 2022",
       x = "State",
       y = "Net Change in Seats") +
  scale_fill_manual(name = "Change Type", values = c("red", "blue"), labels = c("Loss", "Gain")) +
  theme_minimal()

#2

election_results <- MIT_House_Data %>%
  group_by(year, state, district, candidate) %>%
  summarise(Total_Votes = sum(candidatevotes),
            Major_Party_Votes = sum(candidatevotes[party %in% c("DEMOCRAT", "REPUBLICAN")]),
            .groups = 'drop')

# Determine winners based on total votes and major party votes
winners_by_total <- election_results %>%
  group_by(year, state, district) %>%
  top_n(1, Total_Votes) %>%
  ungroup() %>%
  select(year, state, district, candidate, Total_Votes)

winners_by_major_party <- election_results %>%
  group_by(year, state, district) %>%
  top_n(1, Major_Party_Votes) %>%
  ungroup() %>%
  select(year, state, district, candidate, Major_Party_Votes)

# Combine the results to find discrepancies
discrepancy_analysis <- winners_by_total %>%
  left_join(winners_by_major_party, by = c("year", "state", "district"), suffix = c("_total", "_major")) %>%
  filter(candidate_total != candidate_major) %>%
  select(year, state, district, candidate_total, Total_Votes, candidate_major, Major_Party_Votes)

# Render the results as a datatable
datatable(discrepancy_analysis, options = list(pageLength = 10), 
          caption = "Discrepancies in Election Outcomes Based on Voting Systems")
#3
library(dplyr)
library(readr)


# Prepare Presidential data: filter, group, and summarize
presidential_grouped <- Presidential_Vote_Count %>%
  filter(party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  group_by(year, state, party = party_simplified) %>%
  summarise(total_party_votes_president = sum(candidatevotes), .groups = 'drop')

# Prepare House data: filter, group, and summarize
house_grouped <- MIT_House_Data %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  group_by(year, state, party) %>%
  summarise(total_party_votes_house = sum(candidatevotes), .groups = 'drop')
merged_data <- left_join(presidential_grouped, house_grouped, by = c("year", "state", "party"))

# Calculate the difference in votes
merged_data <- merged_data %>%
  mutate(vote_difference = total_party_votes_president - total_party_votes_house)
ggplot(merged_data, aes(x = year, y = vote_difference, color = party, group = party)) +
  geom_line() +
  geom_point() +
  labs(title = "Vote Difference Over Time by Party",
       x = "Year",
       y = "Vote Difference (Presidential - Congressional)") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")  
recent_year_data <- merged_data %>%
  filter(year == 2020)

ggplot(recent_year_data, aes(x = reorder(state, vote_difference), y = vote_difference, fill = party)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  labs(title = "State-by-State Vote Difference in 2020",
       x = "State",
       y = "Vote Difference (Presidential - Congressional)") +
  theme_minimal()
ggplot(merged_data, aes(x = party, y = vote_difference, fill = party)) +
  geom_boxplot() +
  labs(title = "Comparison of Vote Differences Across Parties",
       x = "Party",
       y = "Vote Difference (Presidential - Congressional)") +
  theme_minimal()
library(sf)
# Define the function to read a shapefile from a zip archive
read_shp_from_zip <- function(zip_filename) {
  # Create a temporary directory to extract files
  temp_dir <- tempdir()
  
  # Extract the contents of the zip file to the temporary directory
  zip_contents <- unzip(zip_filename, exdir = temp_dir)
  
  # Find the shapefile (.shp) among the extracted files
  fname_shp <- zip_contents[grepl("\\.shp$", zip_contents)]
  
  # Read the shapefile using read_sf and return the resulting object
  if (length(fname_shp) > 0) {
    nyc_sf <- read_sf(fname_shp)
    return(nyc_sf)
  } else {
    stop("No shapefile found in the zip archive.")
  }
}

# Example usage:
# Assuming you've downloaded and named your file "nyc_borough_boundaries.zip"
if (!file.exists("nyc_borough_boundaries.zip")) {
  download.file("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Shapefile", 
                destfile="nyc_borough_boundaries.zip")
}

# Read the shapefile from the zip
nyc_sf <- read_shp_from_zip("nyc_borough_boundaries.zip")
ggplot(nyc_sf, 
       aes(geometry=geometry)) + 
  geom_sf()
ggplot(nyc_sf, 
       aes(geometry=geometry, 
           fill = shape_area)) + 
  geom_sf()
#
election_2000 <- Presidential_Vote_Count %>%
  filter(year == 2000, party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  group_by(state) %>%
  summarise(winner = candidate[which.max(candidatevotes)],
            party = party_simplified[which.max(candidatevotes)],
            .groups = 'drop')
read_shp_from_zip <- function(zip_filename) {
  temp_dir <- tempdir()
  zip_contents <- unzip(zip_filename, exdir = temp_dir)
  fname_shp <- zip_contents[grepl("\\.shp$", zip_contents)]
  if (length(fname_shp) > 0) {
    return(read_sf(fname_shp))
  } else {
    stop("No shapefile found in the zip archive.")
  }
}
cd106 <- get_congress_map(106)
cd106 <- cd106 %>%
  rename(state = `STATENAME`)
cd106$state <- toupper(cd106$state)
election_2000$state <- toupper(election_2000$state)
unique(cd106$state)
unique(election_2000$state)
election_map_data <- merge(cd106, election_2000, by = "state")

# Check the results of the merge to ensure it's populated correctly
ggplot(data = election_map_data) +
  geom_sf(aes(fill = party), lwd = 0.1) +
  scale_fill_manual(values = c("DEMOCRAT" = "blue", "REPUBLICAN" = "red")) +
  labs(title = "2000 Presidential Election Results by Congressional District",
       subtitle = "Colors represent the winning party by state",
       fill = "Winning Party") +
  coord_sf(xlim = c(-130, -65), ylim = c(25, 50), expand = FALSE) +  # Adjust these limits as needed
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
#Task 7
total_ec_votes <- function(state) {
  state_ecvs <- c(AL = 9, AK = 3, AZ = 8, AR = 6, CA = 54, CO = 8, CT = 8, DE = 3,
                  DC = 3, FL = 25, GA = 13, HI = 4, ID = 4, IL = 22, IN = 12, IA = 7,
                  KS = 6, KY = 8, LA = 9, ME = 4, MD = 10, MA = 12, MI = 18, MN = 10,
                  MS = 7, MO = 11, MT = 3, NE = 5, NV = 4, NH = 4, NJ = 15, NM = 5,
                  NY = 33, NC = 14, ND = 3, OH = 21, OK = 8, OR = 7, PA = 23, RI = 4,
                  SC = 8, SD = 3, TN = 11, TX = 32, UT = 5, VT = 3, VA = 13, WA = 11,
                  WV = 5, WI = 11, WY = 3)
  return(state_ecvs[state])
}

# State-Wide Winner-Take-All
state_wide_winner_take_all <- function(data) {
  data %>%
    group_by(state) %>%
    summarise(winner = candidate[which.max(candidatevotes)],
              ec_votes = total_ec_votes(state)) %>%
    ungroup()
}

# State-Wide Proportional
state_wide_proportional <- function(data) {
  data %>%
    group_by(state, candidate) %>%
    summarise(proportional_ec_votes = round(total_ec_votes(state) * (candidatevotes / sum(candidatevotes))),
              .groups = 'drop') %>%
    ungroup()
}

# National Proportional
national_proportional <- function(data) {
  total_votes <- sum(data$candidatevotes)
  total_ec_votes <- sum(total_ec_votes(data$state))
  data %>%
    group_by(candidate) %>%
    summarise(proportional_national_ec_votes = round(total_ec_votes * (sum(candidatevotes) / total_votes)),
              .groups = 'drop') %>%
    ungroup()
}
data_2000 <- Presidential_Vote_Count %>% 
  filter(year == 2000)

# Calculate electoral votes under different schemes
results_swwta <- state_wide_winner_take_all(data_2000)
results_swp <- state_wide_proportional(data_2000)
results_np <- national_proportional(data_2000)

# Combine results for comparison
combined_results <- bind_rows(
  results_swwta %>% mutate(method = "State-Wide Winner-Take-All"),
  results_swp %>% mutate(method = "State-Wide Proportional"),
  results_np %>% mutate(method = "National Proportional")
)
summary(combined_results$proportional_national_ec_votes)

# Filter out small third-party candidates to focus on main candidates
combined_results <- combined_results %>%
  filter(candidate %in% c("Bush", "Gore"))

# Recreate the plot with potential adjustments
ggplot(combined_results, aes(x = candidate, y = proportional_national_ec_votes, fill = method)) +
  geom_col(position = position_dodge(), width = 0.7) +
  labs(title = "2000 Presidential Election Analysis: Electoral Vote Allocations",
       x = "Candidate",
       y = "Electoral Votes",
       fill = "Method") +
  scale_y_continuous(labels = scales::comma) +  # Ensure y-axis is properly scaled
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve text alignment if needed
