# loading necessary packages
library(tidyverse)
library(ggplot2)
library(scales)
library(corrplot)
library(readxl)
library(DT)
library(sf)
library(gt)

#Loading data! 
# First, ensure you have the necessary packages installed
# install.packages("readxl")

get_github_excel_all_sheets <- function(file_path) {
  
  # Construct the full GitHub URL to the raw version of the file
  base_url <- "https://raw.githubusercontent.com/"
  full_url <- paste0(base_url, file_path)
  
  # Temporarily download the file
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(full_url, temp_file, mode = "wb")  
  sheet_names <- excel_sheets(temp_file)
  
  sheets_data <- lapply(sheet_names, function(sheet) {
    read_excel(temp_file, sheet = sheet)
  })
  
  # Return the list of data frames
  return(sheets_data)
}

#Github paths

file_path <- "Edodia/STA9750-2024-FALL/main/public-district-attendance-results-2018-2023.xlsx"
file_path2 <- "Edodia/STA9750-2024-FALL/main/2023-graduation-rates-public-district.xlsx"
all_sheets_data <- get_github_excel_all_sheets(file_path)
all_sheets_data2 <- get_github_excel_all_sheets(file_path2)
Attendance_ALL <- all_sheets_data[[2]]
Attendance_SWD <- all_sheets_data[[3]]
Attendance_Ethnicity <- all_sheets_data[[4]]
Attendance_Gender <- all_sheets_data[[5]]
Attendance_Poverty <- all_sheets_data[[6]]
Attendance_ELL <- all_sheets_data[[7]]
Attendance_STH <- all_sheets_data[[8]]
Graduation_Rates_All <- all_sheets_data2[[2]]
Graduation_Rates_SWD <- all_sheets_data2[[5]]
Graduation_Rates_ELL <- all_sheets_data2[[3]]
Graduation_Rates_Ethnicity <- all_sheets_data2[[6]]
Graduation_Rates_Gender <- all_sheets_data2[[7]]
Graduation_Rates_Poverty <- all_sheets_data2[[8]]
#Specifiying data is within grades 12 for attendance to match graduation data
filter_high_school_grades <- function(data) {
  filtered_data <- data[data$`Grade` %in% c("12"), ]
  return(filtered_data)
}
Attendance_Poverty <- filter_high_school_grades(Attendance_Poverty)
Attendance_Ethnicity <- filter_high_school_grades(Attendance_Ethnicity)
Attendance_Gender <- filter_high_school_grades(Attendance_Gender)
Attendance_ELL <- filter_high_school_grades(Attendance_ELL)
Attendance_ALL <- filter_high_school_grades(Attendance_ALL)
Attendance_STH <- filter_high_school_grades(Attendance_STH)
Attendance_SWD <- filter_high_school_grades(Attendance_SWD)

#Attendance Analysis
# Identifying the top five worst and best districts in NYC for attendance
attendance_summary <- Attendance_ALL %>%
  group_by(District) %>%
  summarise(Average_Attendance = mean(`% Attendance`, na.rm = TRUE)) %>%
  arrange(desc(Average_Attendance))

# View the summary
print(attendance_summary)
top_districts <- attendance_summary %>%
  top_n(5, wt = Average_Attendance) %>%
  arrange(desc(Average_Attendance))

# Plot using ggplot2 with labels
ggplot(top_districts, aes(x = reorder(District, Average_Attendance), y = Average_Attendance, fill = as.factor(District))) +
  geom_col(show.legend = FALSE) +  
  geom_text(aes(label = sprintf("%.2f%%", Average_Attendance)), 
            position = position_stack(vjust = 1.05),  
            color = "black", size = 3.5) +  
  scale_fill_brewer(palette = "Paired") +  
  labs(title = "Top 5 Districts by Average Attendance", x = "District", y = "Average Attendance (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#What are the bottom 5
bottom_districts <- attendance_summary %>%
  arrange(Average_Attendance) %>%
  head(5)  

# Print to check
print(bottom_districts)
ggplot(bottom_districts, aes(x = reorder(District, Average_Attendance), y = Average_Attendance, fill = as.factor(District))) +
  geom_col(show.legend = FALSE) +  
  geom_text(aes(label = sprintf("%.2f%%", Average_Attendance)), 
            position = position_stack(vjust = 1.05),  
            color = "black", size = 3.5) +  
  scale_fill_brewer(palette = "Paired") + 
  labs(title = "Bottom 5 Districts by Average Attendance", x = "District", y = "Average Attendance (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
#Now ploting attendance rates using the nyc shapefiles
#Initial setup
if(!file.exists("nysd_24d.zip")){
  download.file("http://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/nysd_24d.zip", 
                destfile="nysd_24d.zip", method = "curl")
}


td <- tempdir(); 
zip_contents <- unzip("nysd_24d.zip", 
                      exdir = td)

fname_shp <- zip_contents[grepl("shp$", zip_contents)]
nyc_sf <- read_sf(fname_shp)

ggplot(nyc_sf, 
       aes(geometry=geometry, 
           fill = Shape_Area)) + 
  geom_sf()

colnames(nyc_sf)
nyc_sf <- nyc_sf %>%
  dplyr::rename(District = SchoolDist)
nyc_sf$District <- as.character(nyc_sf$District)
attendance_summary$District <- as.character(attendance_summary$District)
nyc_sf_attendance <- merge(nyc_sf, attendance_summary, by = "District", all.x = TRUE)
#now plot
ggplot(nyc_sf_attendance, aes(geometry = geometry, fill = Average_Attendance)) +
  geom_sf() +  
  geom_sf_text(aes(label = District), size = 3, colour = "white", check_overlap = TRUE) +  
  scale_fill_gradient(low = "red", high = "blue",  
                      name = "Average Attendance from 2018 to 2023", 
                      labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Map of NYC School Districts by Average Attendance", 
       subtitle = "District labels are shown on the map") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
#Now more statistics to understand attendance in NYC
#ELL 
# Separate attendance data for ELL and Not ELL for the test
attendance_ell <- Attendance_ELL$`% Attendance`[Attendance_ELL$Category == "ELL"]
attendance_not_ell <- Attendance_ELL$`% Attendance`[Attendance_ELL$Category == "Not ELL"]

# Perform a t-test
t_test_result <- t.test(attendance_ell, attendance_not_ell)
print(t_test_result)
#The Welch t-test results strongly suggest that non-ELL students attend school at a higher rate than ELL students.
ggplot(Attendance_ELL, aes(x = Category, y = `% Attendance`, fill = Category)) +
  geom_boxplot() +
  labs(title = "Attendance Rates by ELL Status",
       x = "Category",
       y = "Attendance Rate (%)") +
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal() +
  theme(legend.position = "none") 

#Gender/ Noticed that the rest of the attendance data may need similar treatment
Attendance_Gender <- na.omit(Attendance_Gender)

head(Attendance_Gender)
unique(Attendance_Gender$`% Attendance`)
Attendance_Gender$`% Attendance` <- gsub("s", "", Attendance_Gender$`% Attendance`)  # Remove 's'
Attendance_Gender$`% Attendance` <- as.numeric(Attendance_Gender$`% Attendance`)  
str(Attendance_Gender$`% Attendance`)
Attendance_Gender <- na.omit(Attendance_Gender)
summary(Attendance_Gender$`% Attendance`)
# Mean averages
average_attendance_gender <- Attendance_Gender %>%
  group_by(Category) %>%
  summarise(Average_Attendance = mean(`% Attendance`, na.rm = TRUE))

print(average_attendance_gender)
ggplot(average_attendance_gender, aes(x = Category, y = Average_Attendance, fill = Category)) +
  geom_bar(stat = "identity", width = 0.7) +  
  geom_text(aes(label = sprintf("%.2f%%", Average_Attendance)), 
            position = position_stack(vjust = 1.05),  
            color = "black", size = 3.5) +  
  scale_fill_brewer(palette = "Pastel1") +  
  labs(title = "Average Attendance Rates by Gender",
       x = "Gender Category",
       y = "Average Attendance Rate (%)") +
  theme_minimal() +  
  theme(legend.position = "none",  
        axis.text.x = element_text(angle = 45, hjust = 1))

#SWD attendance
Attendance_SWD <- na.omit(Attendance_SWD)
average_attendance_swd <- Attendance_SWD %>%
  group_by(Category) %>%
  summarise(Average_Attendance = mean(`% Attendance`, na.rm = TRUE))
print(average_attendance_swd)
swd_dt <- datatable(average_attendance_swd, options = list(pageLength = 5, autoWidth = TRUE), 
                    filter = 'top',  
                    rownames = FALSE)  
swd_dt

#STH
Attendance_STH <-na.omit(Attendance_STH)
average_attendance_sth <- Attendance_STH %>%
  group_by(Category) %>%
  summarise(Average_Attendance = mean(`% Attendance`, na.rm = TRUE))
print(average_attendance_sth)
sth_dt <- datatable(average_attendance_sth, options = list(pageLength = 5, autoWidth = TRUE), 
                    filter = 'top',  
                    rownames = FALSE)  
sth_dt
#Poverty
Attendance_Poverty <-na.omit(Attendance_Poverty)
average_attendance_pov <- Attendance_Poverty %>%
  group_by(Category) %>%
  summarise(Average_Attendance = mean(`% Attendance`, na.rm = TRUE))
print(average_attendance_sth)
pov_dt <- datatable(average_attendance_pov, options = list(pageLength = 5, autoWidth = TRUE), 
                    filter = 'top',  
                    rownames = FALSE)
                    
pov_dt

#Ethnicity
Attendance_Ethnicity <- na.omit(Attendance_Ethnicity)
Attendance_Ethnicity$`% Attendance` <- gsub("s", "", Attendance_Ethnicity$`% Attendance`)  
Attendance_Ethnicity$`% Attendance` <- as.numeric(Attendance_Ethnicity$`% Attendance`)  
average_attendance_ethnicity <- Attendance_Ethnicity  %>%
  group_by(Category) %>%
  summarise(Average_Attendance = mean(`% Attendance`, na.rm = TRUE))

attendance_eth_plot <- ggplot(average_attendance_ethnicity, aes(x = Category, y = Average_Attendance, fill = Category)) +
  geom_bar(stat = "identity", width = 0.7) +  
  geom_text(aes(label = sprintf("%.2f%%", Average_Attendance)), position = position_stack(vjust = 1.05),
            color = "black", size = 3.5) + 
  scale_fill_brewer(palette = "Paired") + 
  labs(title = "Average Attendance Rates by Ethnicity",
       x = "Ethnic Category",
       y = "Average Attendance Rate (%)") +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

attendance_eth_plot

#Now Graduation!!!!! Someone call Kanye West!
#Some data cleaning and modification
filter_by_cohort_year <- function(data) {
  # Define the cohort years to include
  valid_cohorts <- c("2014", "2015", "2016", "2017", "2018")
  
  # Filter the data to include only the specified cohorts
  filtered_data <- data[data$`Cohort Year` %in% valid_cohorts, ]
  
  return(filtered_data)
}
Graduation_Rates_All <- filter_by_cohort_year(Graduation_Rates_All)
Graduation_Rates_ELL <- filter_by_cohort_year(Graduation_Rates_ELL)
Graduation_Rates_Poverty <- filter_by_cohort_year(Graduation_Rates_Poverty)
Graduation_Rates_Gender <- filter_by_cohort_year(Graduation_Rates_Gender)
Graduation_Rates_SWD <- filter_by_cohort_year(Graduation_Rates_SWD)

#Now graduation top 5 and bottom 5 districts
graduation_summary <- Graduation_Rates_All %>%
  group_by(District) %>%
  summarise(Average_Graduation = mean(`% Grads`, na.rm = TRUE)) %>%
  arrange(desc(Average_Graduation))

# View the summary
print(graduation_summary)
top_districts <- graduation_summary  %>%
  top_n(5, wt = Average_Graduation) %>%
  arrange(desc(Average_Graduation))
nyc_sf_grads <- merge(nyc_sf, graduation_summary, by = "District", all.x = TRUE)
nyc_sf_grads$District <- as.character(nyc_sf_grads$District)

# Create the map with labels
graduation_map <- ggplot(nyc_sf_grads, aes(geometry = geometry, fill = Average_Graduation)) +
  geom_sf() +  # Draw the shapes
  geom_sf_text(aes(label = District), size = 3, colour = "white", check_overlap = TRUE, position = position_nudge(y = 0.02)) +  # Add district labels
  scale_fill_gradient(low = "red", high = "blue",  
                      name = "Average Graduation from 2018 through 2024",
                      labels = percent_format(accuracy = 1)) +
  labs(title = "Map of NYC School Districts by Average Graduation Rate",
       subtitle = "Graduation rates from 2018 through 2024") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
graduation_map

#top and bottom 5 grad rates per district
top_5_grad_districts <- head(graduation_summary,5)
bottom_5_grad_districts <- tail(graduation_summary,5)

top_5_grad_plot<- ggplot(top_5_grad_districts, aes(x = reorder(as.factor(District), Average_Graduation), y = Average_Graduation, fill = as.factor(District))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Average_Graduation)), vjust = -0.5, color = "black") +  
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Top 5 Districts by Average Graduation Rate",
       x = "District",
       y = "Average Graduation Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
top_5_grad_plot

bottom_5_grad_plot <- ggplot(bottom_5_grad_districts, aes(x = reorder(as.factor(District), -Average_Graduation), y = Average_Graduation, fill = as.factor(District))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Average_Graduation)), vjust = -0.5, color = "black") +  
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Bottom 5 Districts by Average Graduation Rate",
       x = "District",
       y = "Average Graduation Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
bottom_5_grad_plot

#Statistics for Each demographic identifier: Grad
#lets start with ethnicity
Graduation_Rates_Ethnicity <- na.omit(Graduation_Rates_Ethnicity)
Graduation_Rates_Ethnicity$`% Grads` <- gsub("s", "", Graduation_Rates_Ethnicity$`% Grads`)  
Graduation_Rates_Ethnicity$`% Grads` <- as.numeric(Graduation_Rates_Ethnicity$`% Grads`) 
average_grad_ethnicity <- Graduation_Rates_Ethnicity  %>%
  group_by(Category) %>%
  summarise(Average_Graduation = mean(`% Grads`, na.rm = TRUE))

grad_ethn_plot <- ggplot(average_grad_ethnicity, aes(x = Category, y = Average_Graduation, fill = Category)) +
  geom_bar(stat = "identity", width = 0.7) +  
  geom_text(aes(label = sprintf("%.2f%%", Average_Graduation)), position = position_stack(vjust = 1.05),
            color = "black", size = 3.5) +  
  scale_fill_brewer(palette = "Paired") +  
  labs(title = "Average Graduation Rates by Ethnicity",
       x = "Ethnic Category",
       y = "Average Graduation Rate (%)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grad_ethn_plot

#Pov
Graduation_Rates_Poverty <- na.omit(Graduation_Rates_Poverty)
unique(Graduation_Rates_Poverty$`% Grads`)
average_grad_pov <- Graduation_Rates_Poverty  %>%
  group_by(Category) %>%
  summarise(Average_Graduation = mean(`% Grads`, na.rm = TRUE))
average_grad_pov
grad_pov_dt <- datatable(average_grad_pov, options = list(pageLength = 5, autoWidth = TRUE), 
                            filter = 'top',  
                            rownames = FALSE) 
grad_pov_dt

#ELL
Graduation_Rates_ELL <-na.omit(Graduation_Rates_ELL)
unique(Graduation_Rates_ELL$`% Grads`)
average_grad_ell <- Graduation_Rates_ELL  %>%
  group_by(Category) %>%
  summarise(Average_Graduation = mean(`% Grads`, na.rm = TRUE))
average_grad_pov
grad_ell_dt <- datatable(average_grad_ell, options = list(pageLength = 5, autoWidth = TRUE), 
                         filter = 'top',  
                         rownames = FALSE) 
grad_ell_dt

#SWD
Graduation_Rates_SWD <-na.omit(Graduation_Rates_SWD)
unique(Graduation_Rates_SWD$`% Grads`)
average_grad_swd <- Graduation_Rates_SWD  %>%
  group_by(Category) %>%
  summarise(Average_Graduation = mean(`% Grads`, na.rm = TRUE))
average_grad_swd
grad_swd_dt <- datatable(average_grad_swd, options = list(pageLength = 5, autoWidth = TRUE), 
                         filter = 'top',  
                         rownames = FALSE)
grad_swd_dt

#Gender
Graduation_Rates_Gender <-na.omit(Graduation_Rates_Gender)
unique(Graduation_Rates_Gender$`% Grads`)
Graduation_Rates_Gender$`% Grads` <- gsub("s", "", Graduation_Rates_Gender$`% Grads`)  
Graduation_Rates_Gender$`% Grads` <- as.numeric(Graduation_Rates_Gender$`% Grads`) 
Graduation_Rates_Gender <- Graduation_Rates_Gender[!is.nan(Graduation_Rates_Gender$`% Grads`), ]

average_grad_gender <- Graduation_Rates_Gender  %>%
  group_by(Category) %>%
  summarise(Average_Graduation = mean(`% Grads`, na.rm = TRUE))
average_grad_gender
grad_gen_dt <- datatable(average_grad_gender, options = list(pageLength = 5, autoWidth = TRUE), 
                         filter = 'top',  
                         rownames = FALSE)
grad_gen_dt

#Answering the true question!
combined_data <- merge(Graduation_Rates_All, Attendance_ALL, by = "District")

# Calculate correlation between `% Grads` and `% Attendance`
correlation_result <- cor(combined_data$`% Grads`, combined_data$`% Attendance`, use = "complete.obs")
print(correlation_result)
#correlation of .608 thats realy strong
# Visualizing the correlation with a scatter plot
final_corr <- ggplot(combined_data, aes(x = `% Attendance`, y = `% Grads`)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlation between Attendance and Graduation Rates for Cohorts 2014-2018",
       x = "Attendance Rate (%)",
       y = "Graduation Rate (%)") +
  theme_minimal()
final_corr
84 - 57

