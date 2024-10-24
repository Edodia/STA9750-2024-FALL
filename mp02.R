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

glimpse(NAME_BASICS)
#ensuring characters are numerical
NAME_BASICS <- NAME_BASICS |>
  mutate(birthYear = as.numeric('birthYear'),
         deathYear = as.numeric('deathYear'))


