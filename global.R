# @author: Nataliia Romanenko

library(ggplot2)
library(tidyr) 
library(readr)
library(dplyr)
library(jsonlite)
library(stringr)
library(shinythemes)

# dataset URL: https://www.kaggle.com/rounakbanik/the-movies-dataset
credits <- read_csv("credits.csv")
metadata <- read_csv("movies_metadata.csv")

## create dataframe for main analysis and movie retrival
movies <- data.frame(metadata)
# select columns to merge with credits data
movies <- movies[, c(3,4,6,10:13,15:17,21,23,24)] %>% merge(credits, by = "id")

# replace meaningless data with NAs
movies$vote_average[movies$vote_average < 0.01] <- NA
movies$runtime[movies$runtime == 0] <- NA
movies$budget[movies$budget < 1000] <- NA
movies$revenue[movies$revenue < 1000] <- NA

# get genres from JSON string
movies$genres <- gsub("'", '"', movies$genres) # replace ' with " to use fromJSON
genre.df <- movies %>% filter(nchar(genres)>2) %>% mutate(genre=lapply(genres,fromJSON)) %>% 
  unnest(genre) %>% select(id,genre=name) 

## create data frame for genre specific analysis
# merge genre.df with movies and filter out NAs in genres
movies.full <- left_join(movies, genre.df, by = "id") %>% filter(!is.na(genre))
movies.full$release_date <- format(movies.full$release_date, "%Y")
movies.full <- movies.full[, c(1,2,8:12,16)] # keep colums needed for analysis

# create vector of genre names for dropdown menu
all.genres <- names(table(movies.full$genre))
# create vector with variable names for dropdown menus
plot.vars <- colnames(movies)[c(2,8:10,12)]

# create df for genre by year plot
genre.by.year <- data.frame(table(movies.full$release_date, movies.full$genre))
colnames(genre.by.year) <- c("year", "genre", "count")

