assign("last.warning", NULL, envir = baseenv()) # Clear all warnings
# Clean
rm(list=ls())
library(ggplot2)
library(ggrepel)
library(ggthemes) 
library(scales) 
library(VIM)
library(data.table)
library(formattable)
library(plotly)
library(corrplot)
library(GGally)
library(caret)
library(hydroGOF)
# Load packages
library(tidyverse)
library(lubridate)
library(forcats)
library(caret)
library(dplyr)
library(mice)
###############################
##### AT2_prep_students.R #####
###############################
scrape <- readRDS('scrape.rds')
validation <-  readRDS('test_raw.rds')
train <- readRDS('train_raw.rds')
# IMDB webscraping general item data
scrape_item_general <- scrape %>% select(movie_id, rating_of_ten:length,
                                         imdb_staff_votes:top_1000_voters_average)
# Add item_imdb_ to column names to disambiguate
names(scrape_item_general)[c(-1,-6,-7)] <- paste0('imdb_', names(scrape_item_general)[c(-1,-6,-7)])
names(scrape_item_general)[-1] <- paste0('item_', names(scrape_item_general)[-1])
# User age bands (Train)
train$age_band <- factor(case_when(
  train$age < 18 ~ 'under_18',
  train$age >= 18 & train$age <= 29 ~ '18_to_29',
  train$age >= 30 & train$age <= 44 ~ '30_to_44',
  train$age >= 45 ~ '45_and_over'),
  levels = c('under_18', '18_to_29', '30_to_44', '45_and_over'),
  ordered = TRUE # Don't order as makes modelling annoying
)
# User age bands (Test)
validation$age_band <- factor(case_when(
  validation$age < 18 ~ 'under_18',
  validation$age >= 18 & validation$age <= 29 ~ '18_to_29',
  validation$age >= 30 & validation$age <= 44 ~ '30_to_44',
  validation$age >= 45 ~ '45_and_over'),
  levels = c('under_18', '18_to_29', '30_to_44', '45_and_over'),
  ordered = TRUE # Don't order as makes modelling annoying
)
# Mean rating per item
item_mean_ratings_train <- train %>% 
  group_by(item_id) %>% 
  summarise(item_mean_rating = mean(rating))
# - Gender ratings per item
user_gender_item_mean_ratings_train <- train %>% 
  group_by(gender, item_id) %>% 
  summarise(user_gender_item_mean_rating = mean(rating)) %>% 
  ungroup()
# - Age band ratings per item
user_age_band_item_mean_ratings_train <- train %>% 
  group_by(age_band, item_id) %>% 
  summarise(user_age_band_item_mean_rating = mean(rating)) %>% 
  ungroup()
# - IMDB Gender ratings per item
scrape_gender_specific <- scrape %>% 
  select(movie_id, males_votes:females_average) %>% 
  gather(key = gender_score, value = value, -movie_id, na.rm = TRUE) %>% 
  separate(col = gender_score, into = c('gender', 'score_type')) %>% 
  spread(key = score_type, value = value) %>% 
  mutate(gender = fct_recode(gender, 'M' = 'males', 'F' = 'females'),
         gender = fct_rev(gender)) %>% # To get the factor levels right
  rename(item_id = movie_id,
         user_gender_item_imdb_mean_rating = average,
         user_gender_item_imdb_votes = votes)
# Check levels are correct
identical(levels(train$gender), levels(scrape_gender_specific$gender))
# Rating based on zipcode
zipcode_mean_ratings_train <- train %>% 
  group_by(zip_code,item_id) %>% 
  summarise(zipcode_mean_rating = mean(rating)) %>%
  ungroup()
# Rating based on occupation
occupation_mean_ratings_train <- train %>% 
  group_by(occupation,item_id) %>% 
  summarise(occupation_mean_rating = mean(rating)) %>%
  ungroup()
# - Age band ratings per item
scrape_age_specific <- scrape %>% 
  select(movie_id, starts_with('aged')) %>% 
  gather(key = age_score, value = value, -movie_id, na.rm = TRUE) %>% 
  separate(col = age_score, into = c('age_band', 'score_type'), sep = '_a|_v') %>% 
  spread(key = score_type, value = value) %>% 
  mutate(age_band = fct_recode(age_band,
                               'under_18' = 'aged_under_18',
                               '18_to_29' = 'aged_18-29',
                               '30_to_44' = 'aged_30_44',
                               '45_and_over' = 'aged_45'),
         age_band = factor(age_band,
                           levels = c('under_18', '18_to_29', '30_to_44', '45_and_over'),
                           ordered = TRUE)) %>% # To get the factor levels the same as ml_data 
  rename(item_id = movie_id,
         user_age_band_item_imdb_mean_rating = verage,
         user_age_band_item_imdb_votes = otes)
# Check levels are correct
identical(levels(train$age_band), levels(scrape_age_specific$age_band))
# - Gender-Age band ratings per item
scrape_gender_age_specific <- scrape %>% 
  select(movie_id, males_under_18_votes:females_under_18_average,
         males_18_29_votes:females_18_29_average,
         males_30_44_votes:females_30_44_average,
         males_45_votes:females_45_average) %>% 
  gather(key = gender_age_score, value = value, -movie_id, na.rm = TRUE) %>% 
  separate(col = gender_age_score, into = c('gender', 'age_score_type'),
           extra = 'merge') %>% 
  separate(col = age_score_type, into = c('age_band', 'score_type'), sep = '_a|_v') %>% 
  spread(key = score_type, value = value) %>% 
  mutate(gender = fct_recode(gender, 'M' = 'males', 'F' = 'females'),
         gender = fct_rev(gender),
         age_band = fct_recode(age_band,
                               'under_18' = 'under_18',
                               '18_to_29' = '18_29',
                               '30_to_44' = '30_44',
                               '45_and_over' = '45'),
         age_band = factor(age_band,
                           levels = c('under_18', '18_to_29', '30_to_44', '45_and_over'),
                           ordered = TRUE)) %>% # To get the factor levels the same as ml_data 
  rename(item_id = movie_id,
         user_gender_age_band_item_imdb_mean_rating = verage,
         user_gender_age_band_item_imdb_votes = otes)
# Check levels are correct
identical(levels(train$gender), levels(scrape_gender_age_specific$gender))
identical(levels(train$age_band), levels(scrape_gender_age_specific$age_band))
# Joing Variables onto Train Data
train <- train %>% 
  # Train set specific joins
  left_join(item_mean_ratings_train, by = 'item_id') %>% 
  left_join(user_age_band_item_mean_ratings_train, by = c('age_band', 'item_id')) %>% 
  left_join(user_gender_item_mean_ratings_train, by = c('gender', 'item_id')) %>%
  # Scrape General joins (External dataset)
  left_join(scrape_item_general, by = c('item_id' = 'movie_id')) %>%
  left_join(scrape_gender_specific, by = c('gender', 'item_id')) %>% 
  left_join(scrape_age_specific, by = c('age_band', 'item_id')) %>%
  left_join(scrape_gender_age_specific, by = c('gender', 'age_band', 'item_id')) %>% 
  left_join(zipcode_mean_ratings_train, by = c('zip_code', 'item_id')) %>%
  left_join(occupation_mean_ratings_train, by = c('occupation', 'item_id')) #%>%
# Scrape User_Gender_Age joins (External dataset)
#  mutate(user_id = factor(user_id),      # Make user and item factors
#         item_id = factor(item_id))
# Joing Variables onto Test Data
validation <- validation %>% 
  # Train set specific joins
  left_join(item_mean_ratings_train, by = 'item_id') %>% 
  left_join(user_age_band_item_mean_ratings_train, by = c('age_band', 'item_id')) %>% 
  left_join(user_gender_item_mean_ratings_train, by = c('gender', 'item_id')) %>% 
  # Scrape General joins (External dataset)
  left_join(scrape_item_general, by = c('item_id' = 'movie_id')) %>%
  left_join(scrape_gender_specific, by = c('gender', 'item_id')) %>% 
  left_join(scrape_age_specific, by = c('age_band', 'item_id')) %>%
  left_join(scrape_gender_age_specific, by = c('gender', 'age_band', 'item_id')) %>% 
  left_join(zipcode_mean_ratings_train, by = c('zip_code', 'item_id')) %>%
  left_join(occupation_mean_ratings_train, by = c('occupation', 'item_id')) #%>%
#  mutate(user_id = factor(user_id),      # Make user and item factors
#         item_id = factor(item_id))
# Remove the variable sets (Train)
rm(item_mean_ratings_train, 
   user_gender_item_mean_ratings_train, user_age_band_item_mean_ratings_train)
# Remove the varibale sets (Scrape)
rm(scrape_age_specific, scrape_gender_age_specific, scrape_gender_specific, scrape_item_general)
rm(occupation_mean_ratings_train,zipcode_mean_ratings_train) # Clean

###########################
##### Processing Data #####
###########################
###### Remove unknown genres
train = train[-which(train$unknown==TRUE),] # Remove rows with genres unknown == TRUE
# Remove unknown genres
train$unknown = NULL
validation$unknown = NULL

##### Remove obvious columsn
train$age = NULL
validation$age = NULL
train$video_release_date = NULL
validation$video_release_date = NULL
train$imdb_url = NULL
validation$imdb_url = NULL

##### Re_order columsn for easy manipulation of data
re_order = c("user_id", "item_id", "rating", # Main
             # User ratings (1-5)
             "item_mean_rating", "user_age_band_item_mean_rating", "user_gender_item_mean_rating",
             "zipcode_mean_rating", "occupation_mean_rating",
             # IMDB ratings (1-10)
             "item_imdb_rating_of_ten", "item_imdb_staff_average", "item_imdb_top_1000_voters_average",
             "user_gender_item_imdb_mean_rating", "user_age_band_item_imdb_mean_rating",
             "user_gender_age_band_item_imdb_mean_rating",
             # Votes
             "item_imdb_count_ratings", "item_imdb_staff_votes", "item_imdb_top_1000_voters_votes",
             "user_gender_item_imdb_votes", "user_age_band_item_imdb_votes", "user_gender_age_band_item_imdb_votes",
             # Genres
             "action", "adventure", "animation", "childrens", "comedy", "crime", "documentary",
             "drama", "fantasy", "film_noir", "horror", "musical", "mystery", "romance", "sci_fi",
             "thriller", "war", "western",
             # User information
             "gender", "occupation", "zip_code", "age_band",
             # Movie information
             "timestamp", "movie_title", "release_date", "item_imdb_mature_rating", "item_imdb_length")
train <- train[,re_order]
validation = validation[,names(train[,-3])] # Copy column order to validation
rm(re_order)

##### Extract year and month of timestamp and release_date (Checked NA = none)
# Initiate empty new columns for month and year
train$release_year <- NA
train$timestamp_month <- NA
train$release_month <- NA

validation$release_year <- NA
validation$timestamp_month <- NA
validation$release_month <- NA
# Year
train$timestamp = as.character(train$timestamp)
train$release_date = as.character(train$release_date)
for (i in 1:nrow(train)) {
  train$release_year[i] = substring(train$release_date[i],1,4)
}
rm(i)
validation$timestamp = as.character(validation$timestamp)
validation$release_date = as.character(validation$release_date)
for (i in 1:nrow(validation)) {
  validation$release_year[i] = substring(validation$release_date[i],1,4)
}
rm(i)
# Month
for (i in 1:nrow(train)) {
  train$timestamp_month[i] = substring(train$timestamp[i],6,7)
  train$release_month[i] = substring(train$release_date[i],6,7)
}
rm(i)
for (i in 1:nrow(validation)) {
  validation$timestamp_month[i] = substring(validation$timestamp[i],6,7)
  validation$release_month[i] = substring(validation$release_date[i],6,7)
}
rm(i)
# Remove timestamp and release_date
train$timestamp = NULL
train$release_date = NULL
validation$timestamp = NULL
validation$release_date = NULL
# Remove title
train$movie_title = NULL
validation$movie_title = NULL
# Switch to numeric for year and month
train$release_month = as.numeric(train$release_month)
train$timestamp_month = as.numeric(train$timestamp_month)
train$release_year = as.numeric(train$release_year)
validation$release_month = as.numeric(validation$release_month)
validation$timestamp_month = as.numeric(validation$timestamp_month)
validation$release_year = as.numeric(validation$release_year)
# Drop variables timestamp_month & release_month because EDA shows no interest
train$release_month = NULL
validation$release_month = NULL
train$timestamp_month = NULL
validation$timestamp_month = NULL
# Factor out years (From 1922 to 1998)
train$year_band <- factor(case_when(
  train$release_year < 1950 ~ 'Before 1950s',
  train$release_year >= 1950 & train$release_year <= 1959 ~ '1950s',
  train$release_year >= 1960 & train$release_year <= 1969 ~ '1960s',
  train$release_year >= 1970 & train$release_year <= 1979 ~ '1970s',
  train$release_year >= 1980 & train$release_year <= 1989 ~ '1980s',
  train$release_year >= 1990 & train$release_year <= 1999 ~ '1990s'),
  levels = c('Before 1950s', '1950s', '1960s', '1970s', '1980s', '1990s'),
  ordered = TRUE
)
validation$year_band <- factor(case_when(
  validation$release_year < 1950 ~ 'Before 1950s',
  validation$release_year >= 1950 & validation$release_year <= 1959 ~ '1950s',
  validation$release_year >= 1960 & validation$release_year <= 1969 ~ '1960s',
  validation$release_year >= 1970 & validation$release_year <= 1979 ~ '1970s',
  validation$release_year >= 1980 & validation$release_year <= 1989 ~ '1980s',
  validation$release_year >= 1990 & validation$release_year <= 1999 ~ '1990s'),
  levels = c('Before 1950s', '1950s', '1960s', '1970s', '1980s', '1990s'),
  ordered = TRUE
)
# Remove variable release_year after factoring it out
train$release_year = NULL
validation$release_year = NULL


##### Note: load SAVED_DF.RData for ease of use.





# For NA movies, ratings of IMDB will be the rating 







##### Replace NA ratings and NA votes
# Use mice package
# Ratings
a = which(names(train) == "item_mean_rating")
b = which(names(train) == "user_gender_age_band_item_imdb_mean_rating")
mice(train[,a:b], m=5, maxit = 5, method = 'pmm', seed = 500)
rm(a,b)
# Replace vote by 0 for:
a = which(names(train) == "item_imdb_count_ratings")
b = which(names(train) == "user_gender_age_band_item_imdb_votes")
mice(train[,a:b], m=5, maxit = 5, method = 'pmm', seed = 500)
rm(a,b)



##### Tryout drop variables 
# Drop genres
a = which(names(train) == "action")
b = which(names(train) == "western")
train[,a:b] = NULL
validation[,a:b] = NULL