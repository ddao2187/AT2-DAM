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

###############################
##### Reorder the columns #####
###############################
train = train[,c(1,6,7,                   # Main
                 33:36,41,43,44,47,49:51, # Rating (7:14 ~ Rating of 10, 3:5 ~ Rating of 5)
                 37,40,42,45,46,48,       # Vote number (15:19)
                 38,39,2:5,8:31,32)]         # Others
validation = validation[,names(train[,-3])] # Copy column order to validation
names(validation) == names(train[,-3]) # Check

#################################################################################
#################################################################################
#################################################################################
# Self Note:
# 1. Dates: timestamp & release_date
# 2. Movies: movie_title
# 3. item_imdb_mature_rating
# 4. Maybe* Remove NA values 
# Factor mature rating
train$item_imdb_mature_rating = factor(train$item_imdb_mature_rating)
validation$item_imdb_mature_rating = factor(validation$item_imdb_mature_rating)
identical(levels(train$item_imdb_mature_rating), levels(validation$item_imdb_mature_rating)) # Check
##################################
##### Drop obvious variables #####
##################################
# NULL values
validation$video_release_date  = NULL
train$video_release_date = NULL
# Useless of analyse
validation$imdb_url  = NULL
train$imdb_url = NULL
# Replacement by item_id
#validation$movie_title  = NULL
#train$movie_title = NULL
# The rating based on age is already being calculated
validation$age  = NULL
train$age = NULL
#validation$age_band  = NULL
#train$age_band = NULL
# The rating based on gender is already being calculated
#validation$gender  = NULL
#train$gender = NULL
# Drop variables occupation and zip_code because already calculated the rating
#train$zip_code   = NULL
#validation$zip_code    = NULL
#train$occupation = NULL
#validation$occupation  = NULL

##########################################
##### Drop variables with reasonings ##### 
##########################################
# High missing matches (70%) between item_id and zipcodes
train$zipcode_mean_rating = NULL
validation$zipcode_mean_rating = NULL
# Low variability in genres average user rating
validation[,names(train[,25:ncol(train)])] = NULL
train[,25:ncol(train)] = NULL

#############################
##### Replace NA values #####
#############################
# Note: Replace NA values of movies not available from scrape for train and validation set.
# For train, ratings will be replaced by the user rating * 2L, votes will be replaced by min
# For validation, rating will be replaced by user rating * 2L of from train.
scrape.NA <- scrape[rowSums(is.na(scrape)) > 37,] # Movies with empty values
##### For train set
val = train[1,14:19]
count = 1
for (i in 14:19) {
  val[1,count] = min(train[,i], na.rm = TRUE)
  count = count + 1
}
rm(i,count)
val_l = min(train$item_imdb_length, na.rm = TRUE)
for (i in 1:nrow(train)) {
  if (sum(train$item_id[i] == scrape.NA$movie_id) == 1) {
    if (is.na(train[i,21])) { # Check
      train[i,21] = val_l # Average movie length
    }
    # All missing is replaced by mean user rating of that item * 2 (imdb rating is out of 10)
    if (is.na(train[i,7])) {
      train[i,7] = colMeans(train[which(train$item_id == train$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(train[i,8])) { 
      train[i,8] = colMeans(train[which(train$item_id == train$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(train[i,9])) { 
      train[i,9] = colMeans(train[which(train$item_id == train$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(train[i,10])) { 
      train[i,10] = colMeans(train[which(train$item_id == train$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(train[i,11])) { 
      train[i,11] = colMeans(train[which(train$item_id == train$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(train[i,12])) { 
      train[i,12] = colMeans(train[which(train$item_id == train$item_id[i]),3], na.rm = TRUE) * 2L
    }
    # All vote counts is replaced by the mean
    if (is.na(train[i,14])) { 
      train[i,14] = val[1,1]
    }
    if (is.na(train[i,15])) { 
      train[i,15] = val[1,2]
    }
    if (is.na(train[i,16])) { 
      train[i,16] = val[1,3]
    }
    if (is.na(train[i,17])) { 
      train[i,17] = val[1,4]
    }
    if (is.na(train[i,18])) { 
      train[i,18] = val[1,5]
    }
    if (is.na(train[i,19])) { 
      train[i,19] = val[1,6]
    }
  }
}
rm(i,val_l,val) # Clean

##### For validation set
# Note: Rating of validation will be based on rating from the train
val = validation[1,13:18]
count = 1
for (i in 13:18) {
  val[1,count] = min(validation[,i], na.rm = TRUE)
  count = count + 1
}
rm(i,count)
val_l = min(validation$item_imdb_length, na.rm = TRUE)
for (i in 1:nrow(validation)) {
  if (sum(validation$item_id[i] == scrape.NA$movie_id) == 1) {
    if (is.na(train[i,20])) { # Check
      train[i,20] = val_l # Average movie length
    }
    # All missing is replaced by mean user rating of that item * 2 (imdb rating is out of 10)
    if (is.na(validation[i,6])) {
      validation[i,6] = colMeans(train[which(train$item_id == validation$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(validation[i,7])) { 
      validation[i,7] = colMeans(train[which(train$item_id == validation$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(validation[i,8])) { 
      validation[i,8] = colMeans(train[which(train$item_id == validation$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(validation[i,9])) { 
      validation[i,9] = colMeans(train[which(train$item_id == validation$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(validation[i,10])) { 
      validation[i,10] = colMeans(train[which(train$item_id == validation$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(validation[i,11])) { 
      validation[i,11] = colMeans(train[which(train$item_id == train$item_id[i]),3], na.rm = TRUE) * 2L
    }
    # All vote counts is replaced by the mean
    if (is.na(validation[i,13])) { 
      validation[i,13] = val[1,1]
    }
    if (is.na(validation[i,14])) { 
      validation[i,14] = val[1,2]
    }
    if (is.na(validation[i,15])) { 
      validation[i,15] = val[1,3]
    }
    if (is.na(validation[i,16])) { 
      validation[i,16] = val[1,4]
    }
    if (is.na(validation[i,17])) { 
      validation[i,17] = val[1,5]
    }
    if (is.na(validation[i,18])) { 
      validation[i,18] = val[1,6]
    }
  }
}
rm(i,val_l,val,scrape.NA) # Clean

##### Scale down IMDB rating into 5 points rating
train[,7:12] = train[,7:12] / 2L
validation[,6:11] = validation[,6:11] / 2L







###############
##### EDA #####
###############
# Data summary: (train)
# 1:3 ~ user_id, item_id, rating
# 4:6 ~ item_mean_rating, user_age_band_item_mean_rating, user_gender_item_mean_rating
# 7:14 ~ item_imdb_rating_of_ten(*), item_imdb_staff_average(*), item_imdb_top_1000_voters_average (*)
#        user_age_band_item_imdb_mean_rating, user_gender_age_band_item_imdb_mean_rating
#        user_gender_item_imdb_mean_rating, zipcode_mean_rating, occupation_mean_rating
# 15:20 ~ item_imdb_count_ratings, item_imdb_staff_votes, item_imdb_top_1000_voters_votes
#         user_gender_item_imdb_votes, user_age_band_item_imdb_votes, user_gender_age_band_item_imdb_votes
# 21:31 ~ Others
# 31:50 ~ Genres
# * = Good predictor

##### Plot rating based on genres
# Note: Run from line 1 to 182 first
genres.df = train[,32:50]
genres.df[,20] = train[,3]
record = genres.df[1,1:19] # For record
record[1,] = 0
count = 0
for (j in 1:19) {
  for (i in 1:nrow(genres.df)) {
    if (genres.df[i,j] == TRUE) {
      record[1,j] = record[1,j] + genres.df[i,20]
      count = count + 1
    }
  }
  record[1,j] = record[1,j] / count
  count = 0
  print("Loading")
}
min(record)
max(record)
glimpse(record)
rm(i,j,count,genres.df,record)
############
# Decision: Genres ranges from 3.199629 to 3.916488 out of 5: Not significant => Drop all genres.
############
##### Plot rating based on check
## By Users
check = "item_mean_rating"
#check = "user_age_band_item_mean_rating"
#check = "user_gender_item_mean_rating"

# Gender, mature rating, movie length, occupation
p1 <- ggplot(data=train, aes(x=timestamp_year)) + 
  geom_bar(fill="blue") + 
  xlab("Movie timestamp_year") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
p2 <- ggplot(data=train, aes(x=release_year)) + 
  geom_bar(fill="blue") + 
  xlab("Movie release_year") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
p3 <- ggplot(data=train, aes(x=timestamp_month)) + 
  geom_bar(fill="blue") + 
  xlab("Movie timestamp_month") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
p4 <- ggplot(data=train, aes(x=release_month)) + 
  geom_bar(fill="blue") + 
  xlab("Movie release_month") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
grid.arrange(p2, p3, p1, p4, nrow=2,
             top="Movie EDA")

# Timestamp, release date, zipcode
p1 <- ggplot(data=train, aes(x=timestamp)) + 
  geom_bar(fill="blue") + 
  xlab("Movie timestamp") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
p2 <- ggplot(data=train, aes(x=release_date)) + 
  geom_bar(fill="blue") + 
  xlab("Movie release_date") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
p3 <- ggplot(data=train, aes(x=zip_code)) + 
  geom_bar(fill="blue") + 
  xlab("Movie zip_code") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
grid.arrange(p2, p3, p1, nrow=2,
             top="Movie EDA")
# Note no EDA on title yet






## By ratings
#check = "item_imdb_rating_of_ten"
#check = "item_imdb_staff_average"
#check = "item_imdb_top_1000_voters_average"
#check = "user_age_band_item_imdb_mean_rating"
#check = "user_gender_age_band_item_imdb_mean_rating"
#check = "user_gender_item_imdb_mean_rating"
#check = "zipcode_mean_rating"
#check = "occupation_mean_rating"
#check = "user_age_band_item_imdb_mean_rating"

## By vote numbers
#check = "item_imdb_count_ratings"
#filt = filter(train, occupation == "student")
var1 = train %>%
  select_at(vars("user_id","rating",check))
var1 = aggregate(var1[,2:3], list(var1$user_id),mean)
var1$rating = floor(var1$rating)
# Plot
ggplot(data = var1, aes(x = rating, y = check)) +
  geom_jitter() + ggtitle(check)
rm(var1) # Clean
# Check if this predictor has any unexpected values
length(which(train[,check] > 10 | train[,check] < 0))

# Plot by average rating
var1 = train %>%
  select_at(vars("rating",check)) %>%
  group_by(rating) %>%
  summarise_all("mean")
var1$rating = floor(var1$rating)
# Plot
ggplot(data = var1, aes(x = rating, y = check)) +
  geom_jitter() + ggtitle(check)
rm(var1) # Clean


##################







##### Plot rating based on top 1000 rating
var1 = train %>%
  select_at(vars("user_id","rating","item_imdb_top_1000_voters_average")) %>%
  group_by(user_id) %>%
  summarise_all("mean")
var1$rating = round(var1$rating, digit = 1)
# Plot
ggplot(data = var1, aes(x = rating, y = item_imdb_top_1000_voters_average)) +
  geom_jitter() +  geom_smooth(method = "lm") + ggtitle("Rating of User vs. Top 1000")
rm(var1) # Clean

##### Plot rating based on age_band
# Calculate mean of ratings
var1 = train %>%
  select_at(vars("user_id","rating","user_age_band_item_mean_rating","age_band")) %>%
  group_by(user_id,age_band) %>%
  summarise_at(vars("rating","user_age_band_item_mean_rating"), funs("mean"))
# Filter for each type of age_band for visiualization
var1 = filter(var1, age_band == "under_18")
#var1 = filter(var1, age_band == "18_to_29")
#var1 = filter(var1, age_band == "30_to_44")
#var1 = filter(var1, age_band == "45_and_over")
# Plot
ggplot(data = var1, aes(x = rating, y = user_age_band_item_mean_rating)) +
  geom_jitter() +  geom_smooth(method = "lm") + ggtitle("Rating of User vs. By age_band")
rm(var1) # Clean

##### Plot rating based on item_imdb_mature_rating rating
var1 = train %>%
  select_at(vars("user_id","rating","item_imdb_mature_rating")) %>%
  group_by(item_imdb_mature_rating) %>%
  summarise_at(vars("rating"), funs("mean"))
# Plot
ggplot(data = var1, aes(x = item_imdb_mature_rating, y = rating)) +
  geom_bar(stat="identity") +  geom_smooth(method = "lm") + ggtitle("Ratings based on mature rate")
rm(var1) # Clean

#########################
##### Data cleaning #####
#########################
# Check for duplication: None
sum(duplicated(train))
sum(duplicated(validation))
sum(duplicated(scrape))
# Plot missing valuee
missing.values <- aggr(train, sortVars = T, prop = T, 
                       sortCombs = T, cex.lab = 1.5, 
                       cex.axis = .6, cex.numbers = 5, 
                       combined = F, gap = -.2)
missing.values <- aggr(validation, sortVars = T, prop = T, 
                       sortCombs = T, cex.lab = 1.5, 
                       cex.axis = .6, cex.numbers = 5, 
                       combined = F, gap = -.2)
rm(missing.values) # Clean
# Check complete rows with no NA
sum(complete.cases(train)) # 76060 / 80523 = 94.5%
sum(complete.cases(validation))  # 16154 / 19477 = 82.9%
colSums(sapply(train, is.na))
colSums(sapply(validation, is.na))
colSums(sapply(train_n, is.na))
############
# Decision: 
# 1. More than 70% missing values matches between movies and zip_code => Drop zip_code_mean_rating
# 2. Replace NA values of IMDB ratings with user rating * 2.
# 3. Vote numbers & movie length = minimum values
############




check = table(validation$occupation)

##### Drop movies that is NA for Global rating and Top 1000 from Scrape
#drop = which(is.na(scrape$rating_of_ten) == TRUE) # Global rating = NA
#drop1 = which(is.na(scrape$top_1000_voters_average) == TRUE) # Top 1000 = NA
#drop0 = unique(sort(c(drop,drop1)))
#train <- train[!train$item_id %in% scrape$movie_id[drop0],]
#validation <- validation[!validation$item_id %in% scrape$movie_id[drop0],]
#rm(drop,drop1,drop0) # Clean







####################
##### Cheating #####
####################
#trainset_indices <- sample(seq_len(nrow(train)), size = 5000) # Reduce the data size
#train_n <- train[trainset_indices, ]
train_n = train
train_n = train_n[order(train_n$item_id),] # Re-order by item_id
validation_n = validation
#rm(trainset_indices)
# Temporary drop to make regression running (able to make prediction)
train_n$movie_title = NULL
validation_n$movie_title = NULL
train_n$release_date = NULL
validation_n$release_date = NULL
train_n$timestamp = NULL
validation_n$timestamp = NULL
#train_n$item_imdb_mature_rating = NULL
#validation$item_imdb_mature_rating = NULL
# Factor rating
#train_n$rating = factor(train_n$rating)

# Replace all NA values by 0
train_n[is.na(train_n)] <- 0
validation_n[is.na(validation_n)] <- 0

#############################
##### Linear Regression #####
#############################
# Reduce DF size and split trainset and testset appropriately
# trainset: train_n
# testset: testset
set.seed(1)
# Initiate empty testset
testset = train_n[0,]
dummy = 0
count = 1
for (i in 1:nrow(train_n)) {
  if (train_n$item_id[i] > dummy) {
    dummy = train_n$item_id[i]
    testset[count,] = train_n[i,]
    count = count + 1
  }
}
rm(i,dummy,count)
train_n$user_id = factor(train_n$user_id)
train_n$item_id = factor(train_n$item_id)
# Check
length(unique(train_n$item_id)) == length(unique(testset$item_id))
# Train the model
regres <- glm(rating ~ ., data=train_n[,3:ncol(train_n)]) # Exclude item_id and user_id for regression
# Rate RMSE
testset$prediction = predict(regres, testset[,3:ncol(testset)], type = "response")
# Check if any rating > 5 (wrong)
testset$prediction = floor(testset$prediction)
which(testset$prediction > 5L)
which(testset$prediction < 0L)

rmse(testset$rating, testset$prediction)
length(which(is.na(testset$prediction) == TRUE)) / nrow(testset) # % of missing values 
rm(train_n,testset) # Clean

# Record notes:


####################
##### FINALIZE #####
####################
validation_n$prediction = predict(regres, validation_n[,3:ncol(validation_n)], type = "response")
##### Concatenate user_id and item_id together then remove the 2 used variables for both testset and train
validation_n$user_item = paste(validation_n$user_id, validation_n$item_id, sep = "_")
validation_n$user_id = NULL
validation_n$item_id = NULL
# Move columns
validation_n <- validation_n %>%
  select(user_item, everything()) 
validation_n <- validation_n %>%
  select(prediction, everything()) 
colnames(validation_n) = c("rating","user_item")
validation_n$rating = floor(validation_n$rating)
# Remove all other columns
validation_n = validation_n[,1:2]
# Check if any rating > 5 (wrong)
which(validation_n$rating > 5L)
which(validation_n$rating < 0L)
#validation_n$rating[which(validation_n$rating < 0L)] = abs(validation_n$rating[which(validation_n$rating < 0L)])
min(validation_n$rating)
# Save to file
write.csv(validation_n, file = 'AT2_UPLOAD.csv',quote = FALSE, row.names = FALSE)
rm(validation_n)

glimpse(train[which(is.na(train$item_imdb_staff_votes) == TRUE),])
glimpse(scrape[which(scrape$movie_id == "103"),])














#######################
##### Play ground #####
#######################

train_p = filter(train,item_id == 61)
train_p[,27:31] = NULL
train_p[,4] =NULL
train_p[,26:44] =NULL
train_p[,21] = NULL
max(train_p$rating)
histogram(train_p$rating)


train_p1 = filter(train_p,occupation == "programmer")
max(train_p1$rating)
histogram(train_p1$rating)


##### Test drop variables
#train_n$user_age_band_item_mean_rating = NULL # R = 0.4087 & 0.3738
#train_n$user_gender_item_mean_rating = NULL # R = 0.437 & 0.4038  
#train_n$item_imdb_rating_of_ten = NULL # 0.4452 & 0.4125
#train_n$item_imdb_top_1000_voters_average = NULL # 0.4477 & 0.4151 (Slight increase adjusted)
#train_n$user_gender_item_imdb_mean_rating = NULL # 0.4463 & 0.4136
#train_n$user_age_band_item_imdb_mean_rating = NULL # 0.4476 & 0.415
#train_n$user_gender_age_band_item_imdb_mean_rating = NULL # 0.4477 & 0.4151 (Slight increase adjusted)

#train_n$item_imdb_staff_votes = NULL
#train_n$user_gender_item_imdb_votes = NULL
#train_n$user_gender_age_band_item_imdb_votes = NULL
#train_n$user_age_band_item_imdb_votes = NULL

# Multi-collinearity check
num_vars = unlist(lapply(train, is.numeric))  
re_nums  = train[,num_vars]
re_corr  = cor(re_nums, use = "complete.obs")
corrplot::corrplot(re_corr, method="number")
rm(num_vars,re_nums,re_corr) # Clean
# Note: Remove all variables >= 0.8 corelation

df = validation

a = which(names(df) == "action")
b = which(names(df) == "western")
col_name = names(df)[a:b]
test = matrix(ncol=18,nrow=1)
colnames(test) = col_name
for (i in 1:b-a+1) {
  c = as.numeric(i+a-1)
  test[,i] = length(which(df[,c] == TRUE))
}

barplot(test, main="validation Chart", xlab="Genres")

new = filter(train,drama == TRUE)
new_v = filter(validation,drama == TRUE)
min(new$item_imdb_rating_of_ten)
max(new$item_imdb_rating_of_ten)
mean(new$item_imdb_rating_of_ten)




