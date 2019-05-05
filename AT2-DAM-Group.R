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
test <-  readRDS('test_raw.rds')
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
test$age_band <- factor(case_when(
  test$age < 18 ~ 'under_18',
  test$age >= 18 & test$age <= 29 ~ '18_to_29',
  test$age >= 30 & test$age <= 44 ~ '30_to_44',
  test$age >= 45 ~ '45_and_over'),
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
test <- test %>% 
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

#################################################################################
#################################################################################
#################################################################################
# Self Note:
# 1. Dates: timestamp & release_date
# 2. Movies: movie_title
# 3. item_imdb_mature_rating
# 4. Maybe* Remove NA values 

# Duplicate for testing
original_test  = test
original_train = train

##################################
##### Drop obvious variables #####
##################################
# NULL values
test$video_release_date  = NULL
train$video_release_date = NULL
# Useless of analyse
test$imdb_url  = NULL
train$imdb_url = NULL
# Replacement by item_id
#test$movie_title  = NULL
#train$movie_title = NULL
# The rating based on age is already being calculated
test$age  = NULL
train$age = NULL
test$age_band  = NULL
train$age_band = NULL
# The rating based on gender is already being calculated
test$gender  = NULL
train$gender = NULL
# Drop variables occupation and zip_code because already calculated the rating
train$zip_code   = NULL
test$zip_code    = NULL
train$occupation = NULL
test$occupation  = NULL

##########################################
##### Drop variables with reasonings ##### 
##########################################
# High missing matches (70%) between item_id and zipcodes
train$zipcode_mean_rating = NULL
test$zipcode_mean_rating = NULL
# Low variability in genres average user rating
train[,7:25] = NULL
test[,6:24] = NULL

#################################################################
##### Remove item_id from train that does not exist in test #####
#################################################################
# Reason: Factor of train and test does not match
train.t = train
test.t = test
checking = train.t %>% semi_join(test.t, by = "user_item") # keep rows with matching ID











#############################
##### Replace NA values #####
#############################
# Note: Replace NA values of movies not available from scrape for train and test set.
# For train, ratings will be replaced by the user rating * 2L, votes will be replaced by min
# For test, rating will be replaced by user rating * 2L of from train.
scrape.NA <- scrape[rowSums(is.na(scrape)) > 37,]
##### For train set
val11 = min(train$item_imdb_count_ratings, na.rm = TRUE)
val13 = min(train$item_imdb_length, na.rm = TRUE)
val14 = min(train$item_imdb_staff_votes, na.rm = TRUE)
val16 = min(train$item_imdb_top_1000_voters_votes, na.rm = TRUE)
val19 = min(train$user_gender_item_imdb_votes, na.rm = TRUE)
val20 = min(train$user_age_band_item_imdb_votes, na.rm = TRUE)
val22 = min(train$user_gender_age_band_item_imdb_votes, na.rm = TRUE)
for (i in 1:nrow(train)) {
  if (sum(train$item_id[i] == scrape.NA$movie_id) == 1) {
    if (is.na(train[i,13])) { # Check
      train[i,13] = val13 # Average movie length
    }
    # All missing is replaced by mean user rating of that item * 2 (imdb rating is out of 10)
    if (is.na(train[i,10])) {
      train[i,10] = colMeans(train[which(train$item_id == train$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(train[i,15])) { 
      train[i,15] = colMeans(train[which(train$item_id == train$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(train[i,17])) { 
      train[i,17] = colMeans(train[which(train$item_id == train$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(train[i,18])) { 
      train[i,18] = colMeans(train[which(train$item_id == train$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(train[i,21])) { 
      train[i,21] = colMeans(train[which(train$item_id == train$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(train[i,23])) { 
      train[i,23] = colMeans(train[which(train$item_id == train$item_id[i]),3], na.rm = TRUE) * 2L
    }
    # All vote counts is replaced by the mean
    if (is.na(train[i,11])) { 
      train[i,11] = val11
    }
    if (is.na(train[i,14])) { 
      train[i,14] = val14
    }
    if (is.na(train[i,16])) { 
      train[i,16] = val16
    }
    if (is.na(train[i,19])) { 
      train[i,19] = val19
    }
    if (is.na(train[i,20])) { 
      train[i,20] = val20
    }
    if (is.na(train[i,22])) { 
      train[i,22] = val22
    }
  }
}
rm(val11,val13,val14,val16,val19,val20,val22,i) # Clean

##### For test set
# Note: Rating will be based on rating from the train
val10 = min(test$item_imdb_count_ratings, na.rm = TRUE)
val12 = min(test$item_imdb_length, na.rm = TRUE)
val13 = min(test$item_imdb_staff_votes, na.rm = TRUE)
val15 = min(test$item_imdb_top_1000_voters_votes, na.rm = TRUE)
val18 = min(test$user_gender_item_imdb_votes, na.rm = TRUE)
val19 = min(test$user_age_band_item_imdb_votes, na.rm = TRUE)
val21 = min(test$user_gender_age_band_item_imdb_votes, na.rm = TRUE)
for (i in 1:nrow(test)) {
  if (sum(test$item_id[i] == scrape.NA$movie_id) == 1) {
    if (is.na(test[i,12])) { # Check
      test[i,12] = val12 # Average movie length
    }
    # All missing is replaced by user rating * 2 (imdb rating is out of 10)
    if (is.na(test[i,9])) { 
      test[i,9] = colMeans(train[which(train$item_id == test$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(test[i,14])) { 
      test[i,14] = colMeans(train[which(train$item_id == test$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(test[i,16])) { 
      test[i,16:17] = colMeans(train[which(train$item_id == test$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(test[i,17])) { 
      test[i,16:17] = colMeans(train[which(train$item_id == test$item_id[i]),3], na.rm = TRUE) * 2L
    }
   if (is.na(test[i,20])) { 
      test[i,20] = colMeans(train[which(train$item_id == test$item_id[i]),3], na.rm = TRUE) * 2L
    }
    if (is.na(test[i,22])) { 
      test[i,22] = colMeans(train[which(train$item_id == test$item_id[i]),3], na.rm = TRUE) * 2L
    }
    # All vote counts is replaced by the mean
    if (is.na(test[i,10])) { 
      test[i,10] = val10
    }
    if (is.na(test[i,13])) { 
      test[i,13] = val13
    }
    if (is.na(test[i,15])) { 
      test[i,15] = val15
    }
    if (is.na(test[i,18])) { 
      test[i,18] = val18
    }
    if (is.na(test[i,19])) { 
      test[i,19] = val19
    }
    if (is.na(test[i,21])) { 
      test[i,21] = val21
    }
  }
}
rm(val10,val12,val13,val15,val18,val19,val21,i,scrape.NA) # Clean






###############
##### EDA #####
###############
##### Plot rating based on genres
# Make a new data frame for ease of plot
genres.df = train[,7:25]
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
rm(i,j,count,genres.df,record)
############
# Decision: Genres ranges from 3.2 to 3.9.1: Not significant => Drop all genres.
############

##### Plot rating based on IMDB rating
var1 = train %>%
  select_at(vars("user_id","rating","item_imdb_rating_of_ten")) %>%
  group_by(user_id) %>%
  summarise_all("mean")
var1$rating = round(var1$rating, digit = 1)
# Plot
ggplot(data = var1, aes(x = rating, y = item_imdb_rating_of_ten)) +
  geom_jitter() +  geom_smooth(method = "lm") + ggtitle("Rating of User vs. IMDB")
rm(var1) # Clean

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






#########################
##### Data cleaning #####
#########################
# Check for duplication: None
sum(duplicated(train))
sum(duplicated(test))
sum(duplicated(scrape))
# Plot missing valuee
missing.values <- aggr(train, sortVars = T, prop = T, 
                       sortCombs = T, cex.lab = 1.5, 
                       cex.axis = .6, cex.numbers = 5, 
                       combined = F, gap = -.2)
missing.values <- aggr(test, sortVars = T, prop = T, 
                       sortCombs = T, cex.lab = 1.5, 
                       cex.axis = .6, cex.numbers = 5, 
                       combined = F, gap = -.2)
rm(missing.values) # Clean
############
# Decision: 
# 1. More than 70% missing values matches between movies and zip_code => Drop zip_code_mean_rating
# 2. Replace NA values of IMDB ratings with user rating * 2.
# 3. Vote numbers & movie length = minimum values
############
# Check complete rows with no NA
sum(complete.cases(train)) # 76060 / 80523 = 94.5%
sum(complete.cases(test))  # 16154 / 19477 = 82.9%
colSums(sapply(train, is.na))
colSums(sapply(test, is.na))





check = table(test$occupation)

##### Drop movies that is NA for Global rating and Top 1000 from Scrape
#drop = which(is.na(scrape$rating_of_ten) == TRUE) # Global rating = NA
#drop1 = which(is.na(scrape$top_1000_voters_average) == TRUE) # Top 1000 = NA
#drop0 = unique(sort(c(drop,drop1)))
#train <- train[!train$item_id %in% scrape$movie_id[drop0],]
#test <- test[!test$item_id %in% scrape$movie_id[drop0],]
#rm(drop,drop1,drop0) # Clean






#######################
##### Play ground #####
#######################
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
num_vars = unlist(lapply(train_n, is.numeric))  
re_nums  = train_n[,num_vars]
re_corr  = cor(re_nums, use = "complete.obs")
corrplot::corrplot(re_corr, method="number")
rm(num_vars,re_nums,re_corr) # Clean
# Note: Remove all variables >= 0.8 corelation

##### Linear Regression
train_n = train # R = 0.4477 & 0.4144
train_n$user_id = NULL
train_n$item_id = NULL
regres <- lm(rating ~ ., data=train_n[1:10000,])
summary(regres)
rm(regres,train_n) # Clean

####################
##### FINALIZE #####
####################
##### Concatenate user_id and item_id together then remove the 2 used variables for both test and train
train$user_item = paste(train$user_id, train$item_id, sep = "_")
train$user_id = NULL
train$item_id = NULL
test$user_item = paste(test$user_id, test$item_id, sep = "_")
test$user_id = NULL
test$item_id = NULL