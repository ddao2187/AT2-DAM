# Load packages
library(tidyverse)
library(lubridate)
library(forcats)
library(caret)
library(rpart)
library(Metrics)
library(ggplot2)
library(data.table)
library(randomForest)
library(naniar)
library(mice)
library(missForest)

setwd("G:/Data Science/UTS Courses/36106 Data Algorithms and Meaning/DAM Assignment 2")

training_Dataset <- readRDS('AT2_train_STUDENT.rds')
test_Dataset <- readRDS('AT2_test_STUDENT.rds')

gg_miss_upset(training_Dataset)

#Update factors of item_imdb_mature_rating to consistent level
# - Put NC17 as R18+

#Drop nonuseful variables
#Drop variable video_release_date as all NAs; zip_code as there are no other variables that use zip code; age; imdb_url; item_imdb_length as 1670 out of 1680 movies (unique item_ids) has NA as values
training_Dataset <- select(training_Dataset, -age, -zip_code, -video_release_date, -imdb_url, -item_imdb_length,-item_imdb_staff_votes,-item_imdb_staff_average)
test_Dataset <- select(test_Dataset, -age, -zip_code, -video_release_date, -imdb_url, -item_imdb_length,-item_imdb_staff_votes,-item_imdb_staff_average)

#Drop all rows with unknown==TRUE, as this variable corresponds to only 10 data points and are not used by test dataset
training_Dataset <- training_Dataset[-which(training_Dataset$unknown==TRUE), ]
training_Dataset <- select(training_Dataset,-unknown)
test_Dataset <- select(test_Dataset,-unknown)

#First extract data points in the training dataset that have NAs (i.e. incomplete data points)
training_Dataset_Incomplete <- training_Dataset[!complete.cases(training_Dataset), ]
test_Dataset_Incomplete <- test_Dataset[!complete.cases(test_Dataset), ]
training_Dataset_Incomplete_ItemIDs <- sort(unique(training_Dataset_Incomplete$item_id))

#Then replace missing values with equivalent existing data values
temp_ItemIDs <- training_Dataset %>%
  group_by(item_id) %>%
  mutate(replaced_item_imdb_rating_of_ten = 2*item_mean_rating,
         replaced_item_imdb_count_ratings = n(),
         replaced_item_imdb_top_1000_voters_votes = 1,
         replaced_item_imdb_top_1000_voters_average = 1,
         replaced_user_gender_item_imdb_mean_rating = user_gender_item_mean_rating,
         replaced_user_age_band_item_imdb_mean_rating = user_age_band_item_mean_rating) %>%
  ungroup()

temp_ItemIDs_Gender <- training_Dataset %>%
  group_by(item_id,gender) %>%
  mutate(replaced_user_gender_item_imdb_votes = n()) %>%
  ungroup()  

temp_ItemIDs_AgeBand <- training_Dataset %>%
  group_by(item_id,age_band) %>%
  mutate(replaced_user_age_band_item_imdb_votes = n()) %>%
  ungroup()   
  
temp_ItemIDs_Gender_AgeBand <- training_Dataset %>%
  group_by(item_id,gender,age_band) %>%
  mutate(replaced_user_gender_age_band_item_imdb_votes = n(),
         replaced_user_gender_age_band_item_imdb_mean_rating = 2*item_mean_rating/n()) %>%
  ungroup()  

for (count in 1:nrow(training_Dataset)) {
  if (is.na(training_Dataset$item_imdb_rating_of_ten[count])) {
    training_Dataset$item_imdb_rating_of_ten[count] <- temp_ItemIDs$replaced_item_imdb_rating_of_ten[count]
  }
  if (is.na(training_Dataset$item_imdb_count_ratings[count])) {
    training_Dataset$item_imdb_count_ratings[count] <- temp_ItemIDs$replaced_item_imdb_count_ratings[count]
  }
  if (is.na(training_Dataset$item_imdb_top_1000_voters_votes[count])) {
    training_Dataset$item_imdb_top_1000_voters_votes[count] <- temp_ItemIDs$replaced_item_imdb_top_1000_voters_votes[count]
  }
  if (is.na(training_Dataset$item_imdb_top_1000_voters_average[count])) {
    training_Dataset$item_imdb_top_1000_voters_average[count] <- temp_ItemIDs$replaced_item_imdb_top_1000_voters_average[count]
  }
  if (is.na(training_Dataset$user_gender_item_imdb_mean_rating[count])) {
    training_Dataset$user_gender_item_imdb_mean_rating[count] <- temp_ItemIDs$replaced_user_gender_item_imdb_mean_rating[count]
  }
  if (is.na(training_Dataset$user_age_band_item_imdb_mean_rating[count])) {
    training_Dataset$user_age_band_item_imdb_mean_rating[count] <- temp_ItemIDs$replaced_user_age_band_item_imdb_mean_rating[count]
  }
  if (is.na(training_Dataset$user_gender_item_imdb_votes[count])) {
    training_Dataset$user_gender_item_imdb_votes[count] <- temp_ItemIDs_Gender$replaced_user_gender_item_imdb_votes[count]
  }
  if (is.na(training_Dataset$user_age_band_item_imdb_votes[count])) {
    training_Dataset$user_age_band_item_imdb_votes[count] <- temp_ItemIDs_AgeBand$replaced_user_age_band_item_imdb_votes[count]
  }
  if (is.na(training_Dataset$user_gender_age_band_item_imdb_votes[count])) {
    training_Dataset$user_gender_age_band_item_imdb_votes[count] <- temp_ItemIDs_Gender_AgeBand$replaced_user_gender_age_band_item_imdb_votes[count]
  }
  if (is.na(training_Dataset$user_gender_age_band_item_imdb_mean_rating[count])) {
    training_Dataset$user_gender_age_band_item_imdb_mean_rating[count] <- temp_ItemIDs_Gender_AgeBand$replaced_user_gender_age_band_item_imdb_mean_rating[count]
  }
}

training_Dataset$item_id <- as.integer(training_Dataset$item_id)

#### Models ####
set.seed(43)
training_Dataset$rating <- factor(training_Dataset$rating)
train_indices <- sample(seq_len(nrow(training_Dataset)), size=floor(0.2*nrow(training_Dataset)))
training_split <- training_Dataset[train_indices, ]
training_testset <- training_Dataset[-train_indices, ]

Random_Forest_Results10 <- randomForest(rating ~ .-movie_title-user_id-item_imdb_top_1000_voters_votes-item_imdb_top_1000_voters_average-user_gender_age_band_item_imdb_votes-user_gender_item_imdb_votes-user_age_band_item_imdb_votes-item_imdb_count_ratings-item_imdb_mature_rating, data=training_split, importance=TRUE,na.action=na.omit,proximity=TRUE)
Predict_RandomForest10 <- predict(object = Random_Forest_Results10, newdata = training_testset)
RMSE_RandomForest10 <- rmse(actual = as.numeric(training_testset$rating), predicted = as.numeric(Predict_RandomForest10))

