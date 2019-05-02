# Note:
# 1. Dates: timestamp & release_date
# 2. Movies: movie_title
# 3. item_imdb_mature_rating
# 4. Maybe* Remove NA values 

#####################
##### Load file #####
#####################
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
# Webscraping data
scrape = readRDS('scrape.rds')
# STUDENT testing and training data
test   =  readRDS('AT2_test_STUDENT.rds')
train  = readRDS('AT2_train_STUDENT.rds')
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
# Convert to merge
zipcode_mean_ratings_train$item_id = as.numeric(zipcode_mean_ratings_train$item_id)
occupation_mean_ratings_train$item_id = as.numeric(occupation_mean_ratings_train$item_id)
train$item_id = as.numeric(train$item_id)
test$item_id = as.numeric(test$item_id)
# Join
train <- train %>% 
  left_join(zipcode_mean_ratings_train, by = c('zip_code', 'item_id')) %>%
  left_join(occupation_mean_ratings_train, by = c('occupation', 'item_id')) %>%
  mutate(item_id = factor(item_id))
test <- test %>% 
  left_join(zipcode_mean_ratings_train, by = c('zip_code', 'item_id')) %>%
  left_join(occupation_mean_ratings_train, by = c('occupation', 'item_id')) %>%
  mutate(item_id = factor(item_id))
rm(occupation_mean_ratings_train,zipcode_mean_ratings_train) # Clean
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
# Decision: More than 70% missing values matches between movies and zip_code => Drop zip_code_mean_rating
############
# Check complete rows with no NA
sum(complete.cases(train)) # 76060 / 80523 = 94.5%
sum(complete.cases(test))  # 16154 / 19477 = 82.9%
colSums(sapply(train, is.na))
colSums(sapply(test, is.na))



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