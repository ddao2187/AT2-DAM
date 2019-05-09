library(nnet)
rm(regres,train_n,validation_n,testset,trainset)
#trainset_indices <- sample(seq_len(nrow(train)), size = 10000) # Reduce the data size
#train_n <- train[trainset_indices, ]
train_n = train
train_n = train_n[order(train_n$item_id),] # Re-order by item_id
validation_n = validation
#rm(trainset_indices)
# Temporary drop to make regression running (able to make prediction)

#train_n$item_imdb_mature_rating = NULL
#validation$item_imdb_mature_rating = NULL
# Factor rating


#train_n$rating = factor(train_n$rating)

# Replace all NA values by 0
train_n[is.na(train_n)] <- 0
validation_n[is.na(validation_n)] <- 0

train_n$age_band = NULL
train_n$item_imdb_mature_rating =NULL
train_n$zip_code = NULL
train_n$occupation = NULL
train_n$gender = NULL


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
#    train_n$user_id[i] = NA
    count = count + 1
  }
}
rm(i,dummy,count)
#train_n <- train_n[!is.na(train_n$user_id) == TRUE,]
train_n$user_id = factor(train_n$user_id)
train_n$item_id = factor(train_n$item_id)
#train_n$rating = factor(train_n$rating)
# Check
length(unique(train_n$item_id)) == length(unique(testset$item_id))
# Train the model
#regres <- multinom(rating ~ ., data=train_n[,3:ncol(train_n)], family = "gaussian")


train_n$zipcode_mean_rating = floor(train_n$zipcode_mean_rating)

regres <- glm(rating ~ item_mean_rating, data=train_n[,3:ncol(train_n)]) # Exclude item_id and user_id for regression
# Rate RMSE
testset$prediction = predict(regres, testset[,3:ncol(testset)], type = "response")
# Check if any rating > 5 (wrong)
testset$prediction = floor(testset$prediction)
which(testset$prediction > 5L)
which(testset$prediction < 0L)

testset$rating = as.numeric(testset$rating)
testset$prediction = as.numeric(testset$prediction)
rmse(testset$rating, testset$prediction)
length(which(is.na(testset$prediction) == TRUE)) / nrow(testset) # % of missing values 


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
validation_n$rating = round(validation_n$rating)
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
