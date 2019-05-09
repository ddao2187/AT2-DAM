rm(train_n,validation_n,testset,trainset)
train_n = train


train_n$age_band = NULL
train_n$item_imdb_mature_rating =NULL
train_n$zip_code = NULL
train_n$occupation = NULL
train_n$gender = NULL


train_n <- train_n[, colSums(is.na(train_n)) == 0]
#train_n$release_year = factor(train_n$release_year)
#train_n$timestamp_month = factor(train_n$timestamp_month)
#train_n$release_month = factor(train$release_month)
#########################
##### Random Forest #####
#########################
library(randomForest)

library(nnet)
#trainset_indices <- sample(seq_len(nrow(train)), size = 5000) # Reduce the data size
#train_n <- train[trainset_indices, ]
train_n = train_n[order(train_n$item_id),] # Re-order by item_id
validation_n = validation
#rm(trainset_indices)
# Temporary drop to make regression running (able to make prediction)

#train_n$item_imdb_mature_rating = NULL
#validation$item_imdb_mature_rating = NULL
# Factor rating
train_n$rating = factor(train_n$rating)

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
train_n$rating = factor(train_n$rating)
# Check
length(unique(train_n$item_id)) == length(unique(testset$item_id))
trainset=train_n




# Get index of predicted variable
#class_col_num = grep("rating",names(train_n))
##### Build random forest model
re.rf = randomForest(rating ~.,data = trainset[,3:ncol(trainset)], 
                     importance = TRUE, xtest = testset[, 4:ncol(testset)], ntree=1000)
# Model summary
summary(re.rf)
# Variables contained in model 
names(re.rf)
# Predictions for test set
test_predictions_rf <- data.frame(testset,re.rf$test$predicted)
# Accuracy for test set
mean(re.rf$test$predicted==testset$Target)
# Confusion matrix
table(re.rf$test$predicted,testset$Target)
# Quantitative measure of variable importance
importance(re.rf)
# Sorted plot of importance
varImpPlot(re.rf)