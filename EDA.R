
filt = filter(train,year_band == "1950s")

p1 <- ggplot(data=train, aes(x= rating)) + 
  geom_bar(fill="blue") + 
  xlab("Movie rating") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
p1


p2 <- ggplot(data=train, aes(x=rating, y = item_imdb_staff_votes, fill = gender)) + 
  geom_col() + 
  xlab("Rating")
p2


p3 <- ggplot(data=train, aes(x=rating, y = item_imdb_top_1000_voters_votes)) + 
  geom_bar(fill="blue") + 
  xlab("Movie item_imdb_top_1000_voters_votes") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
p4 <- ggplot(data=train, aes(x=rating, y = user_gender_item_imdb_votes)) + 
  geom_bar(fill="blue") + 
  xlab("Movie user_gender_item_imdb_votes") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
p5 <- ggplot(data=train, aes(x=rating, y = user_age_band_item_imdb_votes)) + 
  geom_bar(fill="blue") + 
  xlab("Movie user_age_band_item_imdb_votes") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
p6 <- ggplot(data=train, aes(x=rating, y = user_gender_age_band_item_imdb_votes)) + 
  geom_bar(fill="blue") + 
  xlab("Movie user_gender_age_band_item_imdb_votes") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
grid.arrange(p1, p2, p3, p4, p5, p6, nrow=3,
             top="Movie EDA")

hist(filt$item_imdb_count_ratings)

filt = filter(train,year_band == "Before 1950s")

df = filt

a = which(names(df) == "action")
b = which(names(df) == "western")
col_name = names(df)[a:b]
test = matrix(ncol=18,nrow=1)
colnames(test) = col_name
for (i in 1:b-a+1) {
  c = as.numeric(i+a-1)
  test[,i] = length(which(df[,c] == TRUE))
}

barplot(test, main="train Chart", xlab="Genres")





pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(train,2,pMiss)
apply(validation,2,pMiss)
