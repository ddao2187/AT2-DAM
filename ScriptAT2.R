##### Record
# Run full RMSE = 0.6896341
# After replace NA = 0.6887705
# With drop genres = 0.6896341 (bad)


##### EDA
rm(p1,p2,p3,p4)
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
             top="TRAIN")
rm(p1,p2,p3,p4)
p1 <- ggplot(data=validation, aes(x=timestamp_year)) + 
  geom_bar(fill="blue") + 
  xlab("Movie timestamp_year") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
p2 <- ggplot(data=validation, aes(x=release_year)) + 
  geom_bar(fill="blue") + 
  xlab("Movie release_year") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
p3 <- ggplot(data=validation, aes(x=timestamp_month)) + 
  geom_bar(fill="blue") + 
  xlab("Movie timestamp_month") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
p4 <- ggplot(data=validation, aes(x=release_month)) + 
  geom_bar(fill="blue") + 
  xlab("Movie release_month") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))
grid.arrange(p2, p3, p1, p4, nrow=2,
             top="VALIDATION")
rm(p1,p2,p3,p4)







scrape.NA <- scrape[rowSums(is.na(scrape)) > 37,] # Movies with empty values
filt = filter(train, item_id == 272)
filt = filter(train,release_year == 1922)
barplot(train$year_band)