#Kaggle Competition Blog thing
library(dplyr)#to read in large file
library(stringr)
library(plyr)
library(readr)

setwd('C:\\Users\\x1\\Documents\\MSDS\\SYS 6018\\blog')
train <- read_csv('.data\\train.csv')

train$topic <- as.factor(train$topic)
#convert to factor

train <- transform(train, total= str_count(text))
#count word length of post
train <- transform(train,work = str_count(text,'work'))
#count # of times post says work

length(unique(train$user.id))
#12880
lm.mod <- lm(age ~ as.factor(topic)+work+total,data = train)
summary(lm.mod)
#longer posts and more work seem to indicate older person

#sorts by so each user only has 1 observation keeping most common topic they post about
train1 <- ddply(train,.(user.id),summarize,sum=sum(work),age = mean(age),number=length(user.id),topic={
  tt <- table(topic)
  names(tt)[which.max(tt)]
})
train1[which(is.na(train1$sum)),]$sum <- 0

lm.mod <- lm(age ~ sum + number+as.factor(topic),data = train1)
summary(lm.mod)

test <- read_csv('.data\\test.csv')
length(unique(test$user.id))
#6440
test$topic <- as.factor(test$topic)
test <- transform(test, work= str_count(text,'work'))
test1 <- ddply(test,.(user.id),summarize,sum=sum(work),number=length(user.id),topic={
  tt <- table(topic)
  names(tt)[which.max(tt)]
})
test1[which(is.na(test1$sum)),]$sum <- 0
preds <- predict(lm.mod,test1)
pred_df <- data.frame('user.id'=test1$user.id,'age'=preds)
write.csv(pred_df,'first_lm_predictions.csv')

