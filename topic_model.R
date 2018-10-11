#Attempt at topic modeling for kaggle blog competition
library(tm)
library(topicmodels)
library(SnowballC)
library(dplyr)#to read in large file
library(stringr)
library(plyr)

setwd('C:\\Users\\x1\\Documents\\SYS 6018\\blog')
train <- readr::read_csv('train.csv')
train <- transform(train, total= str_count(text))
#count word length of post
train <- transform(train,work = str_count(text,'work'))
#count # of times post says work

#makes df with all texts from each user into one variable
df <- aggregate(text ~ user.id, data = train, toString)
names(df) <- c('doc_id','text')

#creates another data frame 1 obs per user includes most common topic and total number of posts
train1 <- ddply(train,.(user.id),summarize,age = mean(age),sum_work=sum(work),number=length(user.id),topic={
  tt <- table(topic)
  names(tt)[which.max(tt)]
})

#merge so total includes all metadata such as number of posts and mode topic as well as full
#copies of blogs in one long string
total <- merge(train1,df)
names(total) <- c('doc_id',names(total)[-1])
total <- total[order(total$age),]
#to load in second time

load('C:\\Users\\x1\\Documents\\SYS 6018\\blog\\train_1_per_user.RData')
rm(df)
###########
#start of topic modeling
blogs <- VCorpus(DataframeSource(total_train[c(1:50,6000:6050,12830:12880),c(1,5)]))#currently takes 50 from young kids 
#50 from 23ish age and 50 from 'old' age

blog.clean = tm_map(blogs, stripWhitespace)                          # remove extra whitespace
blog.clean = tm_map(blog.clean, removeNumbers)                      # remove numbers
blog.clean = tm_map(blog.clean, removePunctuation)                  # remove punctuation
blog.clean = tm_map(blog.clean, content_transformer(tolower))       # ignore case
blog.clean = tm_map(blog.clean, removeWords, stopwords("english"))  # remove stop words
blog.clean = tm_map(blog.clean, stemDocument)                       # stem all words
blog.clean.tf = DocumentTermMatrix(blog.clean, control = list(weighting = weightTf))

# remove empty documents
row.sums = apply(blog.clean.tf, 1, sum)
blogs = blogs[row.sums > 0]
blog.clean.tf = blog.clean.tf[row.sums > 0,]

# train topic model with 3 topics
topic.model = LDA(blog.clean.tf, 3)

#Tried varying number of topics many times 3 outperformed 

# look at the top 10 words within the first 3 topics
terms(topic.model, 10)[,1:3]

pratice_model_training <- total[c(1:50,6000:6050,12830:12880),]
probs <- matrix(nrow=legnth(total$doc_id),ncol=3)
for(i in 3799:3800){
clean_text <- gsub("[^[:alnum:] ]", "", total$text[i])
clean_text <- stringi::stri_trans_general(clean_text, "latin-ascii")
#Removes all non alphabet characters that cause problems


testing.documents = data.frame('text' = clean_text,'doc_id' = '1')
testing.corpus = VCorpus(DataframeSource(testing.documents))
testing.corpus = tm_map(testing.corpus, stripWhitespace)                    # remove extra whitespace
testing.corpus = tm_map(testing.corpus, removeNumbers)                      # remove numbers
testing.corpus = tm_map(testing.corpus, removePunctuation)                  # remove punctuation
testing.corpus = tm_map(testing.corpus, content_transformer(tolower))       # ignore case
testing.corpus = tm_map(testing.corpus, removeWords, stopwords("english"))  # remove stop words
testing.corpus = tm_map(testing.corpus, stemDocument)                       # stem all words
testing.corpus.tf = DocumentTermMatrix(testing.corpus, control = list(weighting = weightTf))
inferred_probabilities = posterior(topic.model, testing.corpus.tf)
probs[i,] <- inferred_probabilities$topics
print(i)
}
#made it to 5122
#327 failed, 3799 failed 3885,3959,
total <- cbind(total,probs)
saveRDS(total,'total_with_probs.rds')
total_train <-  readRDS('total_with_probs.rds')
names(total_train) <- c(names(total_train)[-c(6,7,8)],'p1','p2','p3')

sum(is.na(total_train$sum_work))

sum_work <- train1$sum_work
total_train <- cbind(total_train,sum_work)
total_train$sum_work[which(is.na(total_train$sum_work))] <- 0 
lm.mod_total<- lm(age~as.factor(topic)+number+p1+p2+sum_work,data = total_train)
summary(lm.mod_total)
#Adjusted R-squared:  0.5036 for 3 topics 10.4 error

#Cross Validation

library(boot)
set.seed(1995)
glm.fit=glm(age~as.factor(topic)+number+p1+p2,data = total_train)
cv.err=cv.glm(na.omit(total_train),glm.fit)
cv.err
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(age~as.factor(topic)+number+p1+p2+sum_work,data = total_train)
  cv.error.10[i]=cv.glm(na.omit(total_train),glm.fit,K=10)$delta
  print(i)
}
cv.error.10
#44.84147 44.86881 44.90278 44.85437 44.84654 44.85225 44.82343 44.82200 44.88971 44.90306 w/o sum_work
#44.85597 44.92927 44.86434 44.88540 44.92996 44.90370 44.90964 44.89295 44.86599 44.88892 w/ sum work

##############################
#Testing time
##############################
setwd('C:\\Users\\x1\\Documents\\SYS 6018\\blog')
test <- readr::read_csv('test.csv')

#makes df with all texts from each user into one variable
df <- aggregate(text ~ user.id, data = test, toString)
names(df) <- c('doc_id','text')

#creates another data frame 1 obs per user includes most common topic and total number of posts
test1 <- ddply(test,.(user.id),summarize,number=length(user.id),topic={
  tt <- table(topic)
  names(tt)[which.max(tt)]
})
#merge so total includes all metadata such as number of posts and mode topic as well as full
#copies of blogs in one long string
total_test <- merge(test1,df)
names(total_test) <- c('doc_id',names(total_test)[-1])
probs <- matrix(nrow = 6440,ncol = 3)

for(i in 1:6440){
  clean_text <- gsub("[^[:alnum:] ]", "", total_test$text[i])
  clean_text <- stringi::stri_trans_general(clean_text, "latin-ascii")
  #Removes all non alphabet characters that cause problems
  
  
  testing.documents = data.frame('text' = clean_text,'doc_id' = '1')
  testing.corpus = VCorpus(DataframeSource(testing.documents))
  testing.corpus = tm_map(testing.corpus, stripWhitespace)                    # remove extra whitespace
  testing.corpus = tm_map(testing.corpus, removeNumbers)                      # remove numbers
  testing.corpus = tm_map(testing.corpus, removePunctuation)                  # remove punctuation
  testing.corpus = tm_map(testing.corpus, content_transformer(tolower))       # ignore case
  testing.corpus = tm_map(testing.corpus, removeWords, stopwords("english"))  # remove stop words
  testing.corpus = tm_map(testing.corpus, stemDocument)                       # stem all words
  testing.corpus.tf = DocumentTermMatrix(testing.corpus, control = list(weighting = weightTf))
  inferred_probabilities = posterior(topic.model, testing.corpus.tf)
  probs[i,] <- inferred_probabilities$topics
  print(i)
}
total_test <- cbind(total_test,probs)
saveRDS(total_test,'total_test_with_probs.rds')
#total_train <-  readRDS('total_with_probs.rds')
names(total_test) <- c(names(total_test)[-c(5,6,7)],'p1','p2','p3')
predictions <- predict(lm.mod_total,total_test)
pred_df <- data.frame('user.id'=total_test$doc_id,'age'=predictions)
write.csv(pred_df,'first_topic_lm_predictions.csv')

