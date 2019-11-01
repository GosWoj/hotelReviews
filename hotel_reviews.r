library(tm)
library(tidyverse)
library(xml2)
library(selectr)
library(rvest)
library(stringr)
library(RMySQL)
library(e1071)
library(randomForest)
library(tm)
library(qdap)
library(Perc)
library(caret)
library(e1071)
library(pROC)
library(odbc)
#library(jsonlite)
#library(rebus)
#install.packages("randomForest")
#install.packages("qdap")
#install.packages("pROC")
#install.packages("odbc")

#Base reviws were downloaded from:
# https://www.kaggle.com/jiashenliu/515k-hotel-reviews-data-in-europe 

baseReviews <- read.csv("Hotel_Reviews.csv", head=TRUE, sep=",")

dim(baseReviews)
str(baseReviews)

#randomly deleting half of the data
#remove 70% of data
lessReviews <- baseReviews[-sample(1:nrow(baseReviews), 0.70*nrow(baseReviews)),]
str(lessReviews)

#separating data into positive and negative
#based on Reviewer_Score
#library(tidyverse)
#postiveReviews <- lessReviews %>% filter(lessReviews$Reviewer_Score>=5)
#negativeReviews <- lessReviews %>% filter(lessReviews$Reviewer_Score<5)

#seperating good reviews and negative reviews
postiveReviews <- data.frame(label= "Positive", Review = lessReviews$Positive_Review)
negativeReviews <- data.frame(label = "Negative", Review = lessReviews$Negative_Review)

#getting rid of "No positive" and "No negative"
postiveReviews <- postiveReviews[!grepl('No Positive', postiveReviews$Review), ]
negativeReviews <- negativeReviews[!grepl('No Negative', negativeReviews$Review), ]

#deleting PosRev and NegRev and adding labeled reviews
#lessReviews <- subset(lessReviews, select=-c(Positive_Review, Negative_Review))

#creating dataframe containing just reviews
justReviews <- rbind(postiveReviews, negativeReviews)

#deleting empty strings
justReviews <- subset(justReviews, Review!=" ")

#webscrapping reviews

#url <- read_html("https://www.tripadvisor.com/Hotel_Review-g186338-d3164384-Reviews-Park_Grand_London_Hyde_Park-London_England.html")
#html_nodes indentifies HTML wrappers
#scrapping review
#webReview <- html_node(url, ".hotels-review-list-parts-ExpandableReview__reviewText--3oMkH")
#webReviewz <- html_text(webReview)
#head(webReviewz)
#dfwebReview1 <- data.frame(label = "Positive", Review = webReviewz)
  
#booking POSITIVE1
url1 <- read_html("https://www.booking.com/reviews/gb/hotel/milestoneredcarnationhotels.en-gb.html?label=gen173nr-1DCA0oUEIlcGxhemEtb24tdGhlLXJpdmVyLWNsdWItYW5kLXJlc2lkZW5jZUgJWARoLIgBAZgBCbgBF8gBD9gBA-gBAfgBAogCAagCA7gClK7B7QXAAgE&sid=59f9a72fc74b404992f547c56050aea9&r_lang=en&customer_type=total&order=score_desc")
webReview1 <- html_node(url1, '.review_pos')
textReview1 <- html_text(webReview1)
head(textReview1)
dfwebReview1 <- data.frame(label = "Positive", Review = textReview1)

#booking NEGATIVE2
url2 <- read_html("https://www.booking.com/reviews/pt/hotel/turim-marques.en-gb.html?label=gen173nr-1DCA0oUEIlcGxhemEtb24tdGhlLXJpdmVyLWNsdWItYW5kLXJlc2lkZW5jZUgJWARoLIgBAZgBCbgBF8gBD9gBA-gBAfgBAogCAagCA7gClK7B7QXAAgE&sid=59f9a72fc74b404992f547c56050aea9&r_lang=en&customer_type=total&order=score_asc")
webReview2 <- html_node(url2, '.review_neg')
textReview2 <- html_text(webReview2)
head(textReview2)
dfwebReview2 <- data.frame(label = "Negative", Review = textReview2)

#booking POSITIVE3
url3 <- read_html("https://www.booking.com/reviews/nz/hotel/te-moenga-lodge.en-gb.html?label=gen173nr-1DCA0oUEIlcGxhemEtb24tdGhlLXJpdmVyLWNsdWItYW5kLXJlc2lkZW5jZUgJWARoLIgBAZgBCbgBF8gBD9gBA-gBAfgBAogCAagCA7gClK7B7QXAAgE&sid=59f9a72fc74b404992f547c56050aea9&r_lang=en&customer_type=total&order=score_desc")
webReview3 <- html_node(url3, '.review_pos')
textReview3 <- html_text(webReview3)
head(textReview3)
dfwebReview3 <- data.frame(label = "Positive", Review = textReview3)

#booking NEGATIVE4
url4 <- read_html("https://www.booking.com/reviews/gb/hotel/journeys-kings-cross-st-pancras.en-gb.html?label=gen173nr-1DCA0oUEIlcGxhemEtb24tdGhlLXJpdmVyLWNsdWItYW5kLXJlc2lkZW5jZUgJWARoLIgBAZgBCbgBF8gBD9gBA-gBAfgBAogCAagCA7gClK7B7QXAAgE&sid=59f9a72fc74b404992f547c56050aea9&r_lang=en&customer_type=total&order=score_asc")
webReview4 <- html_node(url4, '.review_neg')
textReview4 <- html_text(webReview4)
head(textReview4)
dfwebReview4 <- data.frame(label = "Negative", Review = textReview4)

#booking POSITIVE5
url5 <- read_html("https://www.booking.com/reviews/bq/hotel/one-ocean-boutique-apartments-bonaire.en-gb.html?label=gen173nr-1DCA0oUEIlcGxhemEtb24tdGhlLXJpdmVyLWNsdWItYW5kLXJlc2lkZW5jZUgJWARoLIgBAZgBCbgBF8gBD9gBA-gBAfgBAogCAagCA7gClK7B7QXAAgE&sid=59f9a72fc74b404992f547c56050aea9&r_lang=en&customer_type=total&order=score_desc")
webReview5 <- html_node(url5, '.review_pos')
textReview5 <- html_text(webReview5)
head(textReview5)
dfwebReview5 <- data.frame(label = "Positive", Review = textReview5)

#booking NEGATIVE6
url6 <- read_html("https://www.booking.com/reviews/es/hotel/aristol.en-gb.html?label=gen173nr-1DCA0oUEIlcGxhemEtb24tdGhlLXJpdmVyLWNsdWItYW5kLXJlc2lkZW5jZUgJWARoLIgBAZgBCbgBF8gBD9gBA-gBAfgBAogCAagCA7gClK7B7QXAAgE&sid=59f9a72fc74b404992f547c56050aea9&r_lang=en&customer_type=total&order=score_asc")
webReview6 <- html_node(url6, '.review_neg')
textReview6 <- html_text(webReview6)
head(textReview6)
dfwebReview6 <- data.frame(label = "Negative", Review = textReview6)

#booking POSITIVE7
url7 <- read_html("https://www.booking.com/reviews/cn/hotel/li-jiang-an-zhu-hong-hui-guan-lijiang.en-gb.html?label=gen173nr-1DCA0oUEIlcGxhemEtb24tdGhlLXJpdmVyLWNsdWItYW5kLXJlc2lkZW5jZUgJWARoLIgBAZgBCbgBF8gBD9gBA-gBAfgBAogCAagCA7gClK7B7QXAAgE&sid=59f9a72fc74b404992f547c56050aea9&r_lang=en&customer_type=total&order=score_desc")
webReview7 <- html_node(url7, '.review_pos')
textReview7 <- html_text(webReview7)
head(textReview7)
dfwebReview7 <- data.frame(label = "Positive", Review = textReview7)

#booking NEGATIVE8
url8 <- read_html("https://www.booking.com/reviews/ca/hotel/strathcona.en-gb.html?label=gen173nr-1DCA0oUEIlcGxhemEtb24tdGhlLXJpdmVyLWNsdWItYW5kLXJlc2lkZW5jZUgJWARoLIgBAZgBCbgBF8gBD9gBA-gBAfgBAogCAagCA7gClK7B7QXAAgE&sid=59f9a72fc74b404992f547c56050aea9&r_lang=en&customer_type=total&order=score_asc")
webReview8 <- html_node(url8, '.review_neg')
textReview8 <- html_text(webReview8)
head(textReview8)
dfwebReview8 <- data.frame(label = "Negative", Review = textReview8)

#booking POSITIVE9
url9 <- read_html("https://www.booking.com/reviews/pl/hotel/blueberry-apartment.en-gb.html?label=gen173nr-1DCA0oUEIlcGxhemEtb24tdGhlLXJpdmVyLWNsdWItYW5kLXJlc2lkZW5jZUgJWARoLIgBAZgBCbgBF8gBD9gBA-gBAfgBAogCAagCA7gClK7B7QXAAgE&sid=59f9a72fc74b404992f547c56050aea9&r_lang=en&customer_type=total&order=score_desc")
webReview9 <- html_node(url9, '.review_pos')
textReview9 <- html_text(webReview9)
head(textReview9)
dfwebReview9 <- data.frame(label = "Positive", Review = textReview9)

#booking NEGATIVE10
url10 <- read_html("https://www.booking.com/reviews/br/hotel/royalty-inn-business-hotelaria-ltda.en-gb.html?label=gen173nr-1DCA0oUEIlcGxhemEtb24tdGhlLXJpdmVyLWNsdWItYW5kLXJlc2lkZW5jZUgJWARoLIgBAZgBCbgBF8gBD9gBA-gBAfgBAogCAagCA7gClK7B7QXAAgE&sid=59f9a72fc74b404992f547c56050aea9&r_lang=en&customer_type=total&order=score_asc")
webReview10 <- html_node(url10, '.review_neg')
textReview10 <- html_text(webReview10)
head(textReview10)
dfwebReview10 <- data.frame(label = "Negative", Review = textReview10)

#Handwritten Reviews
dfhandReview1 <- data.frame(label = "Positive", Review = "Great hotel. Amazing staff. I will come back")
dfhandReview2 <- data.frame(label = "Negative", Review = "Never again. I will never come back. Everything was awful.")
dfhandReview3 <- data.frame(label = "Positive", Review = "It was okay. Nothing to complain about. Could be better.")

#Test review
testReview1 <- data.frame(label="Positive", Review = "I wish we'd never left this beautiful spot. Wendy's house was spectacular we loved every minute there We were a group of 12 people including an infant and there was plenty of space for all of us The views from the terrace were beautiful short walk to the beach and the house was equipped with everything we could possibly need Wendy was so responsive and gracious we felt so spoiled. Thank you for everything Wendy short walk to the beach and the house was equipped with everything we could possibly need. Wendy was so responsive and gracious")
corpusVector1 <- VCorpus(VectorSource(testReview1$Review))

system.time (
  corpusClean <- corpusVector1 %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwordsTotal) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(stripWhitespace)
  #tm_map(PlainTextDocument)
)

#getting all reviews into one data frame
allReviews <- rbind(justReviews, dfwebReview1, dfwebReview2, dfwebReview3, dfwebReview4, dfwebReview5, dfwebReview6, dfwebReview7, dfwebReview8, dfwebReview9, dfwebReview10, dfhandReview1, dfhandReview2, dfhandReview3)

#connectin to mySQL
connection <- dbConnect(MySQL(), user = "root", password = rstudioapi::askForPassword("Database password"), host = "127.0.0.1", dbname = "hotelreviews")

dbWriteTable(connection, "hotelreview", allReviews, row.names = FALSE, overwrite = TRUE)
connection %>% dbGetQuery("SELECT * FROM hotelreviews.hotelreview WHERE label='Negative';")

#calling stored procedure 
dbGetQuery(connection, "CALL GetPositiveReviews();")

allReviews <- dbGetQuery(connection, "CALL GetAllReviews();")

#cleaning
#chaning reviews into vector
#vectorReview <- as.vector(allReviews$Review)

#randomizing dataset
glimpse(allReviews)
set.seed(12)
allReviews <- allReviews[sample(nrow(allReviews)), ]
allReviews <- allReviews[sample(nrow(allReviews)), ]
glimpse(allReviews)

allReviews <- allReviews[1:60000, ]

#converting label to factor
allReviews$label <- as.factor(allReviews$label)

#making corpus
#corpusVector <- VCorpus(VectorSource(vectorReview))
corpusVector <- VCorpus(VectorSource(allReviews$Review))

#checking first element
as.character(corpusVector[[1]])

#stopwords - often but no contribution
#stopWords <- c('hotel', 'location', 'na', 'hotels', 'trip', 'reception', 'staff', 'room', 'rooms', 'shower', 'kitchen', 'bedroom', 'bed', 'city', 'sheets')
stopWords <- c('hotel', 'location', 'na', 'hotels', 'trip', 'nothing', 'everything')

#general and custom
stopwordsTotal <- unique (c(stopWords, stopwords('SMART')))

#wrap corpusClean in system.time() to see how long it took
system.time (
corpusClean <- corpusVector %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwordsTotal) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)
  #tm_map(PlainTextDocument)
)

as.character(corpusClean[[1]])

#mergining clean data with labels
#dfcorpusClean <- data.frame(Review=unlist(sapply(corpusClean, `[`, "content")), 
                            #stringsAsFactors=F)
dfcorpusClean <- data.frame(Review=sapply(corpusClean, content), 
                        stringsAsFactors=F)
#attributes(corpusClean)
#dfcorpusClean <- data.frame(text = get('content', corpusClean))
cleanedReviews <- data.frame(label = allReviews$label, Review = dfcorpusClean)
cleanedReviews <- subset(cleanedReviews, Review!=" ")
cleanedReviews$label <- as.factor(cleanedReviews$label)
cleanedReviews <- cleanedReviews[1:40000, ]

#putting corpusClean into database
connection <- dbConnect(MySQL(), user = "root", password =rstudioapi::askForPassword("Database password"), host = "127.0.0.1", dbname = "hotelreviews")
dbWriteTable(connection, "cleanreview", cleanReviews, row.names = FALSE, overwrite = TRUE)

cleanReviews <- connection %>% dbGetQuery("SELECT * FROM hotelreviews.cleanreview;")

#tokenization
reviewMatrix <- DocumentTermMatrix(corpusClean)
inspect(reviewMatrix[40:50, 10:15])

#creatin training and test data
#creating partitions of dataframe, corpus and matrix

allReviews.train <- allReviews[1:35000, ]
allReviews.test <- allReviews[35001:40000, ]

reviewMatrix.train <- reviewMatrix[1:35000, ]
reviewMatrix.test <- reviewMatrix[35001:40000, ]

corpusClean.train <- corpusClean[1:35000]
corpusClean.test <- corpusClean[35001:40000]

dim(reviewMatrix.train)

#ignoring words that appear in less than 5 reviews
fivefreq <- findFreqTerms(reviewMatrix.train, 5)
length(fivefreq)

#using only 5 most frequent words
reviewMatrix.train.nb <- DocumentTermMatrix(corpusClean.train, control = list(dictionary=fivefreq))
dim(reviewMatrix.train.nb)

reviewMatrix.test.nb <- DocumentTermMatrix(corpusClean.test, control = list(dictionary=fivefreq))
dim(reviewMatrix.test.nb)

#Naive Bayes
#function to convert the word frequencies to yes and no
convert_count <- function(x) {
  y <- ifelse(x>0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

#applying above function to training and testing dtms
#memory.limit(size=86000)
trainNB <- apply(reviewMatrix.train.nb, 2, convert_count)
testNB <- apply(reviewMatrix.test.nb, 2, convert_count)

#training Naive Bayes
system.time(
  classifier <- naiveBayes(trainNB, allReviews.train$label, laplace = 1)
)

#using NB clasifier to make preictions on the test set
system.time(
  prediction <- predict(classifier, newdata=testNB)
)

#truth tabel
#have to be the same length
length(prediction)
length(allReviews.test$label)
table("Predictions" = prediction, "Actual" = allReviews.test$label)

#preparing confusion matrix
conf.mat <- confusionMatrix(prediction, allReviews.test$label)

conf.mat
conf.mat$overall['Accuracy']

#SVM model
corpusSparse <- removeSparseTerms(reviewMatrix, 0.90)
svmMatrix <- as.data.frame(as.matrix(corpusSparse))
svmMatrix$label <- allReviews$label
reviewMatrix.train1 <- svmMatrix[1:10000, ]
reviewMatrix.test1 <- svmMatrix[10001:13000, ]

svm_model <- svm(label ~ ., data = reviewMatrix.train1, kernel='linear', type = "C")
summary(svm_model)

pred <- predict(svm_model, reviewMatrix.test1, type = "response")

length(pred)
length(reviewMatrix.test1$label)
table(pred, reviewMatrix.test1$label)
#conf.mat1 <- confusionMatrix(pred, reviewMatrix.test1$label)
#conf.mat1
confusionMatrix(table(pred, reviewMatrix.test1$label))


#Linear Regression Model
svmMatrix$label <- as.factor(svmMatrix$label)
cleanedReviews.train <- svmMatrix[1:30000, ]
cleanedReviews.test <- svmMatrix[30001:35000, ]

glm_model <- glm(label ~., cleanedReviews.train, family=binomial)
#glm_model <- glm(label ~., cleanedReviews.train, family=binomial, trControl=trainControl(method="none"))
options(max.print=10)
summary(glm_model)
anova(glm_model, test = 'Chisq')
#predictglm <- ifelse(predict.glm(glm_model, cleanedReviews.test, type = "response")>0.5,1,0)
glm_model$xlevels$Review <- union(glm_model$xlevels$Review, levels(cleanedReviews.test$Review))

predictglm <- predict.glm(glm_model, cleanedReviews.test, na.action = na.pass, type = "response")
table(predictglm>.5, cleanedReviews.test$label)
table(predictglm, cleanedReviews.test$label)
summary(predictglm)
#The cases where our model performed properly are given by the diagonal
#big number +
#(30+69)/nrow(cleanedReviews.test)
#(106+111)/nrow(cleanedReviews.test)
(1907+1640)/nrow(cleanedReviews.test)

#calcualting area under the ROC curve
roc.curve <- roc(cleanedReviews.test$label, predictglm, ci=T)

#plotting ROC curve
plot(roc.curve)

fitted.results <- ifelse(predictglm > 0.5,1,0)
auc(cleanedReviews.test$label, predictglm)
