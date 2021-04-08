##### R Task

library(quanteda)
library(rtweet)
get_token() 

## Pick your favorite celebrity who has a Twitter account. 

######### @elonmusk

## find the most recent tweet the celebrited liked
musk <- get_favorites("elonmusk", n = 1)
musk

##Download their 500 most recent tweets. 
musktm<- get_timelines(c("elonmusk"), n = 500)
View(musktm)
#Calculate which one got the most ``likes"

######### "You can now buy a Tesla with Bitcoin" w/ 902346 likes 

### Create a DFM from the text of these tweets
corpus <- corpus(musktm)
?dfm
dfm <- dfm(corpus)
dfm

### After removing stopwords, what word did the celebrity tweet most often?

dfm <- dfm(corpus, remove = stopwords("english"), tolower=TRUE, remove_punct = TRUE)
dfm

?topfeatures
topfeatures(dfm)
#########  'am' was word did the celebrity tweet most often w/ 87 uses 