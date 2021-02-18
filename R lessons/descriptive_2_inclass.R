# Descriptive practice


#1. Write two sentences. Save each as a seperate object in R. 

require(quanteda)


sent <- c(sent1 = 'A complete sentence must have, at minimum, three things: a subject, verb, and an object.',
         sent2 = 'He waded out to the boat.')

#2. Combine them into a corpus

corpus_sent<-corpus(sent)
corpus_sent

#3. Make this corpus into a dfm with all pre-processing options at their defaults.
?dfm
dfm_sent<-dfm(corpus_sent)

#4. Now save a second dfm, this time with stopwords removed.

dfm_sent2 <- dfm(corpus_sent, remove = stopwords("english"))
dfm_sent2

#5. Calculate the TTR for each of these dfms (use textstat_lexdiv). Which is higher?

textstat_lexdiv(dfm_sent)
textstat_lexdiv(dfm_sent2)

####Higher for dfm w/o stop words

#6. Calculate the Manhattan distance between the two sentences you've constructed (by hand!)

### | 0.9333 - 1 | + | 1 - 1 |
###  Manhattan distance: 0.06667
 