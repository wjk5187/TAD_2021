# Manupulating text in R

#1. Find a sentence online. Save it as a string. 

require(quanteda)

sen <- 'A complete sentence must have, at minimum, three things: a subject, verb, and an object.'
sen1.5 <- strsplit(sen, ' ')
sen1.5

#2. Select only the third word of the sentence. Save it as a new string.

sen2 <- sapply(sen2.5, function(x) x[3])
sen2

#3. Choose a letter that appears in your sentence. Use the gsub command to replace all instances of that letter with a period. 

?gsub
gsub('e', '.', sen)
