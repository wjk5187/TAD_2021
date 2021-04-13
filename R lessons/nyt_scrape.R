
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)


##code adapted from Heather Geiger

setwd("C/Users/wadekoontz/Documents/GitHub/TAD_2021/R lessons")


NYTIMES_KEY <- ("MkVIvOPgHbkTEuWkazAwf0OmuYrAjBJ4")

term <- "facebook"
begin_date <- "20200101"
end_date <- "20200401"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

initialQuery <- fromJSON(baseurl)

pages_2020 <- vector("list",length=5)

for(i in 0:4){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  pages_2020[[i+1]] <- nytSearch 
  Sys.sleep(10) #I was getting errors more often when I waited only 1 second between calls. 5 seconds seems to work better.
}
facebook_2020_articles <- rbind_pages(pages_2020)

View(facebook_2020_articles)

term <- "facebook"
begin_date <- "20210101"
end_date <- "20210401"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

initialQuery <- fromJSON(baseurl)

pages_2021 <- vector("list",length=5)

for(i in 0:5){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame()
  pages_2021[[i+1]] <- nytSearch
  Sys.sleep(10)
}
facebook_2021_articles <- rbind_pages(pages_2021)


#############################################
#############################################
#############################################
#############################################
#############################################
#############################################
#############################################
#############################################

#####in-class practice: 


### save the results of two different queries from the date range jan 1 2021 - APril 1 2021

term <- "Pennsylvania"
begin_date <- "20200101"
end_date <- "20200401"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

initialQuery <- fromJSON(baseurl)

pages_2020 <- vector("list",length=5)

for(i in 0:4){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  pages_2020[[i+1]] <- nytSearch 
  Sys.sleep(10) #I was getting errors more often when I waited only 1 second between calls. 5 seconds seems to work better.
}
penn_2020_articles <- rbind_pages(pages_2020)

View(penn_2020_articles)


#############################################


term <- "Japan"
begin_date <- "20210101"
end_date <- "20210401"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

initialQuery <- fromJSON(baseurl)

pages_2020 <- vector("list",length=5)

for(i in 0:4){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  pages_2020[[i+1]] <- nytSearch 
  Sys.sleep(10) #I was getting errors more often when I waited only 1 second between calls. 5 seconds seems to work better.
}
japan_2020_articles <- rbind_pages(pages_2020)

View(japan_2020_articles)


### calculate the proportion of the headlines from each search term assigned to a given section name
?table
table(penn_2020_articles$response.docs.section_name)
arts <- 4/50
biz <- 5/50
style <- 7/50
food <- 1/50
health <- 1/50
ny <- 3/50
op <- 2/50
podcast <- 1/50
realestate <- 2/50
sports <- 1/50
Tmag <- 1/50
Upshot <- 1/50
US <- 20/50
world <- 1/50

table(japan_2020_articles$response.docs.section_name)
arts <- 3/50
books <- 1/50
briefing <- 1/50
biz <- 7/50
food <- 1/50
mag <- 1/50
op <- 3/50
sports <- 1/50
theater <- 2/50
ins <- 1/50
US <- 4/50
well <- 1/50
world <- 24/50

## create a combined dfm with the text of all of the lead paragraphs
library(quanteda)
?corpus
dfm(penn_2020_articles$response.docs.abstract)
dfm(japan_2020_articles$response.docs.abstract)

## calculate the average Flesch Reading Ease score (hint: use code form descriptive_2.R) for the lead paragraphs from each search term. Which is higher?

penn <- textstat_readability(penn_2020_articles$response.docs.abstract, "Flesch")

mean(penn$Flesch)

japan <- textstat_readability(japan_2020_articles$response.docs.abstract, "Flesch")

mean(japan$Flesch)

#pennsylvania associated search term is higher 

