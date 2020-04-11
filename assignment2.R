## 1. Get the data
#Loading the packages
library(readr)
library(textclean)
library(tibble)
library(kableExtra)
library(tidytext)
library(quanteda)
library(downloader)
library(dplyr)
library(knitr)

#Download the data
fileURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download(fileURL, dest="dataset.zip", mode="wb") 
outDir<-"./data"
unzip("./dataset.zip",exdir=outDir)

#Read the data
data_blogs <- read_lines("./data/final/en_US/en_US.blogs.txt") 
data_twitter <- read_lines("./data/final/en_US/en_US.twitter.txt")
data_news <- read_lines("./data/final/en_US/en_US.news.txt")

## 2. Some basic exploration of the data
#Make tokenizations of text
blogs <- tibble(line = 1:length(data_blogs), text = data_blogs) %>% 
        unnest_tokens(word, text)
twitter <- tibble(line = 1:length(data_twitter), text = data_twitter) %>% 
        unnest_tokens(word, text)
news <- tibble(line = 1:length(data_news), text = data_news) %>% 
        unnest_tokens(word, text)

texts <- list(data_blogs, data_twitter, data_news)
texts_tokens <- list(blogs, twitter, news)
files_info <- matrix(0, nrow = 3, ncol = 4, dimnames = list(c("blogs", "news", "twitter"),c("Size of file (Mb)", "Total words", "Total different words",  "Number of texts in file")))
for (i in 1:3){
        files_info[i,1] <- format(object.size(texts[[i]]), units = "Mb")
        files_info[i,2] <- length(texts_tokens[[i]]$word)
        files_info[i,3] <- length(unique(texts_tokens[[i]]$word))
        files_info[i,4] <- length(texts[[i]])
}

#Print the summary in a nice format
kable(files_info) %>% 
        kable_styling()


#Make the memory empty to provide some space for the rest of the calculations
rm(list=setdiff(ls(), c("data_blogs", "data_twitter", "data_news")))
gc()

## 3. Make samples, make corpus and tokens from this samples
#Make samples
set.seed(12345)
blogs_sample <- sample(data_blogs, size = length(data_blogs) * .01)
twitter_sample <- sample(data_twitter, size = length(data_twitter) * .01)
news_sample <- sample(data_news, size = length(data_news) * .01)

#Make a dataframe of the datasets, made of the text and an index, and give the dataframe rownames
blogs_sample <- data.frame(id = 1:length(blogs_sample), text = blogs_sample, type = "blogs", stringsAsFactors = FALSE)
twitter_sample <- data.frame(id = 1:length(twitter_sample), text = twitter_sample, type = "twitter", stringsAsFactors = FALSE)
news_sample <- data.frame(id = 1:length(news_sample), text = news_sample, type = "news", stringsAsFactors = FALSE)

row.names(blogs_sample) <- paste("blogs", blogs_sample$id, sep = "_")
row.names(twitter_sample) <- paste("twitter", twitter_sample$id, sep = "_")
row.names(news_sample) <- paste("news", news_sample$id, sep = "_")

#Make corpus from the dataset
blogs_corpus <- corpus(blogs_sample)
twitter_corpus <- corpus(twitter_sample)
news_corpus <- corpus(news_sample)

#Make one corpus out of the 3
corp <- blogs_corpus + twitter_corpus + news_corpus
length(corp$documents$texts)

#Creating tokens
blogs.tokens <- tokens(blogs_corpus, remove_punct = TRUE, remove_numbers = TRUE) %>% 
        tokens_select(stopwords('english'),selection='remove') %>% 
        tokens_wordstem() %>% 
        tokens_tolower()

twitter.tokens <- tokens(twitter_corpus, remove_punct = TRUE, remove_numbers = TRUE) %>% 
        tokens_select(stopwords('english'),selection='remove') %>% 
        tokens_wordstem() %>% 
        tokens_tolower()

news.tokens <- tokens(news_corpus, remove_punct = TRUE, remove_numbers = TRUE) %>% 
        tokens_select(stopwords('english'),selection='remove') %>% 
        tokens_wordstem() %>% 
        tokens_tolower()

corp.tokens <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE) %>% 
        tokens_select(stopwords('english'),selection='remove') %>% 
        tokens_wordstem() %>% 
        tokens_tolower()

#Creating document-feature-matrices
dfm_blogs <- dfm(blogs.tokens)
dfm_twitter <- dfm(twitter.tokens)
dfm_news <- dfm(news.tokens)
dfm_corp <- dfm(corp.tokens)

my_dfm <- rbind(dfm_blogs, dfm_twitter, dfm_news)

## 4. Analysis with plots
par(mfrow= c(3,1))
barplot(topfeatures(dfm_blogs, 10), col = "blue", main = "10 most frequent words in blogs", ylab = "count")
barplot(topfeatures(dfm_twitter, 10), col = "blue", main = "10 most frequent words in tweets", ylab = "count")
barplot(topfeatures(dfm_twitter, 10), col = "blue", main = "10 most frequent words in news", ylab = "count")

#Wordcloud comparison between blogs-twitter-news
dfm_corp %>%
        dfm(groups = "type") %>%
        dfm_trim(min_termfreq = 5, verbose = FALSE) %>%
        textplot_wordcloud(comparison = TRUE, color = c("navy", "firebrick", "darkgreen"))

## 5. n-grams
#Create bigrams
bigrams <- tokens(corp, remove_numbers = TRUE, what = "sentence") %>% 
        tokens_tolower() %>% 
        as.character() %>% 
        tokens(ngrams = 2, remove_punct = TRUE, remove_numbers = TRUE) %>% 
        as.character()

#Create trigrams
trigrams <- tokens(corp, remove_numbers = TRUE, what = "sentence") %>% 
        tokens_tolower() %>% 
        as.character() %>% 
        tokens(ngrams = 3, remove_punct = TRUE, remove_numbers = TRUE) %>% 
        as.character()

#Make a dfm from the bigrams and trigrams
dfm_bigram <- dfm(bigrams)
dfm_trigram <- dfm(trigrams)

bigram_df <- colSums(dfm_bigram) %>% 
        sort(decreasing = TRUE) 
head(bigram_df)
trigram_df <- colSums(dfm_trigram) %>% 
        sort(decreasing = TRUE) 
head(trigram_df)

