############# Function for the benchmark #####################

library(downloader)
library(readr)
library(tidytext)
library(data.table)
library(dplyr)
library(quanteda)
library(stringr)
library(stringi)

#################################################################
############### Loading the data ################################
#################################################################

#Download the data
fileURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download(fileURL, dest="dataset.zip", mode="wb") 
outDir<-"./data"
unzip("./dataset.zip",exdir=outDir)

#Read the data
data_blogs <- read_lines("./data/final/en_US/en_US.blogs.txt") 
data_twitter <- read_lines("./data/final/en_US/en_US.twitter.txt")
data_news <- read_lines("./data/final/en_US/en_US.news.txt")

#Make tokenizations of text
blogs <- tibble(line = 1:length(data_blogs), text = data_blogs) %>% 
        unnest_tokens(word, text)
twitter <- tibble(line = 1:length(data_twitter), text = data_twitter) %>% 
        unnest_tokens(word, text)
news <- tibble(line = 1:length(data_news), text = data_news) %>% 
        unnest_tokens(word, text)


#Make samples, 10% of the original data
set.seed(12345)
blogs_sample <- sample(data_blogs, size = length(data_blogs) * .1)
twitter_sample <- sample(data_twitter, size = length(data_twitter) * .1)
news_sample <- sample(data_news, size = length(data_news) * .1)

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

#####################################################################
############ Make the ngrams ########################################
#####################################################################



#Function to create datatable with 2 columns: ngram and the count. 
#       Input:  -corp: corpus 
#               -n: n=1 --> unigram, n=2 --> bigram, n=3 --> trigram, etc
#       Output: -dataframe with 2 columns: ngram and the count of the ngram
makedt <- function(corp, n){
        #Create ngram
        ngram <- tokens(corp, remove_numbers = TRUE, what = "sentence") %>% 
                tokens_tolower() %>% 
                as.character() %>% 
                tokens(ngrams = n, remove_punct = TRUE, remove_numbers = TRUE) %>% 
                as.character()
        dfm_ngram <- dfm(ngram)
        ngram_freq <- docfreq(dfm_ngram)
        ngram_freq_dt <- data.table(token = names(ngram_freq), count = ngram_freq) %>% 
                .[order(-count)]
        if(n!=1){
                ngram_freq_dt[, nminus1 :=strsplit(token, "_[^_]+$")][]
                ngram_freq_dt[, prediction:=sub('.*_(.*)$', '\\1', token)][]
        }
        ngram_freq_dt
}

unigrams <- makedt(corp, 1)
bigrams <- makedt(corp, 2)
trigrams <- makedt(corp, 3)


input_string <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"

Find_probs <- function(input_string, unigrams = unigrams, bigrams = bigrams, trigrams = trigrams, nr_of_outputs=1){
        input_string <- gsub(" I'd$", " i would", input_string)
        bigram <- paste(word(input_string, -2), stri_extract_last_words(input_string), sep = "_")       #takes 2 last words of the input_string
        bigram <- tolower(bigram)
        #We start with a given bigram, and try to predict the next word. 
        unigram <- str_split_fixed(bigram, "_", 2)[,2]
        
        #We divide this problem in 2 parts: for observated and for unseen trigrams: 
        #       -1: Find all observated trigrams that start with this bigram: trigrams_obs
        #       -2: calculate the probabilities of trigram_obs: trigram_obs_prob
        #       -3: calculate the probabilities of the unseen trigrams that start with this bigram. 
        #               This means: find all possible trigrams starting with this bigram (bigram + unigrams) that are
        #               not in trigram_obs
        
        
        #########################################################################
        #We will first do this for the trigrams that are observated
        #########################################################################
        
        #Function to find all trigrams that start with the given bigram
        #       Input:  -bigram: with this bigram all trigrams should start
        #               -trigrams: a dataframe with 2 columns: token and count
        
        get_obs_trigrams <- function(bigram, trigrams){
                trigrams[nminus1==bigram]
        }

        
        obs_trigrams <- get_obs_trigrams(bigram, trigrams)
        
        
        #Function to return all the probabilities of the seen trigrams
        #       Input:  -trigrams: a dataframe with 2 columns: 
        #                       -token: this are all the trigrams that start with the same bigram prefix
        #                       -count: how often this trigram is seen
        #               -gamma3: a value for the discount. This is an amount substracted from every count. 
        #                       The sum of this discount will be divided over all unseen trigrams. This process is
        #                       called smoothing. Default: gamma3 =0.5
        get_prob_obs_trigrams <- function(trigrams, gamma3=0.5){
                prob <- (trigrams[ ,count] - gamma3)/sum(trigrams[ ,count])
                prob_obs_trigram <- data.table(token = trigrams[,token], prediction = trigrams[,prediction], prob = prob)[order(-prob)]
                prob_obs_trigram[, nminus1 :=strsplit(token, "_[^_]+$")][]
                prob_obs_trigram
        }
        
        prob_obs_trigram <- get_prob_obs_trigrams(obs_trigrams)
        
        #If this datagram is not empty, just use this one, and don't go one with the Back-off methode. This makes
        #the function much faster
        #if(prob_obs_trigram[,.(.N)] != 0){
        #        return(prob_obs_trigram[,prediction][1])
        #}
        
        
        #########################################################################
        #Now we will to the same for the unseen trigrams
        #########################################################################
        
        #To calculate the probabilities of the unseen trigrams, starting with bigram, we need to make same functions: 
        #       1. get_unseen trigram_tails(trigrams_obs, unigrams)
        #       2. get_alpha_trigram(trigram_obs, gamma3 = 0.5)
        #       3. get_obs_bigrams(unigram, bigrams)
        #       4. get_prob_obs_bigrams(bigrams_obs)
        #       5. get_alpha_bigram(bigrams_obs, gamma2 = 0.5)
        #       6. get_unseen_bigrams_tails(bo_bigrams, bo_bigrams_obs)
        #       7. get_bo_bigrams(bigram, unseen_trigram_tails)
        #       8. get_prob_bigrams(bigram, unseen_trigram_tails, bigrams, alpha_bigram)
        
        
        #1. Function to find all words in B(w_i-2, w_i-1): 
        #all words w_i, so that the trigram (w_i-2, w_i-1, w_i) is not an observated trigram
        #       Input:  -trigrams_obs: the observated trigrams starting with the bigram (w_i-2, w_i-1)
        #               -unigrams: all words in the corpus with their count
        #       Output: character vector with all words that can complete the trigram, so that this trigram is not in trigram_obs
        get_unseen_trigram_tails <- function(obs_trigrams, unigrams){
                trigram_tails <- str_split_fixed(obs_trigrams[,token], "_", 3)[,3] #Find all observated trigram tails
                setdiff(unigrams[,token], trigram_tails)                          #Find all elements of unigrams, that are not in trigrams_tails
        }
        
        unseen_trigram_tails <- get_unseen_trigram_tails(obs_trigrams, unigrams)
        
        #2. Function to calculate alpha(w_i-2, W_i-1)
        #       Input:  -bigram: this is the w_i-2 and w_i-1
        #               -trigrams: a dataframe with all observated trigrams and their count
        #               -gamme3: this is the discount for all trigrams, default is set to 0.5
        #       Output: -alpha_trigram: the total probability mass discounted from all observed trigrams.
        get_alpha_trigram <- function(trigram_obs, gamma3 = 0.5){
                if(trigram_obs[,.N]==0) return(1)
                1-sum((trigram_obs[,count] - gamma3)/sum(trigram_obs[,count]))
        }
        
        alpha_trigram <- get_alpha_trigram(obs_trigrams)  # -->0.5
        
        
        #3. Function to find all observated back_off bigrams 
        #       Input:  -bigram: this is the bigram-prefix ("sell_the")
        #               -unseen_trigram_tails: all tails of the unseen trigrams
        #               -bigrams: a dataframe with 2 columns: token (bigrams) and their count
        #       Output: a dataframe with 2 columns: 
        #               -token: observated back-off bigrams
        #               -count: their frequency
        get_obs_bigrams <- function(bigram, unseen_trigram_tails, bigrams){
                bo_bigrams <- paste(unigram, unseen_trigram_tails, sep = "_")
                obs_bo_bigrams <- bigrams[bigrams[,token] %in% bo_bigrams]
        }
        
        bigrams_obs <- get_obs_bigrams(bigram, unseen_trigram_tails, bigrams)        #Find all bigrams that start with unigram
        
        #4. Function to calculate the maximum likelihood for observed bigrams
        #       Input:  -bigrams_obs: dataframe with 2 columns: 
        #                               -bigrams: this are all the bigrams that start with the same unigram prefix
        #                               -count: the frequency of the token in the vocabulary
        #               -unigrams: all words in the corpus with their count 
        #               -gamma2: the discount for all bigrams. Default is set to 0.5
        #       Output: a dataframe with 2 columns: 
        #               -token: this is a bigram, starting all with the same unigram
        #               -prob: this is the maximum likelihood probability of the bigram
        get_prob_obs_bigram <- function(bo_bigrams_obs, unigrams, gamma2 = 0.5){
                unigram_token <- str_split_fixed(bo_bigrams_obs[,token], "_", 2)[, 1]               #This gives the first word of the bigram-prefix
                unigram <- unigrams[unigrams[,token] %in% unigram,]                                 #This gives the unigrams with its count
                prob <- (bo_bigrams_obs[,count] - gamma2)/ unigram$count
                bigram_prob <- data.table(token = bigrams_obs[,token], prob = prob)[order(-prob)]
                bigram_prob
        }
        
        #get_prob_obs_bigram(bigrams_obs)       
        
        #5. Function to calculate alpha(w_i-1), this is the total probability mass discounted from all observed back-off bigrams.
        #       Input:  -unigram: this is the w_i-1, in the example  "the"
        #               -bigrams: a dataframe with all bigrams in the corpus and their count.
        #               -gamma2: the discount used for all bigrams, default is set to 0.5
        get_alpha_bigram <- function(unigram, bigrams, gamma2=0.5){
                index <- grepl(paste0("^", unigram), bigrams[,token])
                A_bigrams <- bigrams[index]                           #Find all the bigrams (w_i-1, w_i) where w_i is in A(w_i-1)
                if(A_bigrams[,.(.N)] ==0) return(1)
                1-sum((A_bigrams[,count] - gamma2) / sum(A_bigrams[,count]))
        }
        
        alpha_bigram <- get_alpha_bigram(unigram, bigrams)
        
        #6. Function to find all words in B(w_i-1): 
        #all words w_i, so that the bigram (w_i-1, w_i) is not an observated bigram
        #       Input:  -bigrams_obs: a dataframe with 2 columns: 
        #                       -token: the observated bigrams starting with w_i-1
        #                       -count: the frequency of the words
        #               -unigrams: a dataframe with 2 columns: 
        #                       -token: all words in the corpus 
        #                       -count: the frequency of the words
        #       Output: character vector with all words in B(w_i-1)
        get_unseen_bigram_tails <- function(bo_bigrams, bo_bigrams_obs){
                bo_bigram_tails <- str_split_fixed(bo_bigrams, "_", 2)[,2]              #Find all the back_off bigram tails
                obs_bigram_tails <- str_split_fixed(bo_bigrams_obs[,token], "_", 2)[,2] #Find all the observated bigram tails
                setdiff(bo_bigram_tails, obs_bigram_tails)                              #Find all bo_bigram tails that are not in obs_bigram_tails
        }
        
        
        
        ###################### Gaat dit goed???? last_word_bigram heeft lengte 1, unseen_trigram_tails langere lengthe #####################
        
        #7. Function that gives back the backed-off bigrams
        #       Input:  -bigram: this is the bigram prefix
        #               -unseen_trigram_tails
        #       Output: -bigrams that you get if you concatenate the last word of the bigram, with the unseen trigrams, 
        #               these are the backed-off bigrams
        get_bo_bigrams <- function(bigram, unseen_trigram_tails){
                last_word_bigram <- str_split_fixed(bigram, "_", 2)[,2]
                #last_word_bigram <- str_extract(bigram, "[a-z]+$")
                paste(last_word_bigram, unseen_trigram_tails, sep = "_")
        }
        
        #8. Function to calculate the maximum likelihood for unseen bigrams
        #       Input:  -unigrams: dataframe with 2 columns: 
        #                               -token: the unigrams
        #                               -count: the frequency of the token in the vocabulary
        #               -bo_bigram_unseen: the unseen bigrams that are in backed-off bigrams
        #               -alpha_bigram: the total probability mass discounted from all observed back-off bigrams.
        #       Output: prob_unseen_bo_bigrams: a dataframe with 2 columns: 
        #                       -token: this is the unigram
        #                       -prob: this is the maximum likelihood probability
        get_prob_unseen_bigram <- function(unigrams, bo_bigram_unseen, alpha_bigram){
                bigram_unseen_tails <- str_split_fixed(bo_bigram_unseen, "_", 2)[,2]
                bigram_unseen_tails_counts <- unigrams[unigrams[,token] %in% bigram_unseen_tails]
                numerator <- bigram_unseen_tails_counts[,count]
                denominator <- sum(bigram_unseen_tails_counts[,count])
                prob <- alpha_bigram * numerator / denominator
                prob_unseen_bo_bigrams <- data.table(token = bo_bigram_unseen ,prob = prob) 
                if(prob_unseen_bo_bigrams[,.(.N)]==0){prob_unseen_bo_bigrams <- data.table(token = character(), prob = numeric())}
                prob_unseen_bo_bigrams
        }
        
        #8. Function to calculate the maximum likelihood for bigrams
        get_prob_bigrams <- function(bigram, unseen_trigram_tails, bigrams, alpha_bigram){
                unigram <- str_split_fixed(bigram, "_", 2)[,2]
                #Find all the backed-off bigrams
                bo_bigrams <- get_bo_bigrams(bigram, unseen_trigram_tails)
                
                #Separate the backed_off bigrams into the observated ones and the unseen
                #Observated backed-off bigrams: 
                bo_bigrams_obs <- get_obs_bigrams(bigram, unseen_trigram_tails, bigrams)        #Find all bigrams that start with unigram    
                #Unseen backed-off bigrams: 
                unseen_bigram_tails <- get_unseen_bigram_tails(bo_bigrams, bo_bigrams_obs)
                bo_bigram_unseen <- paste(unigram, unseen_bigram_tails, sep = "_")
                
                #Calculate the maximum likelihood for the back-off bigrams
                #For the observated backed-off bigrams: 
                prob_obs_bo_bigrams <- get_prob_obs_bigram(bo_bigrams_obs, unigrams)
                #For the unseen backed-off bigrams:
                prob_unseen_bo_bigrams <- get_prob_unseen_bigram(unigrams, bo_bigram_unseen, alpha_bigram)
                
                #Combine the probs for the observated and the unseen back-off bigrams
                prob_bo_bigrams <- rbind(prob_obs_bo_bigrams, prob_unseen_bo_bigrams)
                prob_bo_bigrams
        }
        
        prob_bigrams <- get_prob_bigrams(bigram, unseen_trigram_tails, bigrams, alpha_bigram)
        
        #9. Function to calculate the maximum likelihood for unseen trigrams
        #       Input:  -bigram: this is the bigram-prefix, to complete to unseen trigrams
        #               -unseen_trigram_tails: all tails of the unseen trigrams
        #               -bigrams: a dataframe with 2 columns: token (bigrams) and their count
        #               -alpha_bigram: the total probability mass discounted from all observed back-off bigrams.
        #               -alpha_trigram: the total probability mass discounted from all observed trigrams.
        #       Output: a dataframe with 2 columns: 
        #                       -token: this is the trigram
        #                       -prob: this is the maximum likelihood probability of the trigram
        get_prob_unseen_trigrams <- function(bigram, unseen_trigram_tails, bigrams, alpha_bigram, alpha_trigram){
                unigram <- str_split_fixed(bigram, "_", 2)[,1]
                prob_bo_bigrams <- get_prob_bigrams(bigram, unseen_trigram_tails, bigrams, alpha_bigram)
                token = paste(unigram, prob_bo_bigrams$token, sep = "_")
                numerator <- prob_bo_bigrams[,prob]
                denominator <- sum(numerator)
                prob_unseen_trigrams <- data.table(token = token, prob = alpha_trigram * numerator / denominator)
                prob_unseen_trigrams
                
                
        }
        
        prob_unseen_trigrams <- get_prob_unseen_trigrams(bigram, unseen_trigram_tails, bigrams, alpha_bigram, alpha_trigram)
        prob_unseen_trigrams[, nminus1 :=strsplit(token, "_[^_]+$")][]
        prob_unseen_trigrams[, prediction:=sub('.*_(.*)$', '\\1', token)][]
        
        
        #To find the probability of all trigrams, combine the probs for the observated and the unseen trigrams: 
        prob_trigrams <- rbind(prob_obs_trigram, prob_unseen_trigrams)[order(-prob)] 
        
        #What will be the word most likely to follow the bigram
        #word <- str_split_fixed(prob_trigrams[,token][1:nr_of_outputs], "_", 3)[,3]
        word <- prob_trigrams$prediction[1]
        return(word)
        
}

bigram <- "adam sandler's"
words <- c("stories", "novels", "pictures", "movies")
Find_probs(bigram)

prob_trigrams[grep("must_be_insensitive$", prob_trigrams$token),]
prob_trigrams[grep("must_be_asleep$", prob_trigrams$token),]
prob_trigrams[grep("must_be_callous$", prob_trigrams$token),]
prob_trigrams[grep("must_be_insane$", prob_trigrams$token),]

