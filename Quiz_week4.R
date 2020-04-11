library(data.table)
library(dplyr)
library(quanteda)
library(stringr)

#Function to create dataframe with 2 columns: ngram and the count. 
#       Input:  -corp: corpus 
#               -n: n=1 --> unigram, n=2 --> bigram, n=3 --> trigram, etc
#       Output: -dataframe with 2 columns: ngram and the count of the ngram
makedf <- function(corp, n){
        #Create ngram
        ngram <- tokens(corp, remove_numbers = TRUE, what = "sentence") %>% 
                tokens_tolower() %>% 
                as.character() %>% 
                tokens(ngrams = n, remove_punct = TRUE, remove_numbers = TRUE) %>% 
                as.character()
        dfm_ngram <- dfm(ngram)
        ngram_freq <- docfreq(dfm_ngram)
        ngram_freq_df <- data.frame(token = names(ngram_freq), count = ngram_freq, row.names = NULL, stringsAsFactors = FALSE) %>% 
                arrange(desc(count))
        ngram_freq_df
}

unigrams <- makedf(all_samples, 1)
bigrams <- makedf(all_samples, 2)
trigrams <- makedf(all_samples, 3)

Find_probs <- function(bigram, words){
        
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
                index <- grep(paste0("^", bigram, "_"), trigrams$token)
                trigrams[index, ]
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
                prob <- (trigrams$count - gamma3)/sum(trigrams$count)
                prob_obs_trigram <- data.frame(token = trigrams$token, prob = prob, row.names = NULL, stringsAsFactors = FALSE) %>% 
                        arrange(desc(prob))
                prob_obs_trigram
        }
        
        prob_obs_trigram <- get_prob_obs_trigrams(obs_trigrams)
        
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
        #               -unigrams: all words in the corpus 
        #       Output: character vector with all words that can complete the trigram, so that this trigram is not in trigram_obs
        get_unseen_trigram_tails <- function(obs_trigrams, unigrams){
                trigram_tails <- str_split_fixed(obs_trigrams$token, "_", 3)[,3] #Find all observated trigram tails
                setdiff(unigrams$token, trigram_tails)                          #Find all elements of unigrams, that are not in trigrams_tails
        }
        
        unseen_trigram_tails <- get_unseen_trigram_tails(obs_trigrams, unigrams)
        
        #2. Function to calculate alpha(w_i-2, W_i-1)
        #       Input:  -bigram: this is the w_i-2 and w_i-1
        #               -trigrams: a dataframe with all observated trigrams and their count
        #               -gamme3: this is the discount for all trigrams, default is set to 0.5
        #       Output: -alpha_trigram: the total probability mass discounted from all observed trigrams.
        get_alpha_trigram <- function(trigram_obs, gamma3 = 0.5){
                if(nrow(trigram_obs) == 0) return(1)
                1-sum((trigram_obs$count - gamma3)/sum(trigram_obs$count))
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
                obs_bo_bigrams <- bigrams[bigrams$token %in%bo_bigrams, ]
        }
        
        bigrams_obs <- get_obs_bigrams(bigram, unseen_trigram_tails, bigrams)        #Find all bigrams that start with unigram
        
        #4. Function to calculate the maximum likelihood for observated bigrams
        #       Input:  -bigrams_obs: dataframe with 2 columns: 
        #                               -bigrams: this are all the bigrams that start with the same unigram prefix
        #                               -count: the frequency of the token in the vocabulary
        #               -gamma2: the discount for all bigrams. Default is set to 0.5
        #       Output: a dataframe with 2 columns: 
        #               -token: this is a bigram, starting all with the same unigram
        #               -prob: this is the maximum likelihood probability of the bigram
        get_prob_obs_bigram <- function(bo_bigrams_obs, gamma2 = 0.5){
                unigram_token <- str_split(bo_bigrams_obs$token, "_")[[1]][1]               #This gives the first word of the bigram-prefix
                unigram <- unigrams[unigrams$token %in% unigram_token,]
                prob <- (bo_bigrams_obs$count - gamma2)/ unigram$count
                bigram_prob <- data.frame(token = bigrams_obs$token, prob = prob, row.names = NULL, stringsAsFactors = FALSE) %>% 
                        arrange(desc(prob))
                bigram_prob
        }
        
        #get_prob_obs_bigram(bigrams_obs)       
        
        #5. Function to calculate alpha(w_i-1), this is the total probability mass discounted from all observed back-off bigrams.
        #       Input:  -unigram: this is the w_i-1 with its count, in the example  "the"
        #               -bigrams: a dataframe with all bigrams in the corpus and their count.
        #               -gamma2: the discount used for all bigrams, default is set to 0.5
        get_alpha_bigram <- function(unigram, bigrams, gamma2=0.5){
                index <- grepl(paste0("^", unigram), bigrams$token)
                A_bigrams <- bigrams[index, ]                           #Find all the bigrams (w_i-1, w_i) where w_i is in A(w_i-1)
                if(nrow(A_bigrams) == 0) return(1)
                1-sum((A_bigrams$count - gamma2) / sum(A_bigrams$count))
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
                obs_bigram_tails <- str_split_fixed(bo_bigrams_obs$token, "_", 2)[,2]   #Find all the observated bigram tails
                setdiff(bo_bigram_tails, obs_bigram_tails)                              #Find all bo_bigram tails that are not in obs_bigram_tails
        }
        
        
        #7. Function that gives back the backed-off bigrams
        #       Input:  -bigram: this is the bigram prefix
        #               -unseen_trigram_tails
        #       Output: -bigrams that you get if you concatenate the last word of the bigram, with the unseen trigrams, 
        #               these are the backed-off bigrams
        get_bo_bigrams <- function(bigram, unseen_trigram_tails){
                last_word_bigram <- str_extract(bigram, "[a-z]+$")
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
                bigram_unseen_tails_counts <- unigrams[unigrams$token %in% bigram_unseen_tails, ]
                numerator <- bigram_unseen_tails_counts$count
                denominator <- sum(bigram_unseen_tails_counts$count)
                prob <- alpha_bigram * numerator / denominator
                prob_unseen_bo_bigrams <- data.frame(token = bo_bigram_unseen, prob = prob, row.names = NULL, stringsAsFactors = FALSE) 
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
                prob_obs_bo_bigrams <- get_prob_obs_bigram(bo_bigrams_obs)
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
                numerator <- prob_bo_bigrams$prob
                denominator <- sum(numerator)
                prob_unseen_trigrams <- data.frame(token = token, prob = alpha_trigram * numerator / denominator)
                prob_unseen_trigrams
                
                
        }
        
        prob_unseen_trigrams <- get_prob_unseen_trigrams(bigram, unseen_trigram_tails, bigrams, alpha_bigram, alpha_trigram)
        
        #To find the probability of all trigrams, combine the probs for the observated and the unseen trigrams: 
        prob_trigrams <- rbind(prob_obs_trigram, prob_unseen_trigrams) %>% 
                arrange(desc(prob))
        
        #Make a dataframe with bigram + words, and their token
        token <- paste0(bigram, "_",words)
        prob <- vector()
        for(i in 1:length(words)){
                if(identical(prob_trigrams[grep(paste0(token[i], "$"), prob_trigrams$token),]$prob, numeric(0))){
                        prob = c(prob, 0)
                }
                else{
                        prob = c(prob, prob_trigrams[grep(paste0(token[i], "$"), prob_trigrams$token),]$prob)
                }
                }
        results <- data.frame(token = token, prob = prob) %>% 
                arrange(desc(prob))
        return(results)
}

bigram <- "adam_sandler's"
words <- c("stories", "novels", "pictures", "movies")
Find_probs(bigram, words)

prob_trigrams[grep("must_be_insensitive$", prob_trigrams$token),]
prob_trigrams[grep("must_be_asleep$", prob_trigrams$token),]
prob_trigrams[grep("must_be_callous$", prob_trigrams$token),]
prob_trigrams[grep("must_be_insane$", prob_trigrams$token),]

