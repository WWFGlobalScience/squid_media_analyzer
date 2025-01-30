##Assessing topics discussed regarding aquaculture

library(dplyr)
library(tidyr)
library(revtools)
library(synthesisr)
library(tidytext)
library(spacyr)
library(stringr)

####################################
### Clean body text for analysis ###
####################################
prep_text <- rel_set %>% select(id_key,headline,body,sentiment,relevance,year,region) %>% distinct()
prep_text$body <- gsub("\\?","\'",prep_text$body)
colnames(prep_text) <- c("id_key","headline","body","headline_sentiment","relevance","year","region")

fixed_maine <- read.csv("sentiment_maine_clean_bodies.csv",header=TRUE,stringsAsFactors = FALSE) %>% select(-X)
prep_text2 <- anti_join(prep_text,fixed_maine,by=c("id_key"))
prep_text_final <- bind_rows(prep_text2,fixed_maine)

prep_text <- prep_text_final
###Add in manually fixed entries from validation
# fix <- read.csv("fixed_set_1-9-2025-2.csv",header=TRUE, stringsAsFactors = FALSE)
# fix_ids <- as.vector(fix$id_key)
# prep_text2 <- prep_text %>% filter(! id_key %in% fix_ids)
# prep_text_new <- bind_rows(prep_text2,fix)
# prep_text_new <- filter(prep_text_new,relevance != "irrelevant" | relevance != "duplicate") %>% select(-X,-X.1)
# prep_text <- prep_text_new

### Extract sentences into a dataframe for asentiment### Extract sentences into a dataframe for assessment ###
tidy_text <- prep_text %>% unnest_tokens(word,body,token="words") %>% group_by(id_key)
tidy_sentences <- prep_text %>% unnest_tokens(sentence,body,token="sentences") %>% group_by(id_key)
tidy_paragraphs <- prep_text %>% unnest_tokens(paragraph,body,token="paragraphs") %>% group_by(id_key)
tidy_articles <- prep_text

####################################
###### Analysis 1: sentiments ######
####################################

###Get sentiment libraries
afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")

###Get analysis set - just seaweed in Maine
tidy_text <- tidy_text %>% filter(relevance == "seaweed aquaculture") %>% filter(region == "Maine")
tidy_sentences <- tidy_sentences %>% filter(relevance == "seaweed aquaculture") %>% filter(region == "Maine")
tidy_articles <- tidy_articles %>% filter(relevance == "seaweed aquaculture") %>% filter(region == "Maine") %>% filter(!is.na(headline))

###Get sentiments of sentences with sentimentr
library(sentimentr)
library(tm)

corpus <- tidy_articles %>%
  get_sentences() %>%
  sentiment()

article_sentiment <- tidy_articles %>%
  get_sentences() %>%
  sentiment_by(by=c("id_key"))

article_sentiment <- left_join(article_sentiment,tidy_articles)

b <- article_sentiment %>% filter(headline_sentiment == "bad") %>% select(id_key)
b$plot_id <- c(1:nrow(b))

n <- article_sentiment %>% filter(headline_sentiment == "neutral") %>% select(id_key)
n$plot_id <- c(1:nrow(n))

p <- article_sentiment %>% filter(headline_sentiment == "good") %>% select(id_key)
p$plot_id <- c(1:nrow(p))

plot_ids <- bind_rows(b,n,p)

article_sentiment <- left_join(article_sentiment,plot_ids,by=c("id_key"))

article_sentiment %>%
  ggplot(mapping = aes(x=plot_id, y=ave_sentiment,fill=headline_sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ headline_sentiment, nrow=1, scales = "free_x") +
  labs(title = "Average sentiment expressed in local media in Maine on seaweed aquaculture") +
  theme_minimal()

library(magrittr)
set.seed(2)
article_sentiment_highlighted <- article_sentiment %>%
  highlight()

####################################
#### Analysis 1: word frequency ####
####################################

### Filter out specific sentences discussing aquaculture and count word frequency and compare between seaweed aquaculture and generic aquaculture ###
aquaString <- c("(?i)aquaculture|(?i)farming\(?i)farmed")
seaweedString <- c("(?i)seaweed|(?i)kelp|(?i)sea moss|(?i)dulse")

seaweed_sentences <- tidy_sentences %>% filter(str_detect(sentence, seaweedString))
seaweed_sentences$keyword <- c("seaweed")
aquaculture_sentences <- tidy_sentences %>% filter(str_detect(sentence, aquaString))
aquaculture_sentences$keyword <- c("aquaculture")

keyword_sentences <- as.data.frame(bind_rows(seaweed_sentences,aquaculture_sentences)) %>%
  distinct()

#keywords <- data.frame("word" = c("aquaculture","seaweed","farmed","farming","kelp","dulse","seaweeds","marine"))

keyword_freq <- keyword_sentences %>%
  unnest_tokens(word,sentence) %>%
  count(sentiment,word,sort=TRUE)

keyword_total <- keyword_freq %>%
  group_by(sentiment) %>%
  summarize(total = sum(n))

keyword_freq <- left_join(keyword_freq,keyword_total)

word_tf_idf <- keyword_freq %>%
  bind_tf_idf(word,sentiment,n)