##Analysis script for seaweed indicators
library(dplyr)
library(tidyr)
library(ggplot2)

##Read in data
rel_set <- readRDS("seaweed_included_2024-09-17.rds")
final_data <- readRDS("seaweed_fullresults_2024-09-17.rds")
search_results <- readRDS("seaweed_searchresults_2024-09-17.rds")

###Tweak analysis set
##Remove articles pre-2015 (not sure why those are there)
rel_set <- rel_set %>% filter(year > 2014)
search_results <- 
###Set variables
year <- c(2015:2023)
topic <- c("other aquaculture","seaweed aquaculture")

###Summarize search results
tot_search <- dplyr::count(search_results,search_origin) ##check these - numbers don't match what we have from Rich

##Total number of relevant articles by source
set1 <- final_data %>% select(id_key,publication_region,relevance) %>% unique()
set1$relevance <- gsub("other aquaculture","relevant",set1$relevance)
set1$relevance <- gsub("seaweed aquaculture","relevant",set1$relevance)
sum1 <- dplyr::count(set1,publication_region,relevance)

##Total number of articles by topic by source
set2 <- rel_set %>% select(id_key,publication_region,relevance) %>% unique()
sum2 <- dplyr::count(set2,publication_region,relevance)

##Total number of relevant article by topic by geographic focus
set3 <- rel_set %>% select(id_key,relevance,topic_location) %>% unique()
sum3 <- dplyr::count(set3,relevance,topic_location) %>% arrange(desc(n),group_by=relevance)

##Total number of relevant articles by topic
set4 <- rel_set %>% select(id_key,relevance) %>% unique()
sum4 <- dplyr::count(set4,relevance)

##Topic over time
set5 <- rel_set %>% select(id_key,year,relevance) %>% unique()
sum5 <- dplyr::count(set5,year,relevance)

#Create PDF of line graph
pdf(file=paste("results/topic_time",format(Sys.time(), "%m-%d-%Y"),".pdf",sep=""), width=8, height=4)
ggplot(data=sum5, aes(x=year,y=n,group=relevance)) +
  geom_line(aes(color=relevance)) +
  geom_point(aes(color=relevance)) +
  theme_minimal() +
  theme(legend.position="bottom") +
  labs(x="publication year",y="number of articles") +
  scale_color_discrete(name="topic") +
  scale_x_continuous(name="publication year",breaks=seq(2015,2024,1))
dev.off()

###Summarize sentiment

##Sentiment by topic over time
set6 <- rel_set %>% select(id_key,year,relevance,sentiment) %>% unique()
sum6s <- set6 %>% 
  dplyr::count(year,relevance,sentiment) %>%
  filter(relevance == "seaweed aquaculture")
sum6s$senti <- factor(sum6s$sentiment, levels = c('bad', 'neutral', 'good'))

indicator_table_seaweed <- sum6s %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(total=sum(n)) %>%
  dplyr::mutate(prop_pos=round((n/total)*100))

indicator_table_seaweed_pos <- filter(indicator_table_seaweed,sentiment=="good") %>% select(year,relevance,prop_pos)

sum6a <- set6 %>% 
  dplyr::count(year,relevance,sentiment) %>%
  filter(relevance == "other aquaculture")
sum6a$senti <- factor(sum6s$sentiment, levels = c('bad', 'neutral', 'good'))

indicator_table_all <- sum6a %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(total=sum(n)) %>%
  dplyr::mutate(prop_pos=round((n/total)*100))

indicator_table_all_pos <- filter(indicator_table_all,sentiment=="good") %>% select(year,relevance,prop_pos)

final_reporting_indicator <- 
  bind_rows(indicator_table_all_pos,indicator_table_seaweed_pos) %>%
  spread(relevance,prop_pos)

final_indicator_data <- bind_rows(indicator_table_all,indicator_table_seaweed) %>% select(year,relevance,sentiment,n,prop_pos)
  
write.csv(final_reporting_indicator,"final_indicator2_table.csv")
write.csv(final_indicator_data,"final_indicator2_data.csv")

#Create PDF of stacked bar
pdf(file=paste("results/sentiment_time_seaweed",format(Sys.time(), "%m-%d-%Y"),".pdf",sep=""), width=8, height=4)
ggplot(data=sum6s, aes(x=year,y=n,fill=senti)) +
  geom_bar(position="fill",stat="identity") +
  #geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  theme_minimal() +
  theme(legend.position="right") +
  labs(x="publication year",y="proportion of articles") +
  scale_fill_discrete(name="sentiment regarding seaweed") +
  scale_x_continuous(name="publication year",breaks=seq(2015,2024,1)) +
  theme(axis.text.x=element_text(angle=45))
dev.off()

pdf(file=paste("results/sentiment_time_other",format(Sys.time(), "%m-%d-%Y"),".pdf",sep=""), width=8, height=4)
ggplot(data=sum6a, aes(x=year,y=n,fill=senti)) +
  geom_bar(position="fill",stat="identity") +
  theme_minimal() +
  theme(legend.position="right") +
  labs(x="publication year",y="proportion of articles") +
  scale_fill_discrete(name="sentiment regarding other aquaculture") +
  scale_x_continuous(name="publication year",breaks=seq(2015,2024,1)) +
  theme(axis.text.x=element_text(angle=45))
dev.off()

##Comparison if we were look at the database sources without the local sources
db_only <- rel_set %>% filter(search_origin == "Lexis Nexis" | search_origin == "Access World News")
set6b <- db_only %>% select(id_key,year,relevance,sentiment) %>% unique()

sum6ss <- set6b %>% 
  dplyr::count(year,relevance,sentiment) %>%
  filter(relevance == "seaweed aquaculture")
sum6ss$senti <- factor(sum6ss$sentiment, levels = c('bad', 'neutral', 'good'))

sum6aa <- set6b %>% 
  dplyr::count(year,relevance,sentiment) %>%
  filter(relevance == "other aquaculture")
sum6aa$senti <- factor(sum6aa$sentiment, levels = c('bad', 'neutral', 'good'))

indicator_table_seaweed_dbonly <- sum6ss %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(total=sum(n)) %>%
  dplyr::mutate(prop_pos=round((n/total)*100))

indicator_table_seaweed_pos_dbonly <- filter(indicator_table_seaweed_dbonly,sentiment=="good") %>% select(year,relevance,prop_pos)

indicator_table_all_dbonly <- sum6aa %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(total=sum(n)) %>%
  dplyr::mutate(prop_pos=round((n/total)*100))

indicator_table_all_pos_dbonly <- filter(indicator_table_all_dbonly,sentiment=="good") %>% select(year,relevance,prop_pos)

final_reporting_indicator_dbonly <- 
  bind_rows(indicator_table_all_pos_dbonly,indicator_table_seaweed_pos_dbonly) %>%
  spread(relevance,prop_pos)

final_indicator_data_dbonly <- bind_rows(indicator_table_all_dbonly,indicator_table_seaweed_dbonly) %>% select(year,relevance,sentiment,n,prop_pos)

write.csv(final_reporting_indicator_dbonly,"results/final_indicator2_table_dbonly.csv")
write.csv(final_indicator_data_dbonly,"results/final_indicator2_data_dbonly.csv")

#Create PDF of stacked bar
pdf(file=paste("results/sentiment_time_seaweed_dbonly",format(Sys.time(), "%m-%d-%Y"),".pdf",sep=""), width=8, height=4)
ggplot(data=sum6ss, aes(x=year,y=n,fill=senti)) +
  geom_bar(position="fill",stat="identity") +
  #geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  theme_minimal() +
  theme(legend.position="right") +
  labs(x="publication year",y="proportion of articles") +
  scale_fill_discrete(name="sentiment regarding seaweed") +
  scale_x_continuous(name="publication year",breaks=seq(2015,2024,1)) +
  theme(axis.text.x=element_text(angle=45))
dev.off()

pdf(file=paste("results/sentiment_time_other_dbonly",format(Sys.time(), "%m-%d-%Y"),".pdf",sep=""), width=8, height=4)
ggplot(data=sum6aa, aes(x=year,y=n,fill=senti)) +
  geom_bar(position="fill",stat="identity") +
  theme_minimal() +
  theme(legend.position="right") +
  labs(x="publication year",y="proportion of articles") +
  scale_fill_discrete(name="sentiment regarding other aquaculture") +
  scale_x_continuous(name="publication year",breaks=seq(2015,2024,1)) +
  theme(axis.text.x=element_text(angle=45))
dev.off()

##Only local sources over time
ia_only <- rel_set %>% filter(search_origin == "Internet Archive Regional")
set6c <- ia_only %>% select(id_key,year,relevance,sentiment) %>% unique()

sum6sss <- set6c %>% 
  dplyr::count(year,relevance,sentiment) %>%
  filter(relevance == "seaweed aquaculture")
sum6sss$senti <- factor(sum6sss$sentiment, levels = c('bad', 'neutral', 'good'))

sum6aaa <- set6c %>% 
  dplyr::count(year,relevance,sentiment) %>%
  filter(relevance == "other aquaculture")
sum6aaa$senti <- factor(sum6aaa$sentiment, levels = c('bad', 'neutral', 'good'))

indicator_table_seaweed_iaonly <- sum6sss %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(total=sum(n)) %>%
  dplyr::mutate(prop_pos=round((n/total)*100))

indicator_table_seaweed_pos_iaonly <- filter(indicator_table_seaweed_iaonly,sentiment=="good") %>% select(year,relevance,prop_pos)

indicator_table_all_iaonly <- sum6aaa %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(total=sum(n)) %>%
  dplyr::mutate(prop_pos=round((n/total)*100))

indicator_table_all_pos_iaonly <- filter(indicator_table_all_iaonly,sentiment=="good") %>% select(year,relevance,prop_pos)

final_reporting_indicator_iaonly <- 
  bind_rows(indicator_table_all_pos_iaonly,indicator_table_seaweed_pos_iaonly) %>%
  spread(relevance,prop_pos)

final_indicator_data_iaonly <- bind_rows(indicator_table_all_iaonly,indicator_table_seaweed_iaonly) %>% select(year,relevance,sentiment,n,prop_pos)

write.csv(final_reporting_indicator_iaonly,"results/final_indicator2_table_iaonly.csv")
write.csv(final_indicator_data_iaonly,"results/final_indicator2_data_iaonly.csv")

#Create PDF of stacked bar
pdf(file=paste("results/sentiment_time_seaweed_iaonly",format(Sys.time(), "%m-%d-%Y"),".pdf",sep=""), width=8, height=4)
ggplot(data=sum6sss, aes(x=year,y=n,fill=senti)) +
  geom_bar(position="fill",stat="identity") +
  #geom_text(aes(label=n),position = position_fill(vjust = 0.5)) +
  theme_minimal() +
  theme(legend.position="right") +
  labs(x="publication year",y="proportion of articles") +
  scale_fill_discrete(name="sentiment regarding seaweed") +
  scale_x_continuous(name="publication year",breaks=seq(2015,2024,1)) +
  theme(axis.text.x=element_text(angle=45))
dev.off()

pdf(file=paste("results/sentiment_time_other_iaonly",format(Sys.time(), "%m-%d-%Y"),".pdf",sep=""), width=8, height=4)
ggplot(data=sum6aaa, aes(x=year,y=n,fill=senti)) +
  geom_bar(position="fill",stat="identity") +
  theme_minimal() +
  theme(legend.position="right") +
  labs(x="publication year",y="proportion of articles") +
  scale_fill_discrete(name="sentiment regarding other aquaculture") +
  scale_x_continuous(name="publication year",breaks=seq(2015,2024,1)) +
  theme(axis.text.x=element_text(angle=45))
dev.off()

##Sentiment by focal geography by topic (limited to Seaweed focal areas)
set7 <- rel_set %>% 
  select(id_key,year,relevance,sentiment,topic_location) %>% 
  unique() %>%
  filter(topic_location %in% c("maine","massachusetts","new hampshire","alaska","oregon","washington","california")) %>%
  unique()
sum7 <- set7 %>%
  group_by(topic_location) %>%
  dplyr::count(year,relevance,sentiment)

sum7a <- sum7 %>% filter(relevance == "other aquaculture")
sum7s <- sum7 %>% filter(relevance == "seaweed aquaculture")

pdf(file=paste("results/sentiment_time_geo_otheraquaculture",format(Sys.time(), "%m-%d-%Y"),".pdf",sep=""), width=12, height=12)
ggplot(sum7a, aes(x=year,y=n,group=sentiment)) + 
  geom_line(aes(color=sentiment)) +
  geom_point(aes(color=sentiment)) +
  theme_minimal() +
  theme(legend.position="bottom") +
  labs(x="publication year",y="number of articles") +
  scale_color_discrete(name="sentiment") +
  facet_wrap(~topic_location) +
  xlab("year") +
  scale_x_continuous(name="publication year",breaks=seq(2015,2024,1)) +
  theme(axis.text.x=element_text(angle=45))
dev.off()

pdf(file=paste("results/sentiment_time_geo_seaweed",format(Sys.time(), "%m-%d-%Y"),".pdf",sep=""), width=12, height=12)
ggplot(sum7s, aes(x=year,y=n,group=sentiment)) + 
  geom_line(aes(color=sentiment)) +
  geom_point(aes(color=sentiment)) +
  theme_minimal() +
  theme(legend.position="bottom") +
  labs(x="publication year",y="number of articles") +
  scale_color_discrete(name="sentiment") +
  facet_wrap(~topic_location) +
  xlab("year") +
  scale_x_continuous(name="publication year",breaks=seq(2015,2024,1)) +
  theme(axis.text.x=element_text(angle=45))
dev.off()

##Sentiment by publication geography by topic (limited to Seaweed focal areas)
ggplot(data=sum5, aes(x=year,y=n,group=relevance)) +
  geom_line(aes(color=relevance)) +
  geom_point(aes(color=relevance)) +
  theme_minimal() +
  theme(legend.position="bottom") +
  labs(x="publication year",y="number of articles") +
  scale_color_discrete(name="topic")