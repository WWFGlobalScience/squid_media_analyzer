##Source script for data analysis and visualization for seaweed sentiment

library(DBI)
library(RSQLite)
library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tibble)
library(stringr)
library(data.table)

##Read in seaweed DB as separate dataframes
db <- dbConnect(drv=RSQLite::SQLite(), dbname="seaweed_public_sentiment_2024_09_15.db")
alldat <- lapply(setNames(nm = dbListTables(db)), dbReadTable, conn = db)
list2env(alldat, envir = .GlobalEnv)

###Remove working files and disconnect from DB
rm(alldat)
dbDisconnect(db)

###Fill in missing source information
# ##Clean url
# extract_source_website <- function(url) {
#   # Find all occurrences of "http://"
#   pattern <- "(^\\w+://web.archive.org/.*/http\\w*://)(\\w+.*\\.\\w+/)(.*$)"
#   
#   match <- str_match(url, pattern)
#   if (!is.na(match[1,3])) {
#     return(match[1,3])
#   } else {
#     return(NA)  # Return NA if no match is found
#   }
# }
# 
# lll <- url_of_article %>% mutate(source_website = sapply(raw_url, extract_source_website))
# lll$cleaned_source_website <- gsub("/.*$","",lll$source_website)
# lll$cleaned_source_website <- gsub("^www\\.","",lll$cleaned_source_website)
# lll$cleaned_source_website <- gsub("^www2\\.","",lll$cleaned_source_website)
# lll$cleaned_source_website <- gsub("^sponsored\\.","",lll$cleaned_source_website)
# lll$cleaned_source_website <- gsub("^contributors\\.","",lll$cleaned_source_website)
# lll$cleaned_source_website <- gsub("^secure\\.","",lll$cleaned_source_website)
# 
# lll <- lll %>%
#   mutate(cleaned_source_website = trimws(cleaned_source_website))
# 
# ##Append website name to main article table
# p <- article %>% select(id_key,publication)
# pp <- lll %>% select(article_id,cleaned_source_website)
# colnames(pp) <- c("id_key","publication")
# orig_pub <- p %>% anti_join(pp,by="id_key")
# updated_pub <- pp %>% semi_join(p,by="id_key")
# ppp <- bind_rows(orig_pub,updated_pub)

##Manually fix ones that flagged as web.archive.com

for (i in 1:nrow(url_of_article)){
  url <- url_of_article$article_source_domain[i]
  id <- url_of_article$id_key[i]
  if (url == "web.archive.org" & id %in% c(662:675)){
    url_of_article$article_source_domain[i] <- "maineboats.com"
  } else if (url == "web.archive.org" & id %in% c(1071:1076)){
      url_of_article$article_source_domain[i] <- "mainebiz.biz"
  } else if (url == "web.archive.org" & id %in% c(1154:1257)){
    url_of_article$article_source_domain[i] <- "ellsworthamerican.com"
  } else
    print(NULL)
}

##Add region of publication based on article source domain
##Assign publication name for Internet Archive results
url_of_article <- url_of_article %>%
  mutate(region = case_when(
    article_source_domain == "mainepublic.org" ~ "Maine",
    article_source_domain == "www.mainepublic.org" ~ "Maine",
    article_source_domain == "weeklypacket.com" ~ "Maine",
    article_source_domain == "islandadvantages.com" ~ "Maine",
    article_source_domain == "mainescoast.com" ~ "Maine",
    article_source_domain == "www.mainescoast.com" ~ "Maine",
    article_source_domain == "maineboats.com" ~ "Maine",
    article_source_domain == "www.maineboats.com" ~ "Maine",
    article_source_domain == "couriergazette.com" ~ "Maine",
    article_source_domain == "digitalmaine.com" ~ "Maine",
    article_source_domain == "theforecaster.net" ~ "Maine",
    article_source_domain == "www.theforecaster.net" ~ "Maine",
    article_source_domain == "lcnme.com" ~ "Maine",
    article_source_domain == "pointseast.com" ~ "Maine",
    article_source_domain == "www.pointseast.com" ~ "Maine",
    article_source_domain == "workingwaterfront.com" ~ "Maine",
    article_source_domain == "www.workingwaterfront.com" ~ "Maine",
    article_source_domain == "castinepatriot.com" ~ "Maine",
    article_source_domain == "adventures.downeast.com" ~ "Maine",
    article_source_domain == "downeast.com" ~ "Maine",
    article_source_domain == "www.downeast.com" ~ "Maine",
    article_source_domain == "secure.downeast.com" ~ "Maine",
    article_source_domain == "quoddytides.com" ~ "Maine",
    article_source_domain == "thebollard.com" ~ "Maine",
    article_source_domain == "portlandmonthly.com" ~ "Maine",
    article_source_domain == "www.portlandmonthly.com" ~ "Maine",
    article_source_domain == "themainemag.com" ~ "Maine",
    article_source_domain == "www.themainemag.com" ~ "Maine",
    article_source_domain == "newengland.com" ~ "Maine",
    article_source_domain == "mainebiz.biz" ~ "Maine",
    article_source_domain == "www.mainebiz.biz" ~ "Maine",
    article_source_domain == "ellsworthamerican.com" ~ "Maine",
    article_source_domain == "www.ellsworthamerican.com" ~ "Maine",
    article_source_domain == "pressherald.com" ~ "Maine",
    article_source_domain == "www.pressherald.com" ~ "Maine",
    article_source_domain == "contributors.pressherald.com" ~ "Maine",
    article_source_domain == "boothbayregister.com" ~ "Maine",
    article_source_domain == "www.boothbayregister.com" ~ "Maine",
    article_source_domain == "knox.villagesoup.com" ~ "Maine",
    article_source_domain == "nhpr.org" ~ "New Hampshire",
    article_source_domain == "capeandislands.org" ~ "Massachusetts",
    article_source_domain == "duxburyclipper.com" ~ "Massachusetts",
    article_source_domain == "wgbh.org" ~ "Massachusetts",
    article_source_domain == "bostonglobe.com" ~ "Massachusetts",
    article_source_domain == "www2.bostonglobe.com" ~ "Massachusetts",
    article_source_domain == "www.bostonglobe.com" ~ "Massachusetts",
    article_source_domain == "sponsored.bostonglobe.com" ~ "Massachusetts",
    article_source_domain == "alaskabeacon.com" ~ "Alaska",
    article_source_domain == "kucb.org" ~ "Alaska",
    article_source_domain == "www.kucb.org" ~ "Alaska",
    article_source_domain == "www.ktoo.org" ~ "Alaska",
    article_source_domain == "thecordovatimes.com" ~ "Alaska",
    article_source_domain == "www.thecordovatimes.com" ~ "Alaska",
    article_source_domain == "alaska-native-news.com" ~ "Alaska",
    article_source_domain == "nwnewsnetwork.org" ~ "Washington & Oregon",
    article_source_domain == "king5.com" ~ "Washington",
    article_source_domain == "www.king5.com" ~ "Washington",
    article_source_domain == "seattletimes.com" ~ "Washington",
    article_source_domain == "vashonbeachcomber.com" ~ "Washington",
    article_source_domain == "kuow.org" ~ "Washington",
    article_source_domain == "opb.org" ~ "Oregon",
    article_source_domain == "www.opb.org" ~ "Oregon",
    article_source_domain == "oregonlive.com" ~ "Oregon",
    article_source_domain == "www.oregonlive.com" ~ "Oregon",
    article_source_domain == "koin.com" ~ "Oregon",
    article_source_domain == "www.koin.com" ~ "Oregon",
    article_source_domain == "kgw.com" ~ "Oregon",
    article_source_domain == "www.kgw.com" ~ "Oregon",
    article_source_domain == "katu.com" ~ "Oregon",
    article_source_domain == "www.katu.com" ~ "Oregon",
    article_source_domain == "ijpr.org" ~ "Oregon",
    article_source_domain == "www.ijpr.org" ~ "Oregon",
    article_source_domain == "theworldlink.com" ~ "Oregon",
    article_source_domain == "wweek.com" ~ "Oregon",
    article_source_domain == "tillamookcountypioneer.net" ~ "Oregon",
    article_source_domain == "calmatters.org" ~ "California",
    article_source_domain == "malibutimes.com" ~ "California",
    article_source_domain == "sfchronicle.com" ~ "California",
    article_source_domain == "latimes.com" ~ "California",
    article_source_domain == "www.latimes.com" ~ "California",
    article_source_domain == "timesofsandiego" ~ "California",
    article_source_domain == "vasiliatimesdelta.com" ~ "California",
    article_source_domain == "dailybreeze.com" ~ "California",
    article_source_domain == "www.dailybreeze.com" ~ "California",
    .default = as.character("Unknown")))

##Append source URL for Internet Archive results
article <- article %>% left_join(url_of_article, by=c("id_key"="article_id")) %>% select(-id_key.y)

##Clean up article file for source file
article$search_origin <- article$source_file
article$search_origin <- gsub("^.*AccessWorldNews.*","Access World News",article$search_origin)
article$search_origin <- gsub("^.*regional_search.*","Internet Archive Regional",article$search_origin)
article$search_origin <- gsub("^.*RegionalSearch_Manual.*","Regional Manual Search",article$search_origin)
article$search_origin <- gsub("^.*RegionalSearch2024.*","Google News",article$search_origin)
article$search_origin <- gsub("^.*RegionalSearch.*","Internet Archive Regional",article$search_origin)
article$search_origin <- gsub("^.*Froehlich.*","Lexis Nexis",article$search_origin)
article$search_origin <- gsub("^.*froelich.*","Other",article$search_origin) ##Need to determine what "froehlich_headlines.csv" came from
article$search_origin <- gsub("^.*NexisUni.*","Lexis Nexis",article$search_origin)

##Remove extraneous source_files that were actually document lists not articles 
article <- filter(article, !grepl("doclist",source_file))

##Update region for database results - limited to global
for (i in 1:nrow(article)){
  search_origin <- article$search_origin[i]
  if (search_origin == "Lexis Nexis" | search_origin == "Access World News" | search_origin == "Other"){
    article$region[i] <- "Global"
  } else
    print(NULL)
}

##Remove data from Froelich training set
article <- article %>% filter(search_origin != "Other")

##Append missing url data manually fixed (09/17/2024)
fix <- read.csv("manual_fix_09_17_2024.csv",header=TRUE)
fix <- fix %>% select(1:4)
colnames(fix) <- c("id_key","article_source_domain","region","search_origin")

article <- article %>% rows_update(fix,by="id_key")

##Fix missing publication years
missing_year <- article %>% filter(is.na(year))

##Compile full search results database
search_results <- article %>% select(id_key,headline,date,year,search_origin,region,source_file) %>% distinct()

##Compile final results set
###Append desired columns for analysis
final_data <- article %>% 
  select(-ground_truth_body_location,-ground_truth_headline_sentiment,-ground_truth_body_subject,-user_classified_body_subject,-user_classified_body_location)
  
final_data <- final_data %>%
  full_join(select(ai_result_body,article_id,value),by=c("id_key"="article_id")) %>% 
  full_join(select(ai_result_headline,article_id,value),by=c("id_key"="article_id")) %>%
  full_join(select(ai_result_location,article_id,value),by=c("id_key"="article_id")) %>%
  unique() %>%
  mutate(value = tolower(value)) %>%
  mutate(value.x = tolower(value.x)) %>%
  mutate(value.y = tolower(value.y))

###Split out predicted geographies discussed in article
s <- strsplit(final_data$value, split=";")
df <- data.frame(ID=rep(final_data$id_key, sapply(s, length)), value = unlist(s))
df$value <- tolower(df$value)
colnames(df) <- c("id_key","topic_location")
df <- distinct(df)
final_data <- left_join(final_data,df,by="id_key") 
final_data <- select(final_data,-value)   

###Rename columns
final_data <- dplyr::rename(final_data,c("relevance"="value.x","sentiment"="value.y"))

##Filter to relevant articles (need to figure out what the NAs are)
rel_set <- filter(final_data, relevance == "other aquaculture" | relevance == "seaweed aquaculture")

##Save analysis sets
saveRDS(search_results,paste("seaweed_searchresults_",Sys.Date(),".rds",sep=""))
saveRDS(final_data,paste("seaweed_fullresults_",Sys.Date(),".rds",sep=""))
saveRDS(rel_set,paste("seaweed_included_",Sys.Date(),".rds",sep=""))

##Remove unneeded dataframes and items
rm(list=c("r","rows","id","i","dom","url_of_article","p","pp","ppp","orig_pub","updated_pub","source","source_final","lll","ai_result_body","ai_result_headline","ai_result_location","alembic_version","article","db","df","s"))

# ###Remove trailing whitespace interfering with filtering, clean up cases, and add unique article ID
# clean_data <- data %>%
#   mutate(headline = trimws(headline)) %>%
#   mutate(headline_sentiment = trimws(headline_sentiment)) %>%
#   mutate(body_subject = trimws(body_subject)) %>%
#   mutate(search_type = trimws(search_type)) %>%
#   mutate(location = tolower(location)) %>%
#   rownames_to_column(var="aid") %>%
#   relocate(aid)
