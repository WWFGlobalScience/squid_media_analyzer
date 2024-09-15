#NOTE:
#File locations need to be changed, each pair/below should be the same file location
#In lines 84/87, 109/112, 134/137, 174/177, 274, 275, 283, 284

library(archiveRetriever)
library(data.table)
library(rvest)
library(tidyverse)
library(stringr)
library(httr)
library(progress)

#load in sources
sources <- read.csv("sources.csv",header=TRUE)

##Set parameters for run
#set where files are stored
folder <- "regional_2015-2023/"
#which batch of source ids are being searched (short name for files)
src <- "ak6-9"
#Years to search
startyear = 2015
endyear = 2023

#####PART 1: Get the parent URLs for all copies of the regional sources saved in the Internet Archive #####
# Create list of regional sources to search
src_list <- c("ak6","ak7","ak8","ak9")
selected_src <- dplyr::filter(sources, id %in% src_list)
regional_sources <- as.character(selected_src$Website)

##Set up search log
log <- data.frame(website=character(),
                  start_year=integer(),
                  end_year=integer(),
                  no_archive_urls=integer(),
                  no_child_links=integer(),
                  no_unique_child_links=integer(),
                  no_scraped=integer(),
                  no_aquaculture=integer(),
                  no_seaweed=integer(),
                  stringsAsFactors = FALSE)


# Extract archived urls for selected time period from the Wayback Machine
archived_urls <- list()

for (url in regional_sources) {
  print(paste0("Extracting archived urls from ", url))
  tryCatch({
    Sys.sleep(10)
    list <- retrieve_urls(url, startDate = paste0(startyear,"-01-01"), endDate = paste0(endyear,"-12-31"))
    Sys.sleep(10)
    archived_urls[[url]] <- list
  }, error = function(e) {
    cat("Error fetching feed from", url, ":", conditionMessage(e), "\n")
  })
}

saveRDS(archived_urls, file = (paste0(folder,"urls_to_scrape-",src,"_",startyear,"_",endyear,".rds")))

##Populate search log
for (i in 1:length(archived_urls)){
  log[i,1] <- paste0(names(archived_urls)[i])
  log[i,2] <- paste0(startyear)
  log[i,3] <- paste0(endyear)
  log[i,4] <- paste0(length(archived_urls[[i]]))
}

#####Part 2: Get the child links for all URLs saved in Part 1 #####
#archived_urls <- readRDS(paste0(folder,"urls_to_scrape-",sources,"_",startyear,"_",endyear,".rds"))

# Unnest the list of urls
archived_urls <- unlist(archived_urls)

#Get all the child links
archived_links <- list()

pb <- progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = length(archived_urls), clear = FALSE, width= 60)

pb$tick(0)

for (url in archived_urls) {
  print(paste0("Extracting archived links from ", url))
  print(paste0("This is link #",match(c(url),archived_urls)," out of ",length(archived_urls)," total links"))
  print(" ")
  pb$tick()
  print(" ")
  
  tryCatch({
    list <- retrieve_links(url)
    Sys.sleep(10)
    archived_links[[url]] <- list
  }, error = function(e) {
    cat("Error fetching links from", url, ":", conditionMessage(e), "\n")
  })
}

#Save the results
saveRDS(archived_links, file = (paste0(folder,"links_to_scrape-",sources,"_",startyear,"_",endyear,".rds")))

#####Part 3: Remove duplicate child links to significantly cut down on Part 4 processing time #####
#archived_links <- readRDS(paste0(folder,"links_to_scrape-",sources,"_",startyear,"_",endyear,".rds"))

#Create a new list with all the links retrieved
links_list <- map(archived_links, ~ .x[, 2])

#Unlist that list to get a character string of links
links <- data.frame(archived_links = unlist(links_list))

# Use str_locate_all to find all occurrences of "http" in each string
all_http_positions <- str_locate_all(links$archived_links, "http")

# Extract the positions of the last "http" in each row
last_http_positions <- sapply(all_http_positions, function(x) tail(x[, 2], 1))

# Create a new column with characters including and following the last "http"
links$original_links <- mapply(substr, links$archived_links, last_http_positions, nchar(links$archived_links))
links$original_links <- paste0("htt", links$original_links)

#Detect and remove duplicates from the websites scraped
links_no_dup <- links[!duplicated(links$original_links), ]

#Save data frame
saveRDS(links_no_dup, file = "C:/Users/readd/Documents/WWF_Consultancy/DanielRead_Deliverable6/Links_to_get_title-SeaweedRegionalSearch2015-2023.rds")

#####Part 4: Extract titles and paragraphs from links #####
#links_no_dup <- readRDS("C:/Users/readd/Documents/WWF_Consultancy/DanielRead_Deliverable6/Links_to_get_title-SeaweedRegionalSearch2015-2023.rds")

links_to_scrape <- as.list(links_no_dup$archived_links)

# Function to extract titles and paragraphs from a given URL with timeout handling
extract_data <- function(url) {
  print(paste0("Scraping url: ", url))
  
  tryCatch({
    # Read HTML from the URL with timeout
    page <- url %>% GET(., timeout(20)) %>% read_html()
    
    # Extract titles
    titles <- page %>%
      html_nodes("title") %>%
      html_text()
    
    # Extract paragraphs
    paragraphs <- page %>%
      html_nodes("p") %>%
      html_text()
    
    # Return a list containing titles and paragraphs
    return(list(title = titles, paragraph = paragraphs, url = url))
  }, error = function(e) {
    # Handle timeout error
    if (inherits(e, "error")) {
      cat("Timeout error occurred for URL:", url, "\n")
    } else {
      # Re-throw other errors
      stop(e)
    }
  })
}

m <- length(links_to_scrape)

results <- pbmclappy(links_to_scrape[1:(m/4)], extract_data, mc.cores=4)
results1 <- pbmclapply(links_to_scrape[((m/4)+1):(m/2)], extract_data, mc.cores=4)
results2 <- pbmclapply(links_to_scrape[((m/2)+1):((m/4)*3)], extract_data, mc.cores=4)
results3 <- pbmclapply(links_to_scrape[(((m/4)*3)+1):m], extract_data,mc.cores=4)

results <- list(results0,results1,results2,results3)

saveRDS(results, file = "C:/Users/readd/Documents/WWF_Consultancy/DanielRead_Deliverable6/results-SeaweedRegionalSearch2015-2023.rds")

#####Part 5: Search for and save articles about aquaculture and seaweed #####
#results <- readRDS("C:/Users/readd/Documents/WWF_Consultancy/DanielRead_Deliverable6/results-SeaweedRegionalSearch2015-2023.rds")

results_df <- rbindlist(results)

collapsed_results <- results_df %>%
  group_by(title, url) %>%
  summarize(paragraph = paste(paragraph, collapse = " ")) 

# Define a function to extract the source website
extract_source_website <- function(url) {
  # Find all occurrences of "http://"
  pattern <- "(?:.*?http://){2}([^/]+)/"
  
  match <- str_match(url, pattern)
  if (!is.na(match[1,2])) {
    return(match[1,2])
  } else {
    return(NA)  # Return NA if no match is found
  }
}

# Define a function to extract the archive date
extract_archive_date <- function(date) {
  # Find all occurrences of "http://"
  extracted <- str_extract(date, "(?<=web/)[^/]+")
  
  return(extracted)
}

# Use the function with dplyr to create a new column
collapsed_results <- collapsed_results %>%
  mutate(source_website = sapply(url, extract_source_website)) %>%
  mutate(archive_date = sapply(url, extract_archive_date)) %>%
  mutate(archive_date = as.POSIXct(archive_date, format = "%Y%m%d%H%M%S")) %>%
  mutate(region = case_when(
    source_website == "mainepublic.org" ~ "Maine",
    source_website == "weeklypacket.com" ~ "Maine",
    source_website == "islandadvantages" ~ "Maine",
    source_website == "mainescoast.com" ~ "Maine",
    source_website == "maineboats.com" ~ "Maine",
    source_website == "couriergazette.com" ~ "Maine",
    source_website == "digitalmaine.com" ~ "Maine",
    source_website == "theforecaster.net" ~ "Maine",
    source_website == "lcnme.com" ~ "Maine",
    source_website == "pointseast.com" ~ "Maine",
    source_website == "workingwaterfront.com" ~ "Maine",
    source_website == "castinepatriot.com" ~ "Maine",
    source_website == "downeast.com" ~ "Maine",
    source_website == "thebollard.com" ~ "Maine",
    source_website == "portlandmonthly.com" ~ "Maine",
    source_website == "themainemag.com" ~ "Maine",
    source_website == "newengland.com" ~ "Maine",
    source_website == "mainebiz.biz" ~ "Maine",
    source_website == "ellsworthamerican.com" ~ "Maine",
    source_website == "pressherald.com" ~ "Maine",
    source_website == "boothbayregister.com" ~ "Maine",
    source_website == "nhpr.org" ~ "New Hampshire",
    source_website == "capeandislands.org" ~ "Massachusetts",
    source_website == "duxburyclipper.com" ~ "Massachusetts",
    source_website == "wgbh.org" ~ "Massachusetts",
    source_website == "bostonglobe.com" ~ "Massachusetts",
    source_website == "alaskabeacon.com" ~ "Alaska",
    source_website == "kucb.org" ~ "Alaska",
    source_website == "ktoo.org" ~ "Alaska",
    source_website == "thecordovatimes.com" ~ "Alaska",
    source_website == "alaska-native-news.com" ~ "Alaska",
    source_website == "nwnewsnetwork.org" ~ "Washington & Oregon",
    source_website == "king5.com" ~ "Washington",
    source_website == "seattletimes.com" ~ "Washington",
    source_website == "vashonbeachcomber.com" ~ "Washington",
    source_website == "kuow.org" ~ "Washington",
    source_website == "opb.org" ~ "Oregon",
    source_website == "oregonlive.com" ~ "Oregon",
    source_website == "koin.com" ~ "Oregon",
    source_website == "kgw.com" ~ "Oregon",
    source_website == "katu.com" ~ "Oregon",
    source_website == "ijpr.org" ~ "Oregon",
    source_website == "theworldlink.com" ~ "Oregon",
    source_website == "wweek.com" ~ "Oregon",
    source_website == "tillamookcountypioneer.net" ~ "Oregon",
    source_website == "calmatters.org" ~ "California",
    source_website == "malibutimes.com" ~ "California",
    source_website == "sfchronicle.com" ~ "California",
    source_website == "latimes.com" ~ "California",
    source_website == "timesofsandiego" ~ "California",
    source_website == "vasiliatimesdelta.com" ~ "California",
    source_website == "dailybreeze.com" ~ "California",
    .default = as.character("Unknown")))

#Create search string looking for everything concerning aquaculture
#This is the Froehlich search string
aquaString <- c("(?i)aquaculture|(?i)marine aquaculture|(?i)offshore aquaculture")

#Create an object that has filtered the results and only includes webpages that include the aquaculture search string in either the title or paragraph
aqua2015_2023 <- collapsed_results %>% filter(str_detect(paragraph, aquaString))

#Save results
saveRDS(aqua2015_2023, file = "C:/Users/readd/Documents/WWF_Consultancy/DanielRead_Deliverable6/aquacultureRegionalSearch2015-2023.rds")
write_csv(aqua2015_2023, "C:/Users/readd/Documents/WWF_Consultancy/DanielRead_Deliverable6/aquacultureRegionalSearch2015-2023.csv")

#Create search string for seaweed aquaculture
seaweedString <- c("((?i)seaweed|(?i)kelp|(?i)sea moss) .* ((?i)aquaculture|(?i)farm*|(?i)cultivat*)")

#SearchFilter the texts for only those in which the aquaculture search terms appear
seaweed2015_2023 <- collapsed_results %>% filter(str_detect(paragraph, seaweedString))

saveRDS(seaweed2015_2023, file = "C:/Users/readd/Documents/WWF_Consultancy/DanielRead_Deliverable6/seaweedRegionalSearch2015-2023.rds")
write_csv(seaweed2015_2023, "C:/Users/readd/Documents/WWF_Consultancy/DanielRead_Deliverable6/seaweedRegionalSearch2015-2023.csv")
