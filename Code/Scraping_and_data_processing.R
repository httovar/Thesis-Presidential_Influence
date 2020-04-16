#SCraping Data from NYT API for date of last Trump SOTU
#library setup
library(nytimes)
library(jsonlite)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(SnowballC)
library(stringr)
library(lubridate)
library(fpp2)



#storing API key
NYTIMES_KEY="Your API Key"

#parameter search date
#Obama was President from 2009-2017. SOTUs are 2012-2016, Search is July 1  2011- Aug 31 2016
#Creating tibble with date ranges for the for loop (see below)
collect_dates = tibble(
  begin_date=str_split(string=seq(from=ISOdate(2011, 08, 01), to=ISOdate(2016, 07,01), by="month"),
                       pattern = " ", simplify = TRUE)[,1],
  end_date=str_split(string=seq(from=ISOdate(2011, 08, 31),  to=ISOdate(2016, 07,31), by="month"),
                     pattern = " ", simplify = TRUE)[,1]
)



#url chunks
baseurl <-"http://api.nytimes.com/svc/search/v2/articlesearch.json?begin_date=" #calls the v2 NYT article API
chunk1 = "&end_date=" #date parameter
chunk2= "&fq=type_of_material:(\"News\") AND (section_name:(\"U.S.\", \"Opinion\")"  #specifies section and material
news_desk_chunk = "OR news_desk:(\"U.S.\", \"National\"))&api-key=" #news desk

##### Initialize Data Set. Do not run 
#all_nyt_data <-setNames(data.frame(matrix(ncol = 30, nrow = 0)), c("response.docs.print_section", "response.meta.offset", "response.meta.hits", 
#                           "response.docs.byline.original", "response.docs.headline.sub", "response.docs.headline.seo",
#                           "response.docs.headline.name", "response.docs.headline.kicker", "response.docs.headline.main",
#                           "response.docs.headline.print_headline", "response.docs.uri", "response.meta.time",
#                           "response.docs.word_count", "response.docs._id", "response.docs.subsection_name",
#                           "response.docs.section_name", "response.docs.news_desk", "response.docs.document_type",
#                           "response.docs.pub_date", "response.docs.source", "response.docs.print_page",
#                           "response.docs.headline.content_kicker", "response.docs.lead_paragraph",
#                           "response.docs.byline.organization", "response.docs.snippet", "response.docs.web_url",
#                           "response.docs.abstract", "copyright", "response.docs.type_of_material", "status"))

#This will run for several hours so do not just run it to test. First adjust dates in the series above.
for(k in 1:nrow(collect_dates)){
  
  
  begin_date <- collect_dates[k,1]
  end_date <- collect_dates[k,2]
  
  #assembling url
  complete_url=paste(baseurl, begin_date, chunk1, end_date, chunk2, news_desk_chunk, NYTIMES_KEY, sep="")
  
  initialQuery <- fromJSON(complete_url)
  maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 
  
  pages <- list()
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(complete_url, "&page=", i), flatten = TRUE) %>% data.frame() 
    message("Retrieving page ", i, " from month ", k)
    pages[[i+1]] <- nytSearch 
    Sys.sleep(10) #Added to prevent NYT API to shut down for "attempting too many queries"
  }
  
  allNYTSearch <- rbind_pages(pages)%>%
    select(everything(), -response.docs.byline.person, -response.docs.multimedia,
           -response.docs.keywords)
  
  all_nyt_data <- rbind(all_nyt_data, allNYTSearch)
  
}

#Writing out path
#nyt_path <- file.path("C:","Users","henni","Documents","App State","Classes","Theses","Thesis - PoliSci","Data",
#                         "Scraped","NYT_API_Data.csv",sep="")
#write_csv(all_nyt_data, nyt_path)


# Data Manipulation####
all_nyt_data%>%
  distinct(response.docs.web_url,.keep_all = T)%>%
  filter(response.docs.section_name != "Opinion" & response.docs.subsection_name == "Politics")%>%
  summarise(total_len = length(response.docs.section_name))

#About 19600 articles in section politics that are not opinion pieces!
all_nyt_data%>%
  distinct(response.docs.web_url,.keep_all = T)%>%
  filter(response.docs.subsection_name == "Politics")%>%
  summarise(total_len = length(response.docs.section_name))

#Cleaning Politics Articles
politics_articles <- all_nyt_data%>%
  distinct(response.docs.web_url,.keep_all = T)%>%
  filter(response.docs.section_name != "Opinion" & response.docs.subsection_name == "Politics")%>%
  select(headline = response.docs.headline.main, abstract = response.docs.abstract,
         snippet = response.docs.snippet, lead_paragraph = response.docs.lead_paragraph,
         pub_date = response.docs.pub_date, author = response.docs.byline.original, section = response.docs.section_name,
         subsection = response.docs.subsection_name, material = response.docs.type_of_material, 
         url = response.docs.web_url, word_count = response.docs.word_count)%>%
  mutate(author = str_replace(string=author, pattern="By ", replacement = ""),
         pub_date = str_replace(string = pub_date, pattern = "T.*", replacement = ""),
         pub_date = ymd(pub_date))%>%
  arrange(pub_date)%>%
  filter(pub_date>="2011-08-01")

nyt_pol_path <- file.path("C:","Users","henni","Documents","App State","Classes","Theses","Thesis - PoliSci","Data",
                          "Scraped","NYT_Political_Articles.csv")
write_csv(politics_articles, nyt_pol_path)


#Aggregating weekly articles
politics_articles%>%
  group_by(pub_date)%>%
  count(material)%>% #Material is always "News" (because of the API Scraping). It thus gives me a way to calculate frequencies
  arrange(desc(n))

#Histogram
politics_articles%>%
  group_by(pub_date)%>%
  count(material)%>%
  ggplot(aes(y=n, x=pub_date))+
  geom_col()+
  labs(title = "Weekly News Reports in New York Times",
       subtitle = "Frequency Counts, Aug 2011 - Jul 2016",
       x="Date",
       y="Frequency")+
  theme_bw()

#Articles by author
politics_articles%>%
  group_by(author)%>%
  count(material)%>%
  arrange(desc(n))%>%
  View()

#Checking out odd authors - "The New York Times" and "First Draft"
politics_articles%>%
  filter(author=="The New York Times")%>%
  View()
#This seems to be series by different authors. For example "The Caucus"

politics_articles%>%
  filter(author=="First Draft")%>%
  View()
#Also a series of articles

