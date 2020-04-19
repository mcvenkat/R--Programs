library(dplyr)
install.packages('DT')
install.packages('wordcloud')
install.packages('tidyverse')
install.packages('plotly')
library(jsonlite)
library(DT)
library(wordcloud)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(plotly)

movies <- read.csv("tmdb_5000_movies.csv", header = TRUE, stringsAsFactors = FALSE)

#Preview of first 100 rows of original data


class(movies)

dim(movies)

colnames(movies)

colnames(movies)[4] <- "movie_id"    #Renaming id column as movie_id

datatable(head(movies, n = 100))

#Assign NA to blank values
movies[movies == ""] <- NA
movies[movies == "[]"] <- NA
movies[movies == 0] <- NA

sum(complete.cases(movies))

#Counting number of missing values in each column
movies %>% summarise_all(funs(sum(is.na(.))))

#Checking for duplicate movie title and removing duplicate values
movies <- movies[!duplicated(movies$title), ]
dim(movies)

#Removing spurious characters from movie title
movies$title <- sub(pattern = "Â", "", movies$title)

  movies$year <- movies$release_date %>%
    as.POSIXlt(tz = "", format = "%m/%d/%Y") %>%
    year()

#Adding new columns gross and gross_flag 
movies <- movies %>%
  mutate(gross = revenue - budget, gross_flag = ifelse(gross < 0, "Loss", "Profit"))


#This code has been inspired from kernel 'Tidydata Movie Data set exploration' from Kaggle
#Creating a tibble, Keywords1, which stores keywords
keywords1 <- movies %>%    
  filter(nchar(keywords) > 2) %>%                 # fiter out blank keywords field
  mutate(                                         # create a new field 
    js = lapply(keywords, fromJSON)               # containing a LIST of keyword and value pairs
  ) %>%                                           # called id and name
  unnest(js) %>%                                  # turn each keyword/value pairs in the LIST into a row
  select(movie_id, title, keywords = name)

#Combining the keywords of a movie in a single column
keywords <- aggregate(keywords ~.,data = keywords1, paste, collapse = ",")

#Creating a tibble, genres1, which stores genres
genres1 <- movies %>%    
  filter(nchar(genres) > 2) %>%                   
  mutate(                                          
    js = lapply(genres, fromJSON)                 
  ) %>%                                           
  unnest(js) %>%                                  
  select(movie_id, title, genres = name) 

#Combining genres of a movie in a single column
genres <- aggregate(genres ~.,data = genres1, paste, collapse = ",")

#Creating a tibble, production_companies1, which stores production companies
production_companies1 <- movies %>%    
  filter(nchar(production_companies) > 2) %>%     
  mutate(                                         
    js = lapply(production_companies, fromJSON)   
  ) %>%                                           
  unnest(js) %>%                                  
  select(movie_id, title, production_companies = name)

#Combining production_companies of a movie in a single column
production_companies <- aggregate(production_companies ~.,data = production_companies1, paste, collapse = ",")

#Creating a tibble, production_countries1, which stores production countries
production_countries1 <- movies %>%    
  filter(nchar(production_countries) > 2) %>%     
  mutate(                                         
    js = lapply(production_countries, fromJSON)   
  ) %>%                                          
  unnest(js) %>%                                  
  select(movie_id, title, production_countries = name) 

#Combining production_countries of a movie in a single column
production_countries <- aggregate(production_countries ~.,data = production_countries1, paste, collapse = ",")


#Creating a tibble, spoken_languages1, which stores languages of the movies
spoken_languages1 <- movies %>%    
  filter(nchar(spoken_languages) > 2) %>%        
  mutate(                                         
    js = lapply(spoken_languages, fromJSON)      
  ) %>%                                          
  unnest(js) %>%                                 
  select(movie_id, title, spoken_languages = iso_639_1) 

#Combining spoken_languages of a movie in a single column
spoken_languages <- aggregate(spoken_languages ~.,data = spoken_languages1, paste, collapse = ",")

#Dropping existing columns - keywords, genres, production_companies, production_countries, spoken_languages
movies <- movies %>%
  select(budget, homepage, movie_id, original_language, original_title, overview, popularity, release_date,
         revenue, runtime, status, tagline, title, vote_average, vote_count, year, gross, gross_flag)

#Attaching columns - keywords, genres, production_companies, production_countries, spoken_languages using full_join in order to retain all observations.
movies <- movies %>%
  full_join(keywords, by = c("movie_id", "title")) %>%
  full_join(genres, by = c("movie_id", "title")) %>%
  full_join(production_companies, by = c("movie_id", "title")) %>%
  full_join(production_countries, by = c("movie_id", "title")) %>%
  full_join(spoken_languages, by = c("movie_id", "title"))

datatable(head(movies, n = 100))


#Function to count number of genres
number <- function(df, col) {
  df_count <- group_by(df, df[[col]]) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  
  names(df_count)[1] <- "df"                  #renaming column
  
  return(df_count)
}

genres_count <- number(genres1, 3)


#Generating word cloud
set.seed(42)
wordcloud(words = genres_count$df, freq = genres_count$count,
          random.order = FALSE, rot.per = 0.20, 
          colors = brewer.pal(8, "Dark2"))

#Importing language file
language <- read.csv("ISO_639_1.csv", header = TRUE)
colnames(language) <- c("language_code", "Language")

colnames(spoken_languages1)[3] <- "language_code"

spoken_languages1 <- left_join(spoken_languages1, language, by = "language_code")

#Obtaining count of languages using function 
language_count <- number(spoken_languages1, 4)

#Wordcloud of languages
wordcloud(words = language_count$df, freq = language_count$count, 
          min.freq = 5, random.order = FALSE, rot.per = 0.25, 
          colors = brewer.pal(8, "Dark2"))

#Run time over the years
movies %>%
  group_by(year) %>%
  summarise(avg_runtime = mean(runtime)) %>%
  ggplot(aes(x = year, y = avg_runtime)) +
  geom_point() +
  xlab("Year") +
  ylab("Average Runtime")

#Count of movies by production countries
country_count <- number(production_countries1, 3)

#Top 10 production countries
country_count %>%
  arrange(desc(count)) %>%
  head(n = 10) %>%
  ggplot(aes(x = reorder(df, -count), y = count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 movie producing countries") +
  xlab("Country") +
  ylab("Count of Movies")

#Count of movies by production_companies
company_count <- number(production_companies1, 3)

#Top 10 production companies
company_count %>%
  head(n = 10) %>%
  ggplot(aes(x = reorder(df, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steel blue") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Top 10 movie producing companies") +
  xlab("Production companies") +
  ylab("Count of Movies")

#vote_average and vote_count over the years
movies %>%
  filter(vote_count >= 1000) %>%
  group_by(year) %>%
  summarise(avg_vote = mean(vote_average)) %>%
  ggplot(aes(x = year, y = avg_vote)) +
  geom_point() +
  geom_smooth(stat = "smooth", position = "identity") +
  xlab("Year") +
  ylab("Average Vote")

#Trend analysis of movies over the years
movies %>%
  group_by(year) %>%
  summarise(movie_count = n()) %>%
  filter(movie_count >= 10)  %>%
  ggplot(aes(x = year, y = movie_count)) +
  geom_line() +
  geom_point() +
  xlab("Year") +
  ylab("Number of Movies") +
  theme_classic()

