install.packages("knitr")
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(knitr)
library(magrittr)

cogapikey<-"XXX"

text=c("is this english?"
       ,"tak er der mere kage"
       ,"merci beaucoup"
       ,"guten morgen"
       ,"bonjour"
       ,"merde"
       ,"That's terrible"
       ,"R is awesome")

# Put data in an object that converts to the expected schema for the API
data_frame(text) %>% 
  mutate(id=row_number()) ->
  textdf

textdf %>% 
  list(documents=.) ->
  mydata

cogapi<-"https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/languages?numberOfLanguagesToDetect=1"

cogapi %>% 
  POST(add_headers(`Ocp-Apim-Subscription-Key`=cogapikey),
       body=toJSON(mydata)) ->
  response

# Process response
response %>% 
  content() %>%
  flatten_df() %>% 
  select(detectedLanguages) %>% 
  flatten_df()->
  respframe

textdf %>% 
  mutate(language= respframe$iso6391Name) ->
  textdf

# New info
mydata<-list(documents = textdf)

# New endpoint
cogapi<-"https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/sentiment"

# Construct a request
cogapi %>% 
  POST(add_headers(`Ocp-Apim-Subscription-Key`=cogapikey),
       body=toJSON(mydata)) ->
  response

# Process response
response %>% 
  content() %>%
  flatten_df() %>% 
  mutate(id=as.numeric(id))-> 
  respframe

# Combine
textdf %>%
  left_join(respframe) ->
  textdf

knitr::kable(textdf)