install.packages("rvest")
library(rvest)
install.packages("curl")
library(curl)
install.packages("Rcrawler")
ipl2008<- read_html(curl("http://www.howstat.com/cricket/Statistics/Matches/MatchScorecard.asp?MatchCode=0001",handle=curl::new_handle("useragent" = "Chrome")))
ipl2008 %>% 
html_node(".TextBlackBold8",) %>%
html_text()%>%
as.character()

install.packages("rvest")
library(rvest)

install.packages("Rcrawler")
library(Rcrawler)

url = "http://www.howstat.com/cricket/Statistics/Matches/MatchScorecard.asp?MatchCode=0001"

url= "http://www.cricbuzz.com/live-cricket-scorecard/16725/syt-vs-sys-1st-match-big-bash-league-2016-17"

url="https://www.cardekho.com/compare/maruti-celerio-and-renault-kwid-and-tata-tiago-and-tata-tigor.htm"

url="https://www.carwale.com/comparecars/marutisuzuki-ignis-vs-tata-tiago-vs-datsun-redi-go-vs-mahindra-kuv100-2016-2017/?c1=4660&c2=5109&c3=5026&c4=5363&source=17"

download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
content <- read_html("scrapedpage.html")
content%>%
html_node("#fuelTypeHideContent+ tr td:nth-child(4)  td:nth-child(5)  ") %>%
  html_text()%>%
  as.character()

getwd()



?Rcrawler
Rcrawler("https://www.carwale.com/comparecars/"
         ,no_cores = 4,no_conn=4,DIR="C:/Users/vchakravart3/Documents",
         KeywordsFilter = c("170 parameters"))



x = readLines('http://www.imdb.com/chart/')
> grep('Rank',x)

download.file('http://finance.yahoo.com/q?s=aapl&x=0&y=0','quote.html')
x = readLines('quote.html')
grep('Last Price',x)
nchar(x[43])

gregexpr('Last Price',x[43])

substring(x[43],135512,204087)

getquote = function(sym){
      baseurl = 'https://finance.yahoo.com/quote/'
      myurl = paste(baseurl,sym,'?ltr=1',sep='')
      x = readLines(myurl)
      q = gsub('^.*<big><b><span [^>]*>([^<]*)</span>.*$','\\1',grep('Last Price',x,value=TRUE))
      as.numeric(q)
}

getquote('aapl')


gethistory = function(symbol){
  data = read.csv(paste('http://ichart.finance.yahoo.com/table.csv?s=',symbol,sep=''))
  data$Date = as.Date(data$Date)
  data
}

aapl = gethistory('aapl')

#Reading user input

readinteger <- function()
{ 
  n <- readline(prompt="Enter an integer: ")
  if(!grepl("^[0-9]+$",n))
  {
    return(readinteger())
  }
  
  return(as.integer(n))
}

print(readinteger())


#Quickly reading very large tables as dataframes
library(data.table)
# Demo speedup
n=1e6
DT = data.table( a=sample(1:1000,n,replace=TRUE),
                 b=sample(1:1000,n,replace=TRUE),
                 c=rnorm(n),
                 d=sample(c("foo","bar","baz","qux","quux"),n,replace=TRUE),
                 e=rnorm(n),
                 f=sample(1:1000,n,replace=TRUE) )
DT[2,b:=NA_integer_]
DT[4,c:=NA_real_]
DT[3,d:=NA_character_]
DT[5,d:=""]
DT[2,e:=+Inf]
DT[3,e:=-Inf]

#standard read.table
write.table(DT,"test.csv",sep=",",row.names=FALSE,quote=FALSE)
cat("File size (MB):",round(file.info("test.csv")$size/1024^2),"\n")    
## File size (MB): 51 

system.time(DF1 <- read.csv("test.csv",stringsAsFactors=FALSE))        
##    user  system elapsed 
##   24.71    0.15   25.42
# second run will be faster
system.time(DF1 <- read.csv("test.csv",stringsAsFactors=FALSE))        
##    user  system elapsed 
##   17.85    0.07   17.98

#optimized read.table
system.time(DF2 <- read.table("test.csv",header=TRUE,sep=",",quote="",  
                              stringsAsFactors=FALSE,comment.char="",nrows=n,                   
                              colClasses=c("integer","integer","numeric",                        
                                           "character","numeric","integer")))


##    user  system elapsed 
##   10.20    0.03   10.32
fread
require(data.table)
system.time(DT <- fread("test.csv"))                                  
##    user  system elapsed 
##    3.12    0.01    3.22
sqldf
require(sqldf)

system.time(SQLDF <- read.csv.sql("test.csv",dbname=NULL))             

##    user  system elapsed 
##   12.49    0.09   12.69

# sqldf as on SO

f <- file("test.csv")
system.time(SQLf <- sqldf("select * from f", dbname = tempfile(), file.format = list(header = T, row.names = F)))

##    user  system elapsed 
##   10.21    0.47   10.73
ff / ffdf
require(ff)

system.time(FFDF <- read.csv.ffdf(file="test.csv",nrows=n))   
##    user  system elapsed 
##   10.85    0.10   10.99
#In summary:
  ##    user  system elapsed  Method
  ##   24.71    0.15   25.42  read.csv (first time)
  ##   17.85    0.07   17.98  read.csv (second time)
  ##   10.20    0.03   10.32  Optimized read.table
  ##    3.12    0.01    3.22  fread
  ##   12.49    0.09   12.69  sqldf
  ##   10.21    0.47   10.73  sqldf on SO
  ##   10.85    0.10   10.99  ffdf

