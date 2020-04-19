# here is a pdf for mining
url <- "http://www.noisyroom.net/blog/RomneySpeech072912.pdf"
dest <- tempfile(fileext = ".pdf")
download.file(url, dest, mode = "wb")

# set path to pdftotxt.exe and convert pdf to text
exe <- "C:\\Program Files\\xpdfbin-win-3.03\\bin32\\pdftotext.exe"
system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = F)

# get txt-file name and open it
filetxt <- sub(".pdf", ".txt", dest)
shell.exec(filetxt); shell.exec(filetxt) # strangely the first try always throws an error..

# do something with it, i.e. a simple word cloud
library(tm)
library(wordcloud)
install.packages('Rstem')
library(Rstem)

txt <- readLines(filetxt) # don't mind warning..

txt <- tolower(txt)
txt <- removeWords(txt, c("\\f", stopwords()))

corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, removePunctuation)
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
d <- data.frame(freq = sort(rowSums(m), decreasing = TRUE))

# Stem words
d$stem <- wordStem(row.names(d), language = "english")

# and put words to column, otherwise they would be lost when aggregating
d$word <- row.names(d)

# remove web address (very long string):
d <- d[nchar(row.names(d)) < 20, ]

# aggregate freqeuncy by word stem and
# keep first words..
agg_freq <- aggregate(freq ~ stem, data = d, sum)
agg_word <- aggregate(word ~ stem, data = d, function(x) x[1])

d <- cbind(freq = agg_freq[, 2], agg_word)

# sort by frequency
d <- d[order(d$freq, decreasing = T), ]

# print wordcloud:
wordcloud(d$word, d$freq)

# remove files
file.remove(dir(tempdir(), full.name=T)) # remove files


## Retrieve the letters  
library(pdftools)      
library(rvest)       
library(XML)
# Getting & Reading in HTML Letters
urls_77_97 <- paste('http://www.berkshirehathaway.com/letters/', seq(1977, 1997), '.html', sep='')
html_urls <- c(urls_77_97,
               'http://www.berkshirehathaway.com/letters/1998htm.html',
               'http://www.berkshirehathaway.com/letters/1999htm.html',
               'http://www.berkshirehathaway.com/2000ar/2000letter.html',
               'http://www.berkshirehathaway.com/2001ar/2001letter.html')

letters_html <- lapply(html_urls, function(x) read_html(x) %>% html_text())
# Getting & Reading in PDF Letters
urls_03_16 <- paste('http://www.berkshirehathaway.com/letters/', seq(2003, 2016), 'ltr.pdf', sep = '')
pdf_urls <- data.frame('year' = seq(2002, 2016),
                       'link' = c('http://www.berkshirehathaway.com/letters/2002pdf.pdf', urls_03_16))
download_pdfs <- function(x) {
  myfile = paste0(x['year'], '.pdf')
  download.file(url = x['link'], destfile = myfile, mode = 'wb')
  return(myfile)
}
pdfs <- apply(pdf_urls, 1, download_pdfs)
letters_pdf <- lapply(pdfs, function(x) pdf_text(x) %>% paste(collapse=" "))
tmp <- lapply(pdfs, function(x) if(file.exists(x)) file.remove(x)) 
# Combine letters in a data frame
letters <- do.call(rbind, Map(data.frame, year=seq(1977, 2016), text=c(letters_html, letters_pdf)))
letters$text <- as.character(letters$text)

## Load additional required packages
require(tidyverse)
require(tidytext)
require(gplots)
require(SnowballC)
require(sqldf)
theme_set(theme_bw(12))
### pull emotion words and aggregate by year and emotion terms

emotions <- letters %>%
  unnest_tokens(word, text) %>%                           
  anti_join(stop_words, by = "word") %>%                  
  filter(!grepl('[0-9]', word)) %>%
  left_join(get_sentiments("nrc"), by = "word") %>%
  filter(!(sentiment == "negative" | sentiment == "positive")) %>%
  group_by(year, sentiment) %>%
  summarize( freq = n()) %>%
  mutate(percent=round(freq/sum(freq)*100)) %>%
  select(-freq) %>%
  spread(sentiment, percent, fill=0) %>%
  ungroup()
## Normalize data 
sd_scale <- function(x) {
  (x - mean(x))/sd(x)
}
emotions[,c(2:9)] <- apply(emotions[,c(2:9)], 2, sd_scale)
emotions <- as.data.frame(emotions)
rownames(emotions) <- emotions[,1]
emotions3 <- emotions[,-1]
emotions3 <- as.matrix(emotions3)
## Using a heatmap and clustering to visualize and profile emotion terms expression data

heatmap.2(
  emotions3,
  dendrogram = "both",
  scale      = "none",
  trace      = "none",
  key        = TRUE,
  col    = colorRampPalette(c("green", "yellow", "red"))
)

Before <- c("produce",  "produces", "produced", "producing", "product", "products", "production")
wstem <- as.data.frame(wordStem(Before))
names(wstem) <- "After"
pander::pandoc.table(cbind(Before, wstem))

## The code below pulled a list of all common/unique emotion words expressed
## in all possible combinations of the the three heatmap groups
venn_list <- (attr(emo_venn3, "intersection"))
## and then print only the list of unique emotion words expressed in group-5.
print(venn_list$'C')

## Confirmation of unique emotion words in heatmap group-1
group1_U <- as.data.frame(venn_list$'A')
names(group1_U) <- "terms"
uniq1 <- sqldf( "select t1.*, g1.terms
                from emotions_final t1
                left join
                group1_U g1
                on t1.word = g1.terms "
)
uniq1a <- !is.na(uniq1$terms)
uniqs1 <- rep(NA, length(emotions_final))
uniqs1[uniq1a] <- 1
plot(uniqs1, main="Dispersion plot of emotions words \n unique to heatmap group 1 ", xlab="Length (Word count)", ylab=" ", col="red", type='h', ylim=c(0,1), yaxt='n')

## confirmation of unique emotion words in heatmap group-5  
group5_U <- as.data.frame(venn_list$'C')
names(group5_U) <- "terms"
uniq5 <- sqldf( "select t1.*, g5.terms
from emotions_final t1
left join
group5_U g5
on t1.word = g5.terms "
)
uniq5a <- !is.na(uniq5$terms)
uniqs5 <- rep(NA, length(emotions_final))
uniqs5[uniq5a] <- 1

plot(uniqs5, main="Dispersion plot of emotions words \n unique to heatmap group 5 ", xlab="Length (Word count)", ylab=" ", col="

## confirmation of unique emotion words in heatmap group-4 
group4_U <- as.data.frame(venn_list$'B')
names(group4_U) <- "terms"
uniq4 <- sqldf( "select t1.*, g4.terms
     from emotions_final t1
     left join
     group4_U g4
     on t1.word = g4.terms "
)
uniq4a <- !is.na(uniq4$terms)
uniqs4 <- rep(NA, length(emotions_final))
uniqs4[uniq4a] <- 1

plot(uniqs4, main="Dispersion plot of emotions words \n unique to heatmap group 4 ", xlab="Length (Word count)", ylab=" ", col="red", type='h', ylim=c(0,1), yaxt='n')

## You need to first download the raw data before running the code to recreate the graph below. 
ggplot(sp500[50:89,], aes(x=year, y=return, colour=return>0)) +
geom_segment(aes(x=year, xend=year, y=0, yend=return),
size=1.1, alpha=0.8) +
geom_point(size=1.0) +
xlab("Investment Year") +
     ylab("S&P500 Annual Returns") +
     labs(title="Annual Returns on Investment in S&P500", subtitle= "source: http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/histretSP.html") + 
     theme(legend.position="none") +
     coord_flip()