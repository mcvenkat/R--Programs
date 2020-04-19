library(ggplot2)
r  <- seq(0,1,length=201)
th <- seq(0,2*pi, length=201)
d  <- expand.grid(r=r,th=th)
gg <- with(d,data.frame(d,x=r*sin(th),y=r*cos(th),
                        z=hcl(h=360*th/(2*pi),c=100*r, l=65)))
ggplot(gg) +
  geom_point(aes(x,y, color=z), size=3)+
  scale_color_identity()+labs(x="",y="") +
  coord_fixed()


# Create hsv grid
d = expand.grid(h=seq(0,1,0.01), s=seq(0,1,0.05), v=1)

p1 = ggplot() +
  coord_polar(theta="x") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  scale_fill_identity() +
  geom_rect(data=d, mapping=aes(xmin=h, xmax=h+resolution(h), 
                                ymin=s, ymax=s+resolution(s), 
                                fill=hsv(h,s,v)))
png("Colour wheel.png", 2000, 2000) 
p1
plot(p1)
dev.off()

# Make up some hsv colors
colors = data.frame(h=seq(0.1,0.5,length.out=6), 
                    s=seq(0.5,0.9,length.out=6), 
                    v=c(.5,.5,.5,.9,.9,.9))

# Convert to hexadecimal
apply(colors, 1, function(x) hsv(x[1],x[2],x[3]))
# "#806640" "#7A8036" "#50802B" "#3CE642" "#29E68B" "#17E6E6"

# Plot them to see what they look like
plot(1:6,rep(1,6), pch=15, cex=5, col=apply(colors, 1, function(x) hsv(x[1],x[2],x[3])))

#Basic colors
d=data.frame(c=colors(), y=seq(0, length(colors())-1)%%66, x=seq(0, length(colors())-1)%/%66)
ggplot() +
  scale_x_continuous(name="", breaks=NA, expand=c(0, 0)) +
  scale_y_continuous(name="", breaks=NA, expand=c(0, 0)) +
  scale_fill_identity() +
  geom_rect(data=d, mapping=aes(xmin=x, xmax=x+1, ymin=y, ymax=y+1), fill="white") +
  geom_rect(data=d, mapping=aes(xmin=x+0.05, xmax=x+0.95, ymin=y+0.5, ymax=y+1, fill=c)) +
  geom_text(data=d, mapping=aes(x=x+0.5, y=y+0.5, label=c), colour="black", hjust=0.5, vjust=1, size=3)

#HSV
d=expand.grid(h=seq(0,0.95,0.05), s=seq(0,0.95,0.05), v=seq(0,1,0.2))
ggplot() +
  coord_polar(theta="x") +
  facet_wrap(~v) +
  scale_x_continuous(name="hue", limits=c(0,1), breaks=seq(0.025,0.925,0.1), labels=seq(0,0.9,0.1)) +
  scale_y_continuous(name="saturation", breaks=seq(0, 1, 0.2)) +
  scale_fill_identity() +
  geom_rect(data=d, mapping=aes(xmin=h, xmax=h+resolution(h), ymin=s, ymax=s+resolution(s), fill=hsv(h,s,v)), color="white", size=0.1)

#HCL Hue Chroma Luminance
d=expand.grid(h=seq(0,350,10), c=seq(0,100,5), l=seq(0,100,20))
ggplot() +
  coord_polar(theta="x")+facet_wrap(~l) +
  scale_x_continuous(name="hue", limits=c(0,360), breaks=seq(5,345,20), labels=seq(0,340,20)) +
  scale_y_continuous(name="chroma", breaks=seq(0, 100, 20)) +
  scale_fill_identity() +
  geom_rect(data=d, mapping=aes(xmin=h, xmax=h+resolution(h), ymin=c, ymax=c+resolution(c), fill=hcl(h,c,l)), color="white", size=0.1)

#RGB Color Space
d=expand.grid(r=seq(0,1,0.1), g=seq(0,1,0.1), b=seq(0,1,0.1))
ggplot() +
  facet_wrap(~b) +
  scale_x_continuous(name="red", breaks=seq(0.05, 1.05, 0.2), labels=seq(0, 1, 0.2)) +
  scale_y_continuous(name="green", breaks=seq(0.05, 1.05, 0.2), labels=seq(0, 1, 0.2)) +
  scale_fill_identity() +
  geom_rect(data=d, mapping=aes(xmin=r, xmax=r+resolution(r), ymin=g, ymax=g+resolution(g), fill=rgb(r,g,b)), color="white", size=0.1)
library("tidyverse")
library("data.table")
library("viridis")
install.packages("tidytext")
library("tidytext")
# Subset to AFINN
afinn_lex <- get_sentiments("afinn") 

# Count AFINN scores
afinn_lex %>% 
  count("afinn")

afinn_lex  

# Subset to nrc
nrc_lex <- get_sentiments("nrc")

# Print nrc_lex
nrc_lex

# Make the nrc counts object
nrc_counts <- nrc_lex %>% 
  count("nrc")

# Barplot
ggplot(nrc_counts, aes(x = "nrc", y = n))+
  geom_bar(stat = "identity") 

library("magrittr")
install.packages("dplyr")
library("dplyr")
# Join text and lexicon
oz_nrc <- inner_join(oz, nrc, by = c("term" = "word"))

# DataFrame of tally
oz_plutchik <- oz_nrc %>% 
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count))

# Plot the counts
ggplot(oz_plutchik, aes(x = sentiment, y = total_count)) +
  # Add a column geom
  geom_col()

getwd()

install.packages("qdap")
install.packages("wordcloud")
install.packages("tm")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(qdap))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(wordcloud))

#load our data
dtrain <- read.csv("C:/Users/vchakravart3/Documents/train.csv")
#peek on our data
str(dtrain)

dtrain$text <- as.character(dtrain$text)
dtrain$author <- as.character(dtrain$author)
edgar <- dtrain %>% select(author, text) %>% filter(author == "EAP")

bos_pol <- polarity(removePunctuation(removeNumbers(tolower(edgar$text[1:1000]))))
bos_pol

ggplot(bos_pol$all, aes(x = polarity, y = density)) +
  bluetheme + 
  geom_histogram(binwidth = 0.25, fill = "#bada55", colour = "grey60") +
  geom_density(size = 0.75)

epol.df <-bos_pol$all #make dataframe
which.min(epol.df$polarity)
which.max(epol.df$polarity)
epol.df$text.var[333]
epol.df$text.var[487]

#create our pol_subsections function
pol_subsections <- function(df) {
  x.pos <- subset(df$text, df$polarity > 0)
  x.neg <- subset(df$text, df$polarity < 0)
  x.pos <- paste(x.pos, collapse = " ")
  x.neg <- paste(x.neg, collapse = " ")
  all.terms <- c(x.pos, x.neg)
  return(all.terms)
}

#At this point you have omitted the neutral sentences and want to focus on organizing the remaining text. In this exercise we use the %>% operator again to forward objects to functions. After some simple cleaning use comparison.cloud() to make the visual.

# Add scores to each document line in a data frame
ed_df <- bos_pol$all %>%
  select(text = text.var, polarity = polarity)

# Custom function
P.all_terms <- pol_subsections(ed_df)

# Make a corpus
P.all_corpus <- P.all_terms %>%
  VectorSource() %>% 
  VCorpus()

# Basic TDM
P.all_tdm <- TermDocumentMatrix(
  P.all_corpus,
  control = list(
    removePunctuation = TRUE,
    stopwords = stopwords(kind = "en")
  )
) %>%
  as.matrix() %>%
  set_colnames(c("positive", "negative"))

#then finally make our comparison cloud
comparison.cloud(
  P.all_tdm,
  max.words = 50,
  colors = c("darkgreen", "darkred")
)


#Next, I want to check the most frequent used words by EAP. Transforming our text involves at least basic common preprocessing functions include: tolower(): Make all characters lowercase , removePunctuation(): Remove all punctuation marks , removeNumbers(): Remove numbers , stripWhitespace(): Remove excess whitespace

vec_source <-VectorSource(edgar[, 2])
vec_corpus <-VCorpus(vec_source)


clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  #corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  return(corpus)
}

clean_corp <- clean_corpus(vec_corpus)
ed_dtm <- DocumentTermMatrix(clean_corp)
ed_mat <- as.matrix(ed_dtm)

e_tm <- TermDocumentMatrix(clean_corp)
e_tdm <- as.matrix(e_tm)


#Calculate the rowSums: term_frequency
term_frequency <- rowSums(e_tdm)

#Sort term_frequency in descending order
term_frequency <- sort(term_frequency, decreasing = TRUE)

#View the top 10 most common words
term_frequency[1:10]

#Plot a barchart of the 10 most common words
barplot(term_frequency[1:10], col = "#3288BD", las = 2)
```

Our common words used by EPA are found on our plot above. Strangely, the keyword "upon" ranks 1st in word usage followed by "one" and "now".

Next, I want to explore EPA with a frequency analysis. Although not difficult, in sentiment analysis this simple method can be surprisingly illuminating. We will make a barplot. We will use Bing lexicon to construct our visual. The Bing lexicon labels words as positive or negative


```{r, echo = FALSE}
#Tidy up the DTM
ed_tidy <- tidy(ed_dtm)

#Examine tidy with a word you saw
#ed_tidy[831:835, ]

#Get Bing lexicon
bing <- get_sentiments("bing")

#Join text to lexicon
ag_bing_words <- inner_join(ed_tidy, bing, by = c("term" = "word"))

#Examine
#ag_bing_words

#Get counts by sentiment
ag_bing_words %>%
  count(sentiment) 

#Inner join
ed_sent <- inner_join(ed_tidy, bing, by = c("term" = "word")) 

#Tidy sentiment calculation
ed_tidy_sentiment <- ed_sent %>% 
  count(term, sentiment, wt = count) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative)

#Review
#ed_tidy_sentiment

#Subset
ed_tidy_small <- ed_tidy_sentiment %>% 
  filter(abs(polarity) >= 50)

#Add polarity
ed_tidy_pol <- ed_tidy_small %>% 
  mutate(
    pol = ifelse(polarity > 0, "positive", "negative")
  )

ggplot(
  ed_tidy_pol, 
  aes(reorder(term, polarity), polarity, fill = pol)
) +
  geom_bar(stat = "identity") + 
  ggtitle("EAP: Sentiment Word Frequency") + 
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1)) + bluetheme + ylab("") + xlab("Words")

#Our plot above produces the words scored as either positive or negative. From EAP, we can see the keywords "well", "great" and "like" rated as positive. Negative words such as "doubt", "death" and "fell" tops our negative sentiments.

#Next, I want to make a comparison cloud for Emotional introspection.

#We will go beyond subsetting on positive and negative language. Instead we will subset text by each of the 8 emotions in Plutchik's emotional wheel to construct a visual. With this approach, we can get more clarity in word usage by mapping to a specific emotion instead of just positive or negative.

We will use the tidytext subjectivity lexicon, "nrc", then perform an inner_join() with our text. The "nrc" lexicon has the 8 emotions plus positive and negative term classes. So you will have to drop positive and negative words after performing your inner_join(). One way to do so is with the negation, !, and grepl().

#create our pol_subsections function
pol_subsections <- function(df) {
  x.pos <- subset(df$text, df$polarity > 0)
  x.neg <- subset(df$text, df$polarity < 0)
  x.pos <- paste(x.pos, collapse = " ")
  x.neg <- paste(x.neg, collapse = " ")
  all.terms <- c(x.pos, x.neg)
  return(all.terms)
}

#At this point you have omitted the neutral sentences and want to focus on organizing the remaining text. In this exercise we use the %>% operator again to forward objects to functions. After some simple cleaning use comparison.cloud() to make the visual.

# Add scores to each document line in a data frame
ed_df <- bos_pol$all %>%
  select(text = text.var, polarity = polarity)

# Custom function
P.all_terms <- pol_subsections(ed_df)

# Make a corpus
P.all_corpus <- P.all_terms %>%
  VectorSource() %>% 
  VCorpus()

# Basic TDM
P.all_tdm <- TermDocumentMatrix(
  P.all_corpus,
  control = list(
    removePunctuation = TRUE,
    stopwords = stopwords(kind = "en")
  )
) %>%
  as.matrix() %>%
  set_colnames(c("positive", "negative"))

#then finally make our comparison cloud
comparison.cloud(
  P.all_tdm,
  max.words = 50,
  colors = c("darkgreen", "darkred")
)

nrc <- get_sentiments("nrc")

# Inner join
ed_sentiment <- inner_join(epol.df, nrc, by = c("term" = "word"))
# Drop positive or negative
ed_pos_neg <- ed_sentiment %>%
  filter(!grepl("positive|negative", sentiment))

# Count terms by sentiment then spread
poe_tidy <- ed_pos_neg %>% 
  count(sentiment, term = term) %>% 
  spread(sentiment, n, fill = 0) %>%
  as.data.frame()
# Set row names
rownames(poe_tidy) <- poe_tidy[, 1]

# Drop terms column
poe_tidy[, 1] <- NULL

# Comparison cloud
comparison.cloud(poe_tidy, max.words = 50, title.size = 1.0)