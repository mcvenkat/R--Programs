install.packages("qdap")
install.packages("tidytext")
install.packages("tm")
install.packages("wordcloud")
install.packages("dendextend")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(qdap))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(wordcloud))
suppressPackageStartupMessages(library(dendextend))

bluetheme <- theme(axis.text.x=element_text(angle =90, size=8, vjust = 0.4),
                   plot.title=element_text(size=16, vjust = 2,family = "Avenir Book", face = "bold", margin = margin(b = 20)),
                   axis.title.y = element_text(margin = margin(r = 20)),
                   axis.title.x =element_text(size=12, vjust = -0.35, margin = margin(t = 20)),
                   plot.background = element_rect(fill = "#DEEBF7"),
                   panel.background = element_rect(fill = "#DEEBF7" ),
                   legend.background = element_rect(fill = "#DEEBF7"),
                   legend.title = element_text(size = 10, family = "Avenir Book", face = "bold"),
                   legend.text = element_text(size = 8, family = "Avenir Book"),
                   panel.grid.major = element_line(size = 0.4, linetype = "solid", color = "#cccccc"),
                   panel.grid.minor = element_line(size = 0),
                   axis.ticks = element_blank(),
                   plot.margin = unit(c(0.5, 1, 1, 1), "cm")
)


dtrain <- read.csv("C:/Users/vchakravart3/Documents/train.csv")

#Edgar Allan Poe

#I wanto to first investigate EAP , being more familiar with his works.

dtrain$text <- as.character(dtrain$text)
dtrain$author <- as.character(dtrain$author)
#str(dtrain)
edgar <- dtrain %>% select(author, text) %>% filter(author == "EAP")

#In order to come up with a quick "feel" in our authors text, we will investigate the polarity. 
#"Polarity is often referred to as sentiment analysis, which tells you how positive or
#negative is the text. By analyzing polarity in R with the qdap package, a score will be
#assigned to each sentence and you can analyze the average and standard deviation
#of polarity by groups such as different authors, text, or topics. Different polarity
#dictionaries are available and qdap defaults to one created by Hu and Liu, 2004. You
#can alter or change this dictionary according to your requirements." - Quote taken from Mastering Machine Learning With R, Cory Leismester. 

#I will only analyse the first 1000 texts of EAP for this short exploration. 

bos_pol <- polarity(removePunctuation(removeNumbers(tolower(edgar$text[1:1000]))))
bos_pol

ggplot(bos_pol$all, aes(x = polarity, y = density)) +
  bluetheme + 
  geom_histogram(binwidth = 0.25, fill = "#bada55", colour = "grey60") +
  geom_density(size = 0.75)

plot(bos_pol)

#Surprisingly, EAP text starts with a negative sentiment. We can identify the texts below.

epol.df <-bos_pol$all
which.min(epol.df$polarity)
which.max(epol.df$polarity)

epol.df$text.var[333]

#Well, that's a negative sentiment indeed. You can see keywords "Evil" , "fearfully" and "Wrought". 

epol.df$text.var[487]

#We will use our polarity and make a comparison cloud.

pol_subsections <- function(df) {
x.pos <- subset(df$text, df$polarity > 0)
x.neg <- subset(df$text, df$polarity < 0)
x.pos <- paste(x.pos, collapse = " ")
x.neg <- paste(x.neg, collapse = " ")
all.terms <- c(x.pos, x.neg)
return(all.terms)
}

#At this point you have omitted the neutral sentences and want to focus on organizing the remaining text. In this exercise we use the %>% operator again to forward objects to functions. After some simple cleaning use comparison.cloud() to make the visual.


ed_df <- bos_pol$all %>%
select(text = text.var, polarity = polarity)


P.all_terms <- pol_subsections(ed_df)


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

# Make a comparison cloud
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

#Our common words used by EAP are found on our plot above. Strangely, the keyword "upon" ranks 1st in word usage followed by "one" and "now".

#Next, I want to explore EPA with a frequency analysis. Although not difficult, in sentiment analysis this simple method can be surprisingly illuminating. We will make a barplot. We will use Bing lexicon to construct our visual. The Bing lexicon labels words as positive or negative


#Tidy up the DTM
ed_tidy <- tidy(ed_dtm)

#Examine tidy with a word you saw
#ed_tidy[831:835, ]

#Get Bing lexicon
bing <- get_sentiments("bing")

#Join text to lexicon
ag_bing_words <- inner_join(ed_tidy, bing, by = c("term" = "word"))

#Examine
ag_bing_words

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
ed_tidy_sentiment

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

#We will use the tidytext subjectivity lexicon, "nrc", then perform an inner_join() with our text. The "nrc" lexicon has the 8 emotions plus positive and negative term classes. So you will have to drop positive and negative words after performing your inner_join(). One way to do so is with the negation, !, and grepl().

nrc <- get_sentiments("nrc")

#Inner join
ed_sentiment <- inner_join(ed_tidy, nrc, by = c("term" = "word"))
#Drop positive or negative
ed_pos_neg <- ed_sentiment %>%
  filter(!grepl("positive|negative", sentiment))

#Count terms by sentiment then spread
poe_tidy <- ed_pos_neg %>% 
  count(sentiment, term = term) %>% 
  spread(sentiment, n, fill = 0) %>%
  as.data.frame()
#Set row names
rownames(poe_tidy) <- poe_tidy[, 1]

#Drop terms column
poe_tidy[, 1] <- NULL

#Comparison cloud
comparison.cloud(poe_tidy, max.words = 50, title.size = 1.0)

#Distance matrix and dendrogram 

#A simple way to do word cluster analysis is with a dendrogram on our term-document matrix. Once we have a TDM, we can call dist() to compute the differences between each row of the matrix.

#Next, we will use hclust() to perform cluster analysis on the dissimilarities of the distance matrix. Lastly, we can visualize the word frequency distances using a dendrogram and plot(). Often in text mining, we can get some interesting insights or word clusters based on a dendrogram.

#We will use the dendextend package to assist us by coloring branches and outlining clusters. dendextend is designed to operate on dendrogram objects, so we have to change the hierarchical cluster from hclust using as.dendrogram().

#Create tweets_tdm2
etext_tdm <-removeSparseTerms(e_tm, sparse = 0.975)

#Create tdm_m
tdm_mp <-as.matrix(etext_tdm)

#Create tdm_df
tdm_dfp <-as.data.frame(tdm_mp)

#Create tweets_dist
ptext_dist <-dist(tdm_dfp)

#Load dendextend
#library(dendextend)

#Create hc
hcp <-hclust(ptext_dist)

#Create hcd
hcdp <- as.dendrogram(hcp)

#Print the labels in hcd
labels(hcdp)

#Change the branch color to red for 
hcdp <- branches_attr_by_labels(hcdp,
                                c("will", "great"), "red")

#Plot hcd
plot(hcdp, main = "EAP Dendrogram") 

#Add cluster rectangles 
rect.dendrogram(hcdp, k = 2, border = "grey50")

##Now, we will make same explorations with HPL author.
hpl <- dtrain %>% select(author, text) %>% filter(author == "HPL")

#Create a VectorSource on column 2: vec_source
hvec_source <-VectorSource(hpl[, 2])

#Convert vec_source to a corpus: vec_corpus
hvec_corpus <-VCorpus(hvec_source)
hvec_corpus

hclean_corp <- clean_corpus(hvec_corpus)
#clean_corp[[227]][1]
#clean_corp
hp_dtm <- DocumentTermMatrix(hclean_corp)
hp_mat <- as.matrix(hp_dtm)

#Examine line 2206 and columns 245:250
ed_mat[2206, 245:250]

#TDM
hp_tm <- TermDocumentMatrix(hclean_corp)
hp_tdm <- as.matrix(hp_tm)
e_tdm[148:150, 2587:2590]

#Again, to get a quick feeling on HPL text

hpl_pol <- polarity(removePunctuation(removeNumbers(tolower(hpl$text[1:1000]))))
hpl_pol

ggplot(hpl_pol$all, aes(x = polarity, y = ..density..)) +
  bluetheme + 
  geom_histogram(binwidth = 0.25, fill = "#bada55", colour = "grey60") +
  geom_density(size = 0.75)

plot(hpl_pol)

#The HPL text starts in a negative sentiment. We can identify this text below.
pol.df <-hpl_pol$all
which.min(hpol.df$polarity)
which.max(hpol.df$polarity)
hpol.df$text.var[132]
#Ahh..such negative sentiments. We can see words "dislike" , "suffer" and "die". 
pol.df$text.var[97]

#Comparison cloud
#Add scores to each document line in a data frame
hpl_df <- hpl_pol$all %>%
  select(text = text.var, polarity = polarity)

#Custom function
H.all_terms <- pol_subsections(hpl_df)

#Make a corpus
H.all_corpus <- H.all_terms %>%
  VectorSource() %>% 
  VCorpus()

#Basic TDM
H.all_tdm <- TermDocumentMatrix(
  H.all_corpus,
  control = list(
    removePunctuation = TRUE,
    stopwords = stopwords(kind = "en")
  )
) %>%
  as.matrix() %>%
  set_colnames(c("positive", "negative"))

#Make a comparison cloud
comparison.cloud(
  H.all_tdm,
  max.words = 50,
  colors = c("darkgreen", "darkred")
)
#Words Term Frequency

#Calculate the rowSums: term_frequency
hterm_frequency <- rowSums(hp_tdm)

#Sort term_frequency in descending order
hterm_frequency <- sort(hterm_frequency, decreasing = TRUE)

#View the top 10 most common words
hterm_frequency[1:10]

#Plot a barchart of the 10 most common words
barplot(hterm_frequency[1:10], col = "#3288BD", las = 2)

#HPL author with frequency analysis
#Tidy up the DTM
hp_tidy <- tidy(hp_dtm)

#Examine tidy with a word you saw
ed_tidy[831:835, ]

#Get Bing lexicon
bing <- get_sentiments("bing")

#Join text to lexicon
hp_bing_words <- inner_join(hp_tidy, bing, by = c("term" = "word"))

#Examine
hp_bing_words

#Get counts by sentiment
hp_bing_words %>%
  count(sentiment) 

#Inner join
hp_sent <- inner_join(hp_tidy, bing, by = c("term" = "word")) 

#Tidy sentiment calculation
hp_tidy_sentiment <- hp_sent %>% 
  count(term, sentiment, wt = count) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative)

#Review
hp_tidy_sentiment

#Subset
hp_tidy_small <- hp_tidy_sentiment %>% 
  filter(abs(polarity) >= 50)

#Add polarity
hp_tidy_pol <- hp_tidy_small %>% 
  mutate(
    pol = ifelse(polarity > 0, "positive", "negative")
  )


ggplot(
  hp_tidy_pol, 
  aes(reorder(term, polarity), polarity, fill = pol)
) +
  geom_bar(stat = "identity") + 
  ggtitle("HPL: Sentiment Word Frequency") + 
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1)) + bluetheme + ylab("") + xlab("Words")

#Comparison cloud for Emotional introspection for author HPL
nrc <- get_sentiments("nrc")

#Inner join
hp_sentiment <- inner_join(hp_tidy, nrc, by = c("term" = "word"))

#Drop positive or negative
hp_pos_neg <- hp_sentiment %>%
  filter(!grepl("positive|negative", sentiment))

#Count terms by sentiment then spread
hp_tidy <- hp_pos_neg %>% 
  count(sentiment, term = term) %>% 
  spread(sentiment, n, fill = 0) %>%
  as.data.frame()
#Set row names
rownames(hp_tidy) <- hp_tidy[, 1]

#Drop terms column
hp_tidy[, 1] <- NULL

#Comparison cloud
comparison.cloud(hp_tidy, max.words = 50, title.size = 1.0)

#Create 
htext_tdm <-removeSparseTerms(hp_tm, sparse = 0.975)

#Create tdm_m
tdm_mh <-as.matrix(htext_tdm)

#Create tdm_df
tdm_dfh <-as.data.frame(tdm_mh)

#Create tweets_dist
htext_dist <-dist(tdm_dfh)

#Create hc
hch <-hclust(htext_dist)

#Create hcd
hcdh <- as.dendrogram(hch)

#Print the labels in hcd
labels(hcdp)

#Distance matrix and dendrogram for HPL author

#Change the branch color to red for 
hcdh <- branches_attr_by_labels(hcdh,
                                c("strange", "great"), "red")

#Plot hcd
plot(hcdh, main = "HPL Dendrogram") 

#Add cluster rectangles 
rect.dendrogram(hcdh, k = 2, border = "grey50")

##Now, we proceed to explore author MWS and apply same steps as above.
mws <- dtrain %>% select(author, text) %>% filter(author == "MWS")

#Create a VectorSource on column 2: vec_source
mvec_source <-VectorSource(mws[, 2])

#Convert vec_source to a corpus: vec_corpus
mvec_corpus <-VCorpus(mvec_source)
mvec_corpus

mclean_corp <- clean_corpus(mvec_corpus)
#clean_corp[[227]][1]
clean_corp
mv_dtm <- DocumentTermMatrix(mclean_corp)
mv_mat <- as.matrix(mv_dtm)

#Examine line 2206 and columns 245:250
ed_mat[2206, 245:250]

#TDM
mv_tm <- TermDocumentMatrix(mclean_corp)
mv_tdm <- as.matrix(mv_tm)
e_tdm[148:150, 2587:2590]

#Quick exploration of MWS.
mws_pol <- polarity(removePunctuation(removeNumbers(tolower(mws$text[1:1000]))))
mws_pol


#Plot it
ggplot(mws_pol$all, aes(x = polarity, y = ..density..)) +
  bluetheme + 
  geom_histogram(binwidth = 0.25, fill = "#bada55", colour = "grey60") +
  geom_density(size = 0.75)

plot(mws_pol)

#The MWS text starts in a with a positive sentiment. That's good for our analysis. We can identify the texts below.

mpol.df <-mws_pol$all
which.min(mpol.df$polarity)
which.max(mpol.df$polarity)
mpol.df$text.var[801]
#Ohh..another negative sentiment. You can see the words "remorse", "regret" and "die". 
mpol.df$text.var[63]
#Well, we can see some positive sentiments. We can see the words "love" , "girl" and "reverence". 

#Add scores to each document line in a data frame
mws_df <- mws_pol$all %>%
select(text = text.var, polarity = polarity)

#Custom function
M.all_terms <- pol_subsections(mws_df)

#Make a corpus
M.all_corpus <- M.all_terms %>%
VectorSource() %>% 
VCorpus()

#Basic TDM
M.all_tdm <- TermDocumentMatrix(
M.all_corpus,
control = list(
removePunctuation = TRUE,
stopwords = stopwords(kind = "en")
)
) %>%
as.matrix() %>%
set_colnames(c("positive", "negative"))

# Make a comparison cloud
comparison.cloud(
M.all_tdm,
max.words = 50,
colors = c("darkgreen", "darkred")
)

#Calculate the rowSums: term_frequency
mterm_frequency <- rowSums(mv_tdm)

#Sort term_frequency in descending order
mterm_frequency <- sort(mterm_frequency, decreasing = TRUE)

#View the top 10 most common words
mterm_frequency[1:10]

#Plot a barchart of the 10 most common words
barplot(mterm_frequency[1:10], col = "#3288BD", las = 2)

#Tidy up the DTM
mv_tidy <- tidy(mv_dtm)

#Examine tidy with a word you saw
ed_tidy[831:835, ]

#Get Bing lexicon
bing <- get_sentiments("bing")

#Join text to lexicon
mv_bing_words <- inner_join(mv_tidy, bing, by = c("term" = "word"))

#Examine
mv_bing_words

#Get counts by sentiment
mv_bing_words %>%
count(sentiment) 

#Inner join
mv_sent <- inner_join(mv_tidy, bing, by = c("term" = "word")) 

#Tidy sentiment calculation
mv_tidy_sentiment <- mv_sent %>% 
count(term, sentiment, wt = count) %>%
spread(sentiment, n, fill = 0) %>%
mutate(polarity = positive - negative)

#Review
mv_tidy_sentiment


mv_tidy_small <- mv_tidy_sentiment %>% 
filter(abs(polarity) >= 50)


mv_tidy_pol <- mv_tidy_small %>% 
mutate(
pol = ifelse(polarity > 0, "positive", "negative")
)

#Plot
ggplot(
mv_tidy_pol, 
aes(reorder(term, polarity), polarity, fill = pol)
) +
geom_bar(stat = "identity") + 
ggtitle("MWS: Sentiment Word Frequency") + 
theme(axis.text.x = element_text(angle = 90, vjust = -0.1)) + bluetheme + ylab("") + xlab("Words")

nrc <- get_sentiments("nrc")

#Inner join
mv_sentiment <- inner_join(mv_tidy, nrc, by = c("term" = "word"))

#Drop positive or negative
mv_pos_neg <- mv_sentiment %>%
filter(!grepl("positive|negative", sentiment))

#Count terms by sentiment then spread
mv_tidy <- mv_pos_neg %>% 
count(sentiment, term = term) %>% 
spread(sentiment, n, fill = 0) %>%
as.data.frame()

#Set row names
rownames(mv_tidy) <- mv_tidy[, 1]

#Drop terms column
mv_tidy[, 1] <- NULL

#Comparison cloud
comparison.cloud(mv_tidy, max.words = 50, title.size = 1.0)

#Create tweets_tdm2
mtext_tdm <-removeSparseTerms(mv_tm, sparse = 0.975)

#Create tdm_m
tdm_mw <-as.matrix(mtext_tdm)

#Create tdm_df
tdm_dfw <-as.data.frame(tdm_mw)

#Create tweets_dist
mwtext_dist <-dist(tdm_dfw)

#Create hc
hcmw <-hclust(mwtext_dist)

#Create hcd
hcdw <- as.dendrogram(hcmw)

#Print the labels in hcd
labels(hcdw)

#Change the branch color to red for 
hcdw <- branches_attr_by_labels(hcdw,
c("love", "like"), "red")

#Plot hcd
plot(hcdw, main = "MWS Dendrogram") 

#Add cluster rectangles 
rect.dendrogram(hcdw, k = 2, border = "grey50")

#Lastly, I want to explore by comparing all 3 authors common words

#Create all_poe
all_poe <- paste(edgar$text, collapse = " ")

#Create all_hpl
all_hpl <- paste(hpl$text, collapse = " ")

all_mws <- paste(mws$text, collapse = " ")

#Create all_ext
all_text <- c(all_poe, all_hpl, all_mws)

#Convert to a vector source
all_ath_text <- VectorSource(all_text)

#Create all_corpus
all_a_corpus <- VCorpus(all_ath_text)

#Similar words

#Clean the corpus
all_a_clean <- clean_corpus(all_a_corpus)

#Create all_tdm
all_a_tdm <- TermDocumentMatrix(all_a_clean)

#Create all_m
all_a_m <- as.matrix(all_a_tdm)

# Create comparison cloud
comparison.cloud(all_a_m, colors = c("orange", "blue", "green"), max.words = 100)

#1 - EAP , 2 - HPL , 3 - MWS

all_dtm <- DocumentTermMatrix(all_a_clean)
#dim(all_dtm)
all_dtm <- removeSparseTerms(all_dtm , 0.51)
dim(all_dtm)

rownames(all_dtm) <- c("EAP" , "HPL" , "MWS")
inspect(all_dtm[1:3, 1:5])

#Word Frequency and Topic Tables. I will run some more diagnostics and try to explore if we can find any new information that we do not know.
freq <-colSums(as.matrix(all_dtm))
ord <- order(-freq)

#Examine Head. We see most frequent words used by all authors. The words are the same with our analysis above. 
freq[head(ord)]

#Examine Tail. The least used words for our 3 authors. Can you guess the author who uses these words?
freq[tail(ord)]

head(table(freq))
tail(table(freq))

#What these tables show is the number of words with that specific frequency, so 1277
#words occurred twice and one word, "one" in our case, occurred 1614 times.

#What words occured 500 times. Using findFreqTerms(), we can find what words occurred at least 500 times. 
findFreqTerms(all_dtm, 500)

#The findAssocs command below shows words by correlation coming from our frequent terms.You can check it yourself. I decided to bypass it since it produces a very long window.
#findAssocs(all_dtm, "first", corlimit=0.9)
#findAssocs(all_dtm, "day", corlimit = 0.9)
#I will instead make a barplot to show most frequent words for all authors

freq1 <- sort(colSums(as.matrix(all_dtm)), decreasing = TRUE)
wf <- data.frame(word=names(freq1), freq1,freq1)
wf <- wf[1:10, ]
barplot(wf$freq1, names=wf$word, main="Word Frequency", xlab="Words",
ylab="Counts", ylim=c(0,2000), col = "#3288BD", las = 2)

#Topic models

#"Topic models are a powerful method to group documents by their main topics. Topic
#models allow the probabilistic modeling of term frequency occurrences in documents. The
#fitted model can be used to estimate the similarity between documents as well as between a
#set of specified keywords using an additional layer of latent variables which are referred to as
#topics. (Grun and Hornik, 2011) In essence, a document is assigned to a topic based
#on the distribution of the words in that document, and the other documents in that
#topic will have roughly the same frequency of words.
#The algorithm that we will focus on is Latent Dirichlet Allocation (LDA) with
#Gibbs sampling, which is probably the most commonly used sampling algorithm.
#In building topic models, the number of topics must be determined before running
#the algorithm (k-dimensions)." 
# Quote taken from Mastering Machine Learning With R, Cory Leismester. 

suppressPackageStartupMessages(library(topicmodels))
set.seed(1234)
lda3 <- LDA(all_dtm, k=3, method ="Gibbs")
topics(lda3)

#We can see topics are grouped by every 3 authors

#Words show top 20 per topic. Topic 3 has key words "raymond" , "death", and "will" from MWS . Topic 2 has keywords "old" , " house" and "old" from HPL. Lastly , keywords from Topic 1 are "upon", "great" and "one" from EAP. More or less, we have already seen these words in our analysis above. But at least, by using LDA, we have confirmed that indeed, we expect these kinds of topics from our 3 authors.

