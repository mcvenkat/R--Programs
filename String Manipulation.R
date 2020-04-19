#String Manipulation
paste("toto","tata",sep=' ')
paste("toto","tata",sep=",")
install.packages("stringr")
library(stringr)
str_c("toto","tata",sep=",")
x <- c("a","b","c")
paste(x,collapse=" ")
str_c(x, collapse = " ")
cat(c("a","b","c"), sep = "+")

unlist(strsplit("a.b.c", "\\."))
install.packages("tau")
library(tau)
tokenize("abc defghk")

nchar("abcdefghijklmnopqrstuvwxyz")

string <- "23 mai 2000"
string2 <- "1 mai 2000"
regexp <- "([[:digit:]]{2}) ([[:alpha:]]+) ([[:digit:]]{4})"
grepl(pattern = regexp, x = string)
str_detect(string, regexp)
grepl(pattern = regexp, x = string2)

string <- "able was I ere I saw Elba"
textcnt(string,n=1L,method="string")

install.packages("cwhmisc")
library(cwhmisc)
cpos("abcdefghijklmnopqrstuvwxyz","p",start=1)
substring.location("abcdefghijklmnopqrstuvwxyz","def")
substring("abcdefghijklmnopqrstuvwxyz","def")
substr("abcdefghijklmnopqrstuvwxyz",1,5)

library("stringr")
regexp <- "([[:digit:]]{2}) ([[:alpha:]]+) ([[:digit:]]{4})"
string <- "blabla 23 mai 2000 blabla 18 mai 2004"
str_extract(string,regexp)
str_extract_all(string,regexp)
str_match(string,regexp)
str_match_all(string,regexp)

install.packages("caroline")
library(caroline)
m(pattern = regexp, vect = string, names = c("day","month","year"), types = rep("character",3))

string <- "23 mai 2000"
regexp <- "([[:digit:]]{2}) ([[:alpha:]]+) ([[:digit:]]{4})"
sub(pattern = regexp, replacement = "\\1", x = string) # returns the first part of the regular expression
sub(pattern = regexp, replacement = "\\2", x = string) # returns the second part
sub(pattern = regexp, replacement = "\\3", x = string) # returns the third part

text <- "abc def ghk"
sub(pattern = " ", replacement = "",  x = text)
gsub(pattern = " ", replacement = "",  x = text)

