install.packages("pdftools")
library(pdftools)
download.file("https://github.com/Huitziii/crispy-pdf/raw/master/71_PV.62.pdf",
              "./71_PV.62.pdf")
text <- pdf_text("./71_PV.62.pdf")
??pdftools
text2 <- strsplit(text, "\n")
head(text2[[1]])
install.packages("tm")
library(tm)
read <- readPDF(control = list(text = "-layout"))

document <- Corpus(URISource("./71_PV.62.pdf"), readerControl = list(reader = read))
doc <- content(document[[1]])
head(doc)

page_breaks <- grep("\\f", doc)
doc[page_breaks[1]]

# Remove header of the first page
president_row <- grep("^President:", doc)[1]
doc <- doc[(president_row + 1):length(doc)]

# Remove footer on first page
footer_row_1 <- grep("This record contains the text of speeches ", doc)[1]
footer_row_2 <- grep("\\f", doc)[1] - 1
doc <- doc[- c(footer_row_1:footer_row_2)]

# Remove headers on other pages
header_rows <- grep("^\\f", doc) # Remember: \f are for page breaks
doc[header_rows] <- "page" # I put a marker here that will be useful later
doc <- doc[- (header_rows - 1)]

library(stringr)
doc_split <- strsplit(doc, "  ") # Split each row whene there are 2 spaces
doc_split <- lapply(doc_split, function(x) {
  # For each element, extract:
  #    - doc1 that is the first column. 
  #    - doc2 that is the second column.
  doc1 <- x[1:8][x[1:8] != ""][1] # The first piece of text that's not empty
  if (is.na(doc1)) doc1 <- ""
  # doc2 takes the next non-empty piece of text
  doc2 <- x[x != ""] 
  if (doc1 != "") doc2 <- doc2[-1]
  if (length(doc2) == 0) doc2 <- ""
  # Sometimes there is more text needed to be extracted. 
  # I try to give it to either doc1 or doc2 depending on the size of it.
  while (sum(nchar(doc2)) > 65) {
    doc1 <- paste(doc1, doc2[1], collapse = " ")
    doc2 <- doc2[-1]
  }
  # Clean it before returning it
  doc2 <- paste(doc2, collapse = " ")
  doc1 <- str_trim(doc1) # stringr::str_trim trim the spaces before/after
  doc2 <- str_trim(doc2)
  list(doc1 = doc1, doc2 = doc2)
})
doc1 <- sapply(doc_split, `[[`, 1) # First column
doc2 <- sapply(doc_split, `[[`, 2) # Second column

# Vector of the page breaks coordinates:
pages_rows <- c(0, which(doc1 == "page"), length(doc1))
doc <- c()
# Page by page, we fill a new vector:
for (i in 1:(length(pages_rows) - 1)) {
  doc <- c(doc, c(doc1[(pages_rows[i] + 1):pages_rows[i + 1]],
                  doc2[(pages_rows[i] + 1):pages_rows[i + 1]]))
}
doc <- doc[doc != "page"]

speakers_rows <- grep("Mrs?\\..+\\(", doc)
president_rows <- c(grep("The President:", doc),
                    grep("The Acting President:", doc))
all_rows <- sort(c(speakers_rows, president_rows))

speeches <- list()
for (i in 1:(length(all_rows) - 1)) {
  start <- all_rows[i]
  if (!start %in% speakers_rows) next
  stop <- all_rows[i + 1] - 1
  
  speeches <- append(speeches, list(doc[start:stop]))
}
speeches[[1]]
