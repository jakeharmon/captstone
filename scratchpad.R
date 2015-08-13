rm(list=ls())
library(NLP)
library(openNLP)
library(openNLPmodels.en)
library(tm)
library(LaF)
options( java.parameters = "-Xmx16g" )
library(RWeka)
library(stringr)
library(plyr)
library(data.table)

set.seed(2)
conBlog <- file("final/en_US/en_US.blogs.txt","r")
blogAll <- readLines(conBlog)

conTwitter <- file("final/en_US/en_US.twitter.txt","r")
twitterAll <- readLines(conTwitter)

conNews <- file("final/en_US/en_US.news.txt","r")
newsAll <- readLines(conNews)

rm(list = c("conBlog","conNews", "conTwitter"))

dtBlogAll <- data.table(blogAll)
dtTwitterAll <- data.table(twitterAll)
dtNewsAll <- data.table(newsAll)

rm(list = c("blogAll","newsAll", "twitterAll"))

dtAll <- rbindlist(list(dtBlogAll,dtTwitterAll,dtNewsAll))
rm(list = c("dtBlogAll","dtTwitterAll","dtNewsAll"))



blogLineCount <- determine_nlines("final/en_US/en_US.blogs.txt")

blogSample <- sample_lines("final/en_US/en_US.blogs.txt", .1*blogLineCount)

twitterLineCount <- determine_nlines("final/en_US/en_US.twitter.txt")

twitterSample <- sample_lines("final/en_US/en_US.twitter.txt", .1*twitterLineCount)

newsLineCount <- determine_nlines("final/en_US/en_US.news.txt")

newsSample <- sample_lines("final/en_US/en_US.news.txt", .1*newsLineCount)

writeLines(blogSample, con="sample/en_US.blogs.sample.txt")
writeLines(twitterSample, con="sample/en_US.twitter.sample.txt")
writeLines(newsSample, con="sample/en_US.news.sample.txt")

# src <- DirSource(directory = "sample",mode = "text")
# src <- DataframeSource(dtAll)
src <- DirSource(directory = "final/en_US/",mode = "text")

corpus <- Corpus(src)

# corpus.tagged <- lapply(as.list(head(blogSample)), function(x){
#   sent_token_annotator <- Maxent_Sent_Token_Annotator()
#   word_token_annotator <- Maxent_Word_Token_Annotator()
#   pos_tag_annotator <- Maxent_POS_Tag_Annotator()
#   y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
#   y2 <- annotate(x, pos_tag_annotator, y1)
#   #  y3 <- annotate(x, Maxent_POS_Tag_Annotator(probs = TRUE), y1)
#   y2w <- subset(y2, type == "word")
#   tags <- sapply(y2w$features, '[[', "POS")
#   r1 <- sprintf("%s/%s", x[y2w], tags)
#   r2 <- paste(r1, collapse = " ")
#   return(r2)   }  )
  

RemoveNonAlphaNumeric <- function(x) gsub("[^a-zA-z\\- ]","",x)
# Remove entire row if there is profanity from the training set.  That way, the prediction model will still work 
# even if POS is part of it.
RemoveBadWords <-
connection <- file("badwords.txt", "r")
badwords <- readLines(connection,warn = F)
options(mc.cores=1)
corpus <- removeWords(corpus[[1]], badwords)
corpus <- tm_map(corpus, content_transformer(FUN = RemoveNonAlphaNumeric))
corpus <- tm_map(corpus, content_transformer(tolower))

corpus <- tm_map(corpus, stripWhitespace)

#corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, removeWords,)  


BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
PentgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))


tdmUni <- TermDocumentMatrix(corpus, control = list(tokenize=WordTokenizer))
tdmBi <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
tdmTri <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))
tdmQuad <- TermDocumentMatrix(corpus, control = list(tokenize = QuadgramTokenizer))
tdmPent <- TermDocumentMatrix(corpus, control = list(tokenize = PentgramTokenizer))

tdmUni<-removeSparseTerms(x = tdmUni,sparse = .34)
tdmBi<-removeSparseTerms(x = tdmBi,sparse = .34)
tdmTri<-removeSparseTerms(x = tdmTri,sparse = .34)
tdmQuad<-removeSparseTerms(x = tdmQuad,sparse = .34)
tdmPent<-removeSparseTerms(x = tdmPent,sparse = .34)

freqtdmUni <- sort(rowSums(as.matrix(tdmUni)), decreasing=TRUE)
freqtdmBi <- sort(rowSums(as.matrix(tdmBi)), decreasing=TRUE)
freqtdmTri <- sort(rowSums(as.matrix(tdmTri)), decreasing=TRUE)
freqtdmQuad <- sort(rowSums(as.matrix(tdmQuad)), decreasing=TRUE)
freqtdmPent <- sort(rowSums(as.matrix(tdmPent)), decreasing=TRUE)



# get probablity of "great" as second word in a bigram.

dfUni <- data.frame(cbind(names(freqtdmUni), freqtdmUni))
names(dfUni) <- c("structure","count")
dfUni$count <- as.integer(dfUni$count)
row.names(dfUni) <- NULL

dfBi <- as.data.frame(cbind(names(freqtdmBi), freqtdmBi))
names(dfBi) <- c("structure","count")
dfBi$count <- as.integer(dfBi$count)
row.names(dfBi) <- NULL

dfTri <- as.data.frame(cbind(names(freqtdmTri), freqtdmTri))
names(dfTri) <- c("structure","count")
dfTri$count <- as.integer(dfTri$count)
row.names(dfTri) <- NULL

dfQuad <- as.data.frame(cbind(names(freqtdmQuad), freqtdmQuad))
names(dfQuad) <- c("structure","count")
dfQuad$count <- as.integer(dfQuad$count)
row.names(dfQuad) <- NULL

dfPent <- as.data.frame(cbind(names(freqtdmPent), freqtdmPent))
names(dfPent) <- c("structure","count")
dfPent$count <- as.integer(dfPent$count)
row.names(dfPent) <- NULL

#rm(list=c("freqtdmUni","freqtdmBi","freqtdmTri","freqtdmQuad","freqtdmPent"))

# firstWord <- "deal"
# structs <- subset(dfBi,grepl(paste0("^",firstWord," ") ,dfBi$structure))
# structs <- structs[with(structs,order(-count)),]
# sumstructs <- sum(subset(dfBi,grepl(paste0("^",firstWord," ") ,dfBi$structure),"count"))
# probs <- cbind(structs, prob = structs$count/sumstructs)
# probs
# 
# firstWord <- "deal"
# secondWord <- "of"
# structs <- subset(dfTri,grepl(paste0("^",firstWord," ",secondWord) ,dfTri$structure))
# structs <- structs[with(structs,order(-count)),]
# sumstructs <- sum(subset(dfTri,grepl(paste0("^",firstWord," ",secondWord) ,dfTri$structure),"count"))
# probs <- cbind(structs, prob = structs$count/sumstructs)
# probs