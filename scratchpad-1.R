rm(list=ls())
library(NLP)
library(tm)
library(LaF)
options( java.parameters = "-Xmx8g" )
library(rJava)
(.jinit(parameters = "-Xmx8g"))
library(RWeka)
library(data.table)
library(ggplot2)
set.seed(165)
blogLineCount <- determine_nlines("final/en_US/en_US.blogs.txt")
blogSample <- sample_lines("final/en_US/en_US.blogs.txt", .1*blogLineCount)

twitterLineCount <- determine_nlines("final/en_US/en_US.twitter.txt")
twitterSample <- sample_lines("final/en_US/en_US.twitter.txt", .25*twitterLineCount)


newsLineCount <- determine_nlines("final/en_US/en_US.news.txt")
newsSample <- sample_lines("final/en_US/en_US.news.txt", .25*newsLineCount)

#  twitter <- readLines("final/en_US/en_US.twitter.txt",)
#  blog <- readLines("final/en_US/en_US.blogs.txt")
#  news <- readLines("final/en_US/en_US.news.txt",skipNul = F)
#  dictionary <- readLines("dictionary_en.txt",skipNul = T)

# dt.all_words <- fread("all_word_counts.txt",sep = " ")
# setnames(dt.all_words,c("count","word"))
# setcolorder(dt.all_words,c("word","count"))
# head(dt.all_words)
# #reorder factors by frequency.
# dt.all_words[,word:=factor(dt.all_words$word,levels=as.character(dt.all_words$word))]
# qplot(word, data=dt.all_words[1:100,], weight=count, geom="histogram") + theme(axis.text.x = element_text(angle = 90, hjust = 1))




writeLines(blogSample, con="sample/en_US.blogs.sample.txt",useBytes = T)
writeLines(twitterSample, con="sample/en_US.twitter.sample.txt",useBytes = T)
writeLines(newsSample, con="sample/en_US.news.sample.txt",useBytes = T)



src <- DirSource(directory = "sample", mode = "text")

corpus <- Corpus(src)

RemoveNonAlphaNumeric <- function(x) gsub("[^a-zA-Z\\-\\' ]","",x)

corpus <- tm_map(corpus, content_transformer(FUN = RemoveNonAlphaNumeric))

# corpus <- tm_map(corpus, removeWords(corpus Negate('%in%') dictionary)) 


corpus <- tm_map(corpus, content_transformer(tolower))

corpus <- tm_map(corpus, stripWhitespace)

#corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, PlainTextDocument)   

dlmtrs <- " \\t\\r\\n.!?,;\"()" ;

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters =dlmtrs ))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters =dlmtrs))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4, delimiters =dlmtrs))
PentgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5, delimiters =dlmtrs))
SixGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 6, max = 6, delimiters =dlmtrs))
options(mc.cores=1)
# tdm <- TermDocumentMatrix(corpus, control = list(tokenize=AllgramTokenizer))
# tdm<-removeSparseTerms(x = tdm,sparse = .5)
# freqtdm <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
# rm(tdm)
# dt <- data.table(cbind(names(freqtdm), freqtdm))
# rm(freqtdm)
# setnames(dt,c("structure","count"))
# dt$count <- as.integer(dt$count)
# row.names(dt) <- NULL

#Build Unigram
tdmUni <- TermDocumentMatrix(corpus, control = list(tokenize=WordTokenizer))
tdmUni<-removeSparseTerms(x = tdmUni,sparse = .34)
freqtdmUni <- sort(rowSums(as.matrix(tdmUni)), decreasing=TRUE)
rm(tdmUni)
dtUni <- data.table(cbind(names(freqtdmUni), freqtdmUni))
rm(freqtdmUni)
setnames(dtUni,c("structure","count"))
dtUni$count <- as.integer(dtUni$count)
row.names(dtUni) <- NULL

#Build bigram
tdmBi <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
tdmBi<-removeSparseTerms(x = tdmBi,sparse = .34)
freqtdmBi <- sort(rowSums(as.matrix(tdmBi)), decreasing=TRUE)
rm(tdmBi)
dtBi <- data.table(cbind(names(freqtdmBi), freqtdmBi))
rm(freqtdmBi)
setnames(dtBi,c("structure","count"))
dtBi$count <- as.integer(dtBi$count)
row.names(dtBi) <- NULL


tdmTri <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))
tdmTri<-removeSparseTerms(x = tdmTri,sparse = .34)
freqtdmTri <- sort(rowSums(as.matrix(tdmTri)), decreasing=TRUE)
rm(tdmTri)
dtTri <- as.data.table(cbind(names(freqtdmTri), freqtdmTri))
rm(freqtdmTri)
setnames(dtTri,c("structure","count"))
dtTri$count <- as.integer(dtTri$count)
row.names(dtTri) <- NULL

tdmQuad <- TermDocumentMatrix(corpus, control = list(tokenize = QuadgramTokenizer))
tdmQuad<-removeSparseTerms(x = tdmQuad,sparse = .34)
freqtdmQuad <- sort(rowSums(as.matrix(tdmQuad)), decreasing=TRUE)
rm(tdmQuad)
dtQuad <- as.data.table(cbind(names(freqtdmQuad), freqtdmQuad))
rm(freqtdmQuad)
setnames(dtQuad,c("structure","count"))
dtQuad$count <- as.integer(dtQuad$count)
row.names(dtQuad) <- NULL


tdmPent <- TermDocumentMatrix(corpus, control = list(tokenize = PentgramTokenizer))
tdmPent<-removeSparseTerms(x = tdmPent,sparse = .34)
freqtdmPent <- sort(rowSums(as.matrix(tdmPent)), decreasing=TRUE)
rm(tdmPent)
dtPent <- as.data.table(cbind(names(freqtdmPent), freqtdmPent))
setnames(dtPent,c("structure","count"))
dtPent$count <- as.integer(dtPent$count)
row.names(dtPent) <- NULL


blogLineCount <- determine_nlines("final/en_US/en_US.blogs.txt")
blogSample <- sample_lines("final/en_US/en_US.blogs.txt", .001*blogLineCount)

vsrc <- VectorSource(blogSample)
testCorpus <- Corpus(vsrc)
close(vsrc)
tdmTest <- TermDocumentMatrix(testCorpus, control = list(tokenize = SixGramTokenizer))
freqtdmTest <- sort(rowSums(as.matrix(tdmTest)), decreasing=TRUE)
rm(tdmTest)
dtTest <- as.data.table(cbind(names(freqtdmTest), freqtdmTest))
setnames(dtTest,c("structure","count"))
dtTest$count <- as.integer(dtTest$count)
row.names(dtTest) <- NULL
inputs <- dtTest[,lapply(dtTest$structure,FUN = function(x) unlist(strsplit(x, split = " ")))]
inputs = t(inputs)
inputsDt[, test := do.call(paste, .SD), .SDcols = c("V1","V2","V3","V4","V5")]
inputsDt[,answer:=V6]
inputsDt[,c("V1","V2","V3","V4","V5","V6"):=NULL]
rm(inputs)



setkey(dtUni,count,struure,physical = T)
setkey(dtBi,count,structure,physical = T)
setkey(dtTri,count,structure,physical = T)
setkey(dtQuad,count,structure,physical = T)
setkey(dtPent,count,structure,physical = T)
setkey(dtTest,count,structure,physical = T)
setorder(dtUni,-count)
setorder(dtBi,strsplit(structure,"\\s+")[1],-count)
setorder(dtTri,-count)
setorder(dtQuad,-count)
setorder(dtPent,-count)
setorder(dtTest,-count)

#dtBi[,startword := grep('^\\b.+\\b',dtBi,value = TRUE)]

#Good Turning Estimation: Realocate the probablitiy mass of nGrams so that ones we have not seen are the same as ones we've seen once.
dtUni[,counter:=1]
dtBi[,counter:=1]
dtTri[,counter:=1]
dtQuad[,counter:=1]
dtPent[,counter:=1]

#Set nr as the 
dtUni[,nr:=sum(counter), by=count]
dtBi[,nr:=sum(counter), by=count]
dtTri[,nr:=sum(counter), by=count]
dtQuad[,nr:=sum(counter), by=count]
dtPent[,nr:=sum(counter), by=count]

dtUni[,n:=sum(counter*count), by=count]
dtBi[,n:=sum(counter*count), by=count]
dtTri[,n:=sum(counter*count), by=count]
dtQuad[,n:=sum(counter*count), by=count]
dtPent[,n:=sum(counter*count), by=count]

dtUni[,p0:=nr/n, by=count]
dtBi[,p0:=nr/n, by=count]
dtTri[,p0:=nr/n, by=count]
dtQuad[,p0:=nr/n, by=count]
dtPent[,p0:=nr/n, by=count]

#Calc Good Turing Count
dtUni[,gtCount:=((count + 1) * ((nr + 1))/n)]
dtBi[,gtCount:=((count + 1) * ((nr + 1))/n)]
dtTri[,gtCount:=((count + 1) * ((nr + 1))/n)]
dtQuad[,gtCount:=((count + 1) * ((nr + 1))/n)]
dtPent[,gtCount:=((count + 1) * ((nr + 1))/n)]




save(list = c("dtUni","dtBi","dtTri","dtQuad","dtPent"),file = "processedNGrams.RData")
load("processedNGrams.RData")