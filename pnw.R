dtmList <- list(dtUni, dtBi, dtTri, dtQuad, dtPent)
cleanInput <- function(input)
{
  #Clean the input in the same way the ngrams cleaned
  input <- gsub("[^a-zA-Z\\-\\' ]","",input)
  input <- tolower(input) 
  input <- stripWhitespace(input)
}

pnw <-function(input, resultSize = 3) {
  input <- cleanInput(input)
  inputV <- unlist(strsplit(input, split = " "))
  inputSize <- length(inputV)
  
  #Get Start Index to start testing with.
  if(inputSize >= 4)
  {
    startIdx <- inputSize - 3
  }
  else
  {
    startIdx <- 1
  }

  while(startIdx <= inputSize){
    #figure out which nGram to use based on how many words are being tested
    dtmIdx <- length(inputV[startIdx:inputSize]) + 1
    dtm <- dtmList[[dtmIdx]]
    
    #paste the string together
    testString <- paste(inputV[startIdx:inputSize],sep = " ", collapse = " ")

    #Good Turing Estimation
    tmp <- dtm[structure %like% paste0("^",testString, " .+"),]
    if(dim(tmp)[1]==0)
    {
      print(paste0("No results found in ", (inputSize - startIdx + 2), "-gram"))
      #alpha(wi-1) * ml(wi)
      dtm.minus1 <- dtmList[[dtmIdx-1]]
      
      1-sum()
    }
    else
    {
      print(paste0("Found ",dim(tmp)[1]," results found in ", (inputSize - startIdx + 2), "-gram"))
      setorder(tmp,-count)
      katzCount <- sum(tmp$count*tmp$nr)
      
      
      tmp[,gte:=gtCount/total]
      setkey(tmp,gte)
      setorder(tmp,-gte)
      print(tmp)
    }
  
    
    startIdx = startIdx + 1
  }
  
  tester <- function(x)
  {
    
    test <- inputsDt[sample(.N,1)]
    pnw(test$test)
    print(test$test)
    print(test$answer)
  }

}