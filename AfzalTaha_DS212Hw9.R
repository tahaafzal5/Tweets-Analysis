library(tm)

wordsCorpus <- function(vec, fileName)
{ 
  words.vec <- VectorSource(vec)
  words.corpus <- Corpus(words.vec)
  words.corpus <- tm_map(words.corpus, content_transformer(tolower))
  words.corpus <- tm_map(words.corpus, removePunctuation)
  words.corpus <- tm_map(words.corpus, removeNumbers)
  words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
  
  tdm <- TermDocumentMatrix(words.corpus, control = (list(stopwords = T)))
  
  m <- as.matrix(tdm)
  wordCounts <- rowSums(m)
  wordCounts <- sort(wordCounts, decreasing = TRUE)
  
  #total number of words (after removing stopwords)
  totalWords <- sum(wordCounts)
  
  #total number of unique words
  uniques <- length(wordCounts)
  
  pos <- "positive-words.txt"
  neg <- "negative-words.txt"
  p <- scan(pos, character(0), sep = "\n")
  n <- scan(neg, character(0), sep = "\n")
  
  #vector with unique words
  words <- names(wordCounts)
  
  # match() function used to show matches between the two lists
  matched <- match(words, p, nomatch = 0)
  
  # Count the words that matched
  mCounts <- wordCounts[which(matched != 0)]
  length(mCounts)
  mWords <- names(mCounts)
  mWords
  
  # number of occurrence of positive words.
  nPos<- sum(mCounts)
  
  #positive word vocabulary count
  nUniquePos<-length(mCounts)
  
  # Use the match() function to show matches between the two lists
  matched <- match(words, n, nomatch=0)
  
  # Count the words that matched
  nCounts <- wordCounts[which(matched !=0)]
  length(nCounts)
  nWords <- names(nCounts)
  nWords
  #the number of occurrence of positive words.
  nNeg<- sum(nCounts)
  
  #positive word vocabulary count
  nUniqueNeg<-length(nCounts)
  
  #percentage of positive words.
  ratioPos <- nPos / totalWords
  ratioPos<-ratioPos * 100
  
  #percentage of negative words.
  ratioNeg <- nNeg / totalWords
  ratioNeg<-ratioNeg * 100
  
  results<- c(totalWords, uniques, nPos, nUniquePos, nNeg, nUniqueNeg, ratioPos, ratioNeg)
  output(results, fileName)
}

#prints the results
output <- function(vect, fileName)
{
  print(paste("File Name: ", fileName, sep=""))
  print(paste("Total number of words in text(aftercleanup): ", vect[1], sep = ""))
  print(paste("Number of words used(vocabulary): ", vect[2], sep = ""))
  print(paste("Number of occurences of positive words: ", vect[3], sep = ""))
  print(paste("Positive word vocabulary count: ", vect[4], sep = ""))
  print(paste("Number of occurences of negative words: ", vect[5], sep = ""))
  print(paste("Negative word vocabulary count: ", vect[6], sep = ""))
  print(paste("Percentage of positive words: ", vect[7], sep = ""))
  print(paste("Percentage of negative words: ", vect[8],"%", sep = ""))
}

#ge files and columns with tweet texts
trumpFile<- read.csv("Donald-Tweets.csv", stringsAsFactors = FALSE)
trumpTxt<- trumpFile$Tweet_Text

elonFile<- read.csv("elonmusk_tweets.csv", stringsAsFactors = FALSE)
elonTxt<- elonFile$text

womenmarchFile<- read.csv("womenmarch.csv", stringsAsFactors = FALSE)
womenmarchtxt<- womenmarchFile$text

#calculate and display everything.
wordsCorpus(trumpTxt, "Donald-Tweets.csv")
wordsCorpus(elonTxt, "elonmusk_tweets.csv")
wordsCorpus(womenmarchtxt, "womenmarch.csv")