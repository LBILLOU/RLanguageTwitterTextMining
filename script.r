# Authentification to twitter API
twitterAuth <- function() {
  library(twitteR)
  # Twitter API Settings
  consumer_key <- "7WIMjXXXXXXXXXXXygOixPkQC"
  consumer_secret <- "q1rzeDtglHWbXXXXXXXXXXXXXXXXXXXB2QiYynlNUW"
  access_token <- "1115489134XXXXXXXXXXXXXXXXXXXXXXXXXXXXIiWheBKz5JaAo"
  access_secret <- "1FTr5XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXrt235hSVzAY"
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
}

# Function to extract tweets (study word, number of tweets)
extractTweets <- function(query, numberOfTweets) {
  # Extracting tweets
  tweetText <- searchTwitter(query, n = numberOfTweets, lang = "en")
  return(tweetText)
}

# Function to clean text used in cleanTweets function
clean.text <- function(txt){
  txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", txt) # no retweet "RT"
  txt = gsub("@\\w+", "", txt)  # no @username
  txt = gsub("[[:punct:]]", "", txt) # no punct
  txt = gsub("[[:digit:]]", "", txt) # no digit
  txt = gsub("http\\w+", "", txt) # no website
  txt = gsub("amp", "", txt) # getting rid of amp
  txt = gsub("[ \t]{2,}", " ", txt) # double space to single space
  txt = gsub("^\\s+|\\s+$", "", txt) # no space beginning/end
  stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
  stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
  txt = stringr::str_replace_all(txt, stopwords_regex, '')
  
  try.tolower = function(x){
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  txt = sapply(txt, try.tolower)
  txt = txt[txt != ""]
  names(txt) = NULL
  return(txt)
}

# Function to clean tweets from raw extracted tweets
cleanTweets <- function(rawTweets){
  # Placing tweets data into data frame
  tempDF <- twListToDF(rawTweets)

  # Extracting tweets' text from data frame
  rawTweets <- unlist(tempDF$text)
  rm(tempDF) # Deleting temporary data frame

  # Cleaning text of tweets
  rawTweets <- clean.text(rawTweets)
  
  return(rawTweets)
}

# Function to get top frequency words
topWordsFrequency <- function(cleanTweetsText){
  # Remove stop word and count word frequency
  library(tm)
  freqCount <- Corpus(VectorSource(cleanTweetsText))
  freqCount <- tm_map(freqCount, removeWords, c(stopwords('english'), keyword)) # removes stopwords and keyword from top words
  freqCount <- TermDocumentMatrix(freqCount)
  freqCount <- as.matrix(freqCount)
  freqCount <- sort(rowSums(freqCount),decreasing=TRUE)
  freqCount <- data.frame(word = names(freqCount),freq=freqCount)
  return(freqCount)
}

# Function to get top 5 word co-occurences, returns dataframe
topcooccurences <- function(cleanTweetsText){
  # Create a corpus
  tweetcorpus = Corpus(VectorSource(cleanTweetsText))
  # Removes stopwords and keyword
  tweetcorpus <- tm_map(tweetcorpus, removeWords, c(stopwords('english'), keyword))
  # Create document term-document matrix 
  tweetTDM = TermDocumentMatrix(tweetcorpus)
  tweetTDM
  # Define term-document matrix as matrix
  ttmatrix = as.matrix(tweetTDM)
  # Change it to a Boolean matrix
  ttmatrix[ttmatrix>=1] <- 1
  # Transform into a term-term adjacency matrix
  ttmatrix <- ttmatrix %*% t(ttmatrix)
  ## Creating sorted list of top co-occurrences
  coOccurrence = list(c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0))
  # Going through triangular matrix to prevent A-B & B-A selection and for speed
  for (row in 1:dim(ttmatrix)[1]){
    for(col in row:dim(ttmatrix)[2]){
      # Getting rid of A-A or B-B pairs
      if (row != col){
        for (i in 1:length(coOccurrence)){
          if(ttmatrix[row,col]>coOccurrence[[i]][3]){
            coOccurrence[[i]][1] <- rownames(ttmatrix)[row]
            coOccurrence[[i]][2] <- colnames(ttmatrix)[col]
            coOccurrence[[i]][3] <- ttmatrix[row,col]
            break
          }
        }
      }
    }
  }
  t5c <- coOccurrence
  word1 = c(t5c[[1]][1],t5c[[2]][1],t5c[[3]][1],t5c[[4]][1],t5c[[5]][1])
  word2 = c(t5c[[1]][2],t5c[[2]][2],t5c[[3]][2],t5c[[4]][2],t5c[[5]][2])
  freq = c(t5c[[1]][3],t5c[[2]][3],t5c[[3]][3],t5c[[4]][3],t5c[[5]][3])
  coocDF = data.frame(word1, word2, freq)
  return(coocDF)
}

# Function to assign scores to words
polarityScore = function(sentences, pos.words, neg.words, .progress='none'){
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# positive/negative words -> https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
hu.liu.pos = scan("C:/Users/Loïc/Desktop/Langage R/project/RtwitteR/ressources/positive-words.txt", what='character', comment.char=';')
hu.liu.neg = scan("C:/Users/Loïc/Desktop/Langage R/project/RtwitteR/ressources/negative-words.txt", what='character', comment.char=';')

# Function to assign polarity and sentiment to top words
wordPol <- function(topWordsDF){
  # Adding column to dataframe
  topWordsDF$polarity <- 'none'
  # Scoring each word
  for (i in 1:nrow(topWordsDF)){
    topWordsDF[i,3] <- polarityScore(topWordsDF[i,1], hu.liu.pos, hu.liu.neg)$score
  }
  return(topWordsDF)
}

# Function to plot top words with color depending on polarity
topWordsPolPlot <- function(cleanTweetsText, topwordsdataf){
  op <- par(mar=c(6,4,4,2)) # Allows the names.arg below the barplot to be visible
  colz <- ifelse(topwordsdataf$polarity<=-1, "indianred1", 
                 ifelse(topwordsdataf$polarity>=1, "olivedrab1", "honeydew3"))
  barplot(topwordsdataf$freq[1:nrow(topwordsdataf)], las = 2, names.arg = topwordsdataf$word[1:nrow(topwordsdataf)],
          col = colz, 
          main =sprintf("TOP 15 WORDS\nStudy : %s | Number of tweets : %i", keyword, length(cleanTweetsText)),
          ylab = "Word frequency")
  legend("topright", inset=.05, title="Word Polarity",
         legend=c("Positive", "Neutral", "Negative"), 
         fill=c("olivedrab1", "honeydew3","indianred1"))
  rm(op)
}

# Function to assign polarity and sentiment to top cooccurrences
coocPol <- function(top5cooc){
  # Adding columns to dataframe
  top5cooc$polword1 <- 'none'
  top5cooc$polword2 <- 'none'
  top5cooc$polarity <- 'none'
  # Scoring each word
  for (i in 1:nrow(top5cooc)){
    top5cooc[i,4] <- polarityScore(top5cooc[i,1], hu.liu.pos, hu.liu.neg)$score
    top5cooc[i,5] <- polarityScore(top5cooc[i,2], hu.liu.pos, hu.liu.neg)$score
    top5cooc[i,6] <- as.numeric(top5cooc[i,4]) + as.numeric(top5cooc[i,5])
  }
  return(top5cooc)
}

# Function to plot top cooccurrences with color depending on polarity
topCoocPolPlot <- function(cleanTweetsText, topcoocdataf){
  a <- as.numeric(as.character(topcoocdataf[[3]][1]))
  b <- as.numeric(as.character(topcoocdataf[[3]][2]))
  c <- as.numeric(as.character(topcoocdataf[[3]][3]))
  d <- as.numeric(as.character(topcoocdataf[[3]][4]))
  e <- as.numeric(as.character(topcoocdataf[[3]][5]))
  numb <- c(a,b,c,d,e)
  lab <- c(paste(topcoocdataf[1,1], topcoocdataf[1,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[2,1], topcoocdataf[2,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[3,1], topcoocdataf[3,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[4,1], topcoocdataf[4,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[5,1], topcoocdataf[5,2], sep = " & "))
  op <- par(mar=c(8,4,4,2)) # Allows the names.arg below the barplot to be visible
  colz <- ifelse(topcoocdataf$polarity<=-1, "indianred1", 
                 ifelse(topcoocdataf$polarity>=1, "olivedrab1", "honeydew3"))
  barplot(numb, las = 2, names.arg = lab,
          col = colz, 
          main =sprintf("TOP CO-OCCURENCES\nStudy : %s | Number of tweets : %i", keyword, length(tweetsText)),
          ylab = "Co-occurence frequency")
  legend("topright", inset=.05, title="Polarity",
         legend=c("Positive", "Neutral", "Negative"), 
         fill=c("olivedrab1", "honeydew3","indianred1"))
  rm(op)
}

# Function : analysis comparison verdict
polarityVerdict <- function(){
  TWPSum <- as.numeric(topWordsPol[1,3])+as.numeric(topWordsPol[2,3])+as.numeric(topWordsPol[3,3])
  TCPSum <- as.numeric(topCoocPol[1,6])+as.numeric(topCoocPol[2,6])+as.numeric(topCoocPol[3,6])
  # Function to get overall polarity
  generalPolarity <- function(PSum){
    if(PSum == 0){
      verdict <- 'neutral'
    }else if (PSum < 0){
      verdict <- 'negative'
    }else if (PSum > 0){
      verdict <- 'positive'
    }else{
      verdict <- 'error'
    }
    return(verdict)
  }
  # Getting overall polarity for both top 3
  TWPVerdict <- generalPolarity(TWPSum)
  TCPVerdict <- generalPolarity(TCPSum)
  # Printing results
  cat("\nTop 3 words overall polarity: ", TWPVerdict, "\n")
  print(as.character(topWordsPol[1,1]))
  print(as.character(topWordsPol[2,1]))
  print(as.character(topWordsPol[3,1]))
  cat("\nTop 3 co-occurrences overall polarity: ", TCPVerdict,"\n")
  print(paste(topCoocPol[1,1], topCoocPol[1,2], sep = " & "))
  print(paste(topCoocPol[2,1], topCoocPol[2,2], sep = " & "))
  print(paste(topCoocPol[3,1], topCoocPol[3,2], sep = " & "))
  
  if (TWPVerdict == TCPVerdict){
    cat("\nOverall study polarity: ", TWPVerdict)
  }else {
    cat("\nOverall study polarity: Cannot be defined")
    cat("\nTop 3 Words -> ", TWPVerdict)
    cat("\nTop 3 Co-Occurences -> ", TCPVerdict)
  }
}


### Main ###

#Twitter authentification
twitterAuth()
# Keyword for study
keyword <- "Super Bowl"
# Number of tweets for study
numberoftweets <- 5000
# Extracting Tweets
rawTweets <- extractTweets(keyword, numberoftweets)
# Cleaning Tweets (1)
tweetsText <- cleanTweets(rawTweets)
# Searching for top words
topWords <- topWordsFrequency(tweetsText)
topWords[1:15,] # (2)
# Searching for top co-occurences
topfivecooccurrences <- topcooccurences(tweetsText)
topfivecooccurrences # (2) 
# Adding polarity to top words and plotting to bar graph
topWordsPol <- wordPol(topWords[1:15,])
topWordsPolPlot(tweetsText,topWordsPol) # (3)
# Adding polarity to top cooccurrences and plotting to bar graph
topCoocPol <- coocPol(topfivecooccurrences)
topCoocPolPlot(tweetsText,topCoocPol) # (3)
# Defining Study's Overall Polarity (3)
polarityVerdict()


### EXTRA ###

# Scoring function version 1 -> english opinion lexicon
scoringV1 <- function(cleanTweetsText){
  # https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
  hu.liu.pos = scan("C:/Users/Loïc/Desktop/Langage R/project/RtwitteR/ressources/positive-words.txt", what='character', comment.char=';')
  hu.liu.neg = scan("C:/Users/Loïc/Desktop/Langage R/project/RtwitteR/ressources/negative-words.txt", what='character', comment.char=';')
  # Function to assign scores cleaned tweets
  polarityScore = function(sentences, pos.words, neg.words, .progress='none'){
    require(plyr)
    require(stringr)
    
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array ("a") of scores back, so we use 
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      
      
      # split into words. str_split is in the stringr package
      word.list = str_split(sentence, '\\s+')
      # sometimes a list() is one level of hierarchy too much
      words = unlist(word.list)
      
      # compare our words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      
      # match() returns the position of the matched term or NA
      # we just want a TRUE/FALSE:
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      
      # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
      score = sum(pos.matches) - sum(neg.matches)
      
      return(score)
    }, pos.words, neg.words, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
  }
  # Scoring tweets V1
  library(plyr)
  polarityScoreV1 <- polarityScore(cleanTweetsText, hu.liu.pos, hu.liu.neg)$score
  return(polarityScoreV1)
}
# Scoring function version 2 -> sentimentr package
scoringV2 <- function(cleanTweetsText){
  # Scoring tweets V2
  library(sentimentr)
  polarityScoreV2 <- sentiment(cleanTweetsText)[,4]$sentiment
  return(polarityScoreV2)
}
# Scoring function version 2 -> syuzhet package
scoringV3 <- function(cleanTweetsText){
  #Scoring tweets V3
  library(syuzhet)
  polarityScoreV3 <- get_nrc_sentiment(cleanTweetsText)[,9:10]
  polarityScoreV3 <- cbind(polarityScoreV3, Total = polarityScoreV3[2] - polarityScoreV3[1])
  polarityScoreV3 <- polarityScoreV3[,3]
  return(polarityScoreV3)
}
#Function to define polarity depending on score
polarity = function(scoreTable){
  polarityTable <- data.frame()
  for (score in scoreTable){
    if (score > 0){
      temp <- data.frame("polarity" = "Positive")
      polarityTable <- rbind(polarityTable, temp)
    } else if (score < 0) {
      temp <- data.frame("polarity" = "Negative")
      polarityTable <- rbind(polarityTable, temp)
    } else {
      temp <- data.frame("polarity" = "Neutral")
      polarityTable <- rbind(polarityTable, temp)
    }
  }
  return(polarityTable$polarity)
}
# Function to plot grouped bars of three sentiment analyses 
triplePlot <- function(cleanTweetsText, polarityV1, polarityV2, polarityV3){
  # Creating polarity dataframe
  df <- data.frame(text=cleanTweetsText, 
                   "english.opinion.lexicon"=polarityV1,
                   "sentimentr.package"=polarityV2,
                   "syuzhet.package"=polarityV3,
                   stringsAsFactors=FALSE)
  
  # Melting data frame for grouped bar plotting
  library(reshape)
  melteddf <- melt(df, id="text")
  
  # Plotting 3 scoring methods on one graph
  library(ggplot2)
  ggplot(melteddf, aes(value, fill=variable)) + 
    geom_bar(position="dodge", color="black") +
    labs(x="Polarity Categories", y="Number of Tweets", fill="Scoring Method", 
         title=sprintf("Tweets Polarity\nKeyword: %s | Number of tweets: %i\nDate: %s", keyword, length(cleanTweetsText), Sys.time())) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_brewer(palette="Set1")
}
# Function to plot using plot_ly
triplePlotly <- function(cleanTweetsText, polarityV1, polarityV2, polarityV3){
  library(plotly)
  df <- data.frame(text=cleanTweetsText, 
                   "english.opinion.lexicon"=polarityV1,
                   "sentimentr.package"=polarityV2,
                   "syuzhet.package"=polarityV3,
                   stringsAsFactors=FALSE)
  countV1 <- table(df$english.opinion.lexicon)
  countV2 <- table(df$sentimentr.package)
  countV3 <- table(df$syuzhet.package) # Ordered by appearance!
  
  countdf <- data.frame(polarity=c("Negative","Neutral","Positive"), 
                        scorecountV1=c(countV1[["Negative"]],countV1[["Neutral"]],countV1[["Positive"]]),
                        scorecountV2=c(countV2[["Negative"]],countV2[["Neutral"]],countV2[["Positive"]]),
                        scorecountV3=c(countV3[["Negative"]],countV3[["Neutral"]],countV3[["Positive"]]),
                        stringsAsFactors=FALSE)
  countdf
  
  plot_ly(countdf, x = ~polarity, y = ~scorecountV1, type = 'bar', 
          name = 'english opinion lexicon', marker = list(color = 'rgba(100,100,100)')) %>%
    add_trace(y = ~scorecountV2, name = 'sentimentr package') %>%
    add_trace(y = ~scorecountV3, name = 'syuzhet package') %>%
    layout(xaxis = list(title = 'Polarity Categories'),
           title = sprintf('Tweets Polarity<br>Keyword: %s | Number of tweets: %i<br>Date: %s', keyword, length(cleanTweetsText), Sys.time()),
           titlefont = list(size = 10, color = "black"),
           legend = list(bgcolor = "#E2E2E2", x = 100, y = 0.5),
           yaxis = list(title = 'Number of Tweets'),
           barmode = 'group')
}
# Processing
scoreV1 <- scoringV1(tweetsText)
scoreV2 <- scoringV2(tweetsText)
scoreV3 <- scoringV3(tweetsText)
polarityV1 <- polarity(scoreV1)
polarityV2 <- polarity(scoreV2)
polarityV3 <- polarity(scoreV3)
triplePlot(tweetsText, polarityV1, polarityV2, polarityV3)
# triplePlotly(tweetsText, polarityV1, polarityV2, polarityV3)

# Function to extract and plot sentiment scoring
sentimentScoring <- function(cleanTweetsText){
  library(syuzhet)
  sentiment <- get_nrc_sentiment(cleanTweetsText)[,1:8]
  sentimentTotals <- data.frame(colSums(sentiment[,c(1:8)]))
  names(sentimentTotals) <- "count"
  sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
  rownames(sentimentTotals) <- NULL
  ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
    geom_bar(aes(fill = sentiment), stat = "identity") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    xlab("Sentiment") + ylab("Frequency") + 
    ggtitle(sprintf("SENTIMENTS SCORES\nStudy : %s | Number of tweets : %i", keyword, length(cleanTweetsText)))
}
sentimentScoring(tweetsText)

# Function to plot wordcloud of top words
topWordsWordcloudPlot <- function(freqCount, wordcloudNumber){
  library(wordcloud)
  library(RColorBrewer)
  wordcloud(freqCount$word[1:wordcloudNumber], freqCount$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
}
topWordsWordcloudPlot(topWords,50)


# BASIC PLOTS FOR TOP WORDS AND TOP CO-OCCURENCES

# Function to plot top words used in pool of tweets
topWordsBasicPlotting <- function(cleanTweetsText, freqCount, topNumber){
  op <- par(mar=c(6,4,4,2)) # Allows the names.arg below the barplot to be visible
  barplot(freqCount$freq[1:topNumber], las = 2, names.arg = freqCount$word[1:topNumber],
          col ="lightblue", 
          main =sprintf("TOP 15 WORDS\nStudy : %s | Number of tweets : %i", keyword, length(cleanTweetsText)),
          ylab = "Word frequency")
  rm(op)
}
# Plotting top words in bar graph
topWordsBasicPlotting(tweetsText,topWords,15)
# Function to plot top co-occurences
cooccurencesBasicPlotting <- function(topcoocdataf){
  a <- as.numeric(as.character(topcoocdataf[[3]][1]))
  b <- as.numeric(as.character(topcoocdataf[[3]][2]))
  c <- as.numeric(as.character(topcoocdataf[[3]][3]))
  d <- as.numeric(as.character(topcoocdataf[[3]][4]))
  e <- as.numeric(as.character(topcoocdataf[[3]][5]))
  numb <- c(a,b,c,d,e)
  lab <- c(paste(topcoocdataf[1,1], topcoocdataf[1,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[2,1], topcoocdataf[2,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[3,1], topcoocdataf[3,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[4,1], topcoocdataf[4,2], sep = " & "))
  lab <- c(lab, paste(topcoocdataf[5,1], topcoocdataf[5,2], sep = " & "))
  op <- par(mar=c(8,4,4,2)) # Allows the names.arg below the barplot to be visible
  barplot(numb, las = 2, names.arg = lab,
          col = "lightblue", 
          main =sprintf("TOP CO-OCCURENCES\nStudy : %s | Number of tweets : %i", keyword, length(tweetsText)),
          ylab = "Co-occurence frequency")
  rm(op)
}
# Plotting top cooccurences in bar graph
cooccurencesBasicPlotting(topfivecooccurrences)
