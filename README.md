# RLanguageTwitterTextMining -> Files Coming Soon

 MAIN
 
 //Twitter authentification
 twitterAuth()
 //Keyword for study
 keyword <- "happy"
 //Number of tweets for study
 numberoftweets <- 5000
 //Extracting Tweets
 rawTweets <- extractTweets(keyword, numberoftweets)
 //Cleaning Tweets (1)
 tweetsText <- cleanTweets(rawTweets)
 //Searching for top words
 topWords <- topWordsFrequency(tweetsText)
 topWords[1:15,] # (2)
                                                                                                                        
     Word                freq
birthday                 2357
hope                      870
aldenrichardsat           618
night                     603
攼㹤愼㸰戼㹣攼㹤戼㹥㠼㸸      595
enjoyed                   565
bae                       563
pambansang                563
day                       497
jennie                    448
love                      347
happyjennieday            206
one                       205
blackpink                 198
king                      171


 //Searching for top co-occurences
 topfivecooccurrences <- topcooccurences(tweetsText)
 topfivecooccurrences # (2) 

    word1   word2 freq
1     and    love   98
2   happy pretend   98
3    have    hope   98
4  thanks     the   97
5 support     the   95

IMG


 //Adding polarity to top words and plotting to bar graph
 topWordsPol <- wordPol(topWords[1:15,])
 topWordsPolPlot(tweetsText,topWordsPol) # (3)
 
IMG


 //Adding polarity to top cooccurrences and plotting to bar graph
 topCoocPol <- coocPol(topfivecooccurrences)
 topCoocPolPlot(tweetsText,topCoocPol) # (3)

IMG


 //Defining Study's Overall Polarity
 polarityVerdict()

Top 3 words overall polarity:  neutral 
[1] "birthday"
[1] "hope"
[1] "aldenrichardsat"

Top 3 co-occurrences overall polarity:  positive 
[1] "and & love"
[1] "happy & pretend"
[1] "have & hope"

Overall study polarity: Cannot be defined

---------------------------------------------

 EXTRAS

 //Processing
 scoreV1 <- scoringV1(tweetsText)
 scoreV2 <- scoringV2(tweetsText)
 scoreV3 <- scoringV3(tweetsText)
 polarityV1 <- polarity(scoreV1)
 polarityV2 <- polarity(scoreV2)
 polarityV3 <- polarity(scoreV3)
 triplePlot(tweetsText, polarityV1, polarityV2, polarityV3)
 
IMG


 //Sentiment analysis over study
 sentimentScoring(tweetsText)

IMG


 //Plotting top words word cloud
 topWordsWordcloudPlot(topWords,50)

IMG


 //Plotting top words in bar graph
 topWordsBasicPlotting(tweetsText,topWords,15)
 
IMG


 //Plotting top cooccurences in bar graph
 cooccurencesBasicPlotting(topfivecooccurrences)
 
IMG
