# Install and Load twitteR package
install.packages('twitteR')
library(twitteR)

# API Keys and tokens - Insert within double quotes your own keys and tokens
api_key <- ""
api_secret <- ""
access_token <- ""
access_token_secret <- ""

# Set up Twitter authorization with your keys and access tokens
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# Set working directory to where authenticare.R file is located
setwd("C:/Users/Daniel_Warner/Documents/6353")

# Collect tweets on a keyword based on its hashtag. 
capitol_tweets = searchTwitter("uscapitol", n=2000, lang="en")

# Retrieves texts only from tweets as vectors
capitolTweets = sapply(capitol_tweets, function(x) x$getText())

# Create function that converts all cases to lower case
catch.error = function(x)
{
  # let us create a missing value for test purpose
  y = NA
  # try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}

# Creates function to clean tweets
cleanTweets <- function(tweet){
  # Clean the tweet for sentiment analysis
  #  remove html links, which are not required for sentiment analysis
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  # First we will remove retweet entities from the stored tweets (text)
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Then remove all "#Hashtag"
  tweet = gsub("#\\w+", " ", tweet)
  # Then remove all "@people"
  tweet = gsub("@\\w+", " ", tweet)
  # Then remove all the punctuation
  tweet = gsub("[[:punct:]]", " ", tweet)
  # Then remove numbers, we need only text for analytics
  tweet = gsub("[[:digit:]]", " ", tweet)
  # Then we remove unnecessary spaces (white spaces, tabs etc)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  # Finally we replace UTF-8 characters with ASCII ones
  tweet=iconv(tweet, "UTF-8", "ascii",sub='')
  # if anything else, you feel, should be removed, you can. For example "slang words" etc using the above function and methods.
  # Next we'll convert all the word in lowar case. This makes uniform pattern.
  tweet = catch.error(tweet)
  tweet
}

# Creates function that cleans tweets and removes NA or duplicate tweets
cleanTweetsAndRemoveNAs <- function (Tweets) {
  TweetsCleaned = sapply(Tweets, cleanTweets)
  # Remove the "NA" tweets from this tweet list
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  names(TweetsCleaned) = NULL
  # Remove repetitive tweets from this tweet list
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned
}

# Call cleanTweetsAndRemoveNAs function to clean the tweets
capitolTweetsCleaned = cleanTweetsAndRemoveNAs(capitolTweets)

# Get tweet counts of cleaned data sets
length(capitolTweetsCleaned)

################################################################################

library("plyr")
library("stringr")

opinion.lexicon.pos = scan('positivewords.txt', what='character', comment.char=';')
opinion.lexicon.neg = scan('negativewords.txt', what='character', comment.char=';')

head(opinion.lexicon.neg) # look at first few negative words
head(opinion.lexicon.pos) # look at first few positive words

# Create getSentimentScore function that will calculate the sentiment score for each tweet              
getSentimentScore = function(sentences, words.positive, words.negative, .progress='none')
{
  require(plyr) # Require plyr package
  require(stringr)  # Require stringr package
  
  scores = laply(sentences, function(sentence, words.positive, words.negative) {
    
    # Let first remove the Digit, Punctuation character and Control characters:
    sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '', gsub('\\d+', '', sentence)))
    
    # Then lets convert all to lower sentence case:
    sentence = tolower(sentence)
    
    # Now lets split each sentence by the space delimiter
    words = unlist(str_split(sentence, '\\s+'))
    
    # Get the boolean match of each words with the positive & negative opinion-lexicon
    pos.matches = !is.na(match(words, words.positive))
    neg.matches = !is.na(match(words, words.negative))
    
    # Now get the score as total positive sentiment minus the total negatives
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, words.positive, words.negative, .progress=.progress )
  
  # Return a data frame with respective sentence and the score
  return(data.frame(text=sentences, score=scores))
}

# Calculate a raw sentiment score for each tweet about a keyword using the getSentimentScore function
capitolResult = getSentimentScore(capitolTweetsCleaned, opinion.lexicon.pos, opinion.lexicon.neg)

View(capitolResult) # View results

# Create a histogram showing the distribution of the raw sentiment scores for each keyword
hist(capitolResult$score)

library('psych')

# Calculate descriptive stats for raw sentiment scores for each keyword
describe(capitolResult$score)

#################################################################################
library(ggplot2)
install.packages('syuzhet') # Install syuzhet package for sentiment analysis
library(syuzhet)

###########################################
# US CAPITOL SENTIMENT
###########################################
# Use get_nrc_sentiment function to obtain the sentiment of cleaned tweets
capitolEmotions=get_nrc_sentiment(as.character(capitolTweetsCleaned)) # Get raw sentiment scores for each tweet
View(capitolEmotions)
capitolEmotionsDF=t(data.frame(capitolEmotions)) # Obtain transpose of data frame
View(capitolEmotionsDF)

# Calculate number of tweets with each emotion >0
capitolEmotionsDFCount=data.frame(rownames(capitolEmotionsDF), rowSums(capitolEmotionsDF > 0))
View(capitolEmotionsDFCount)
rownames(capitolEmotionsDFCount)=NULL # Set row names to NULL
colnames(capitolEmotionsDFCount)=c('Emotion','Frequency') # Set column names to 'Emotion' and 'Frequency'
View(capitolEmotionsDFCount)

# Barplot of capitol tweet sentiment
barplot(capitolEmotionsDFCount$Frequency,  names.arg = capitolEmotionsDFCount$Emotion, main="US Capitol Tweet Sentiment", xlab="Emotions", ylab="Frequency")

# Obtain a single sentiment score for each tweet 
# Positive values indicate positive sentiment and negative values indicate negative sentiment
capitolPolarity=get_sentiment(as.character(capitolTweetsCleaned)) 
View(capitolPolarity)
