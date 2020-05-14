##########################################################################################
#Installing and loading required packages
install.packages("twitteR")
install.packages("ROAuth")
install.packages("RJSONIO")
install.packages("tm")
install.packages("wordcloud")

library(twitteR)
library(ROAuth)
library(RJSONIO)
library(tm)
library(wordcloud)


#Setting the Key, Secret, and Token - Please create your own
api_key <- "<your code>"
api_secret <- "<your code>"
token <- "<your code>"
token_secret <- "<your code>"

# Create Twitter Connection
setup_twitter_oauth(api_key,api_secret,token,token_secret)
#Yes #for direct authentication

#Entering list of words to be searched in twitter
fd_bp = c("theblackpanther","BlackPanther") 
fd_tr =c("tombraider","TombRaider")

#Run Twitter Search. Format is searchTwitter("Search Terms", n=100, lang="en", geocode="lat,lng", also accepts since and until).
tweets_bp <- searchTwitter(fd_bp, n=5000, lang="en", since="2018-01-01")
tweets_tr <- searchTwitter(fd_tr, n=5000, lang="en", since="2015-01-01")

#Transforming tweets list into a data frame
tweets_bp.df <- twListToDF(tweets_bp)
tweets_tr.df <- twListToDF(tweets_tr)

write.csv(tweets_bp.df,"tweets_bp.df.csv")
View(tweets_bp.df)
#tweets_bp.text <- tweets_bp.df$text

#some_text <- tweets.df[,1]


###########################################################################################
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
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
##############################
#https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon

hu.liu.pos = scan("<local directory path for positive words>", what='character', comment.char=';')
hu.liu.neg = scan("<local directory path for negative-words>", what='character', comment.char=';')

# add a few twitter and industry favorites
pos.words = c(hu.liu.pos, 'fantastic')
neg.words = c(hu.liu.neg, 'wtf','shit','bloody')
###############################
blackpantherscore <- score.sentiment(tweets_bp.df$text,pos.words,neg.words,.progress='text')
tombraiderscore <- score.sentiment(tweets_tr.df$text,pos.words,neg.words,.progress='text')
#####################
hist(blackpantherscore$score)
qplot(blackpantherscore$score)
qplot(tombraiderscore$score)