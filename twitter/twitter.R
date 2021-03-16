library(rtweet)
library(tm)
library(stringr)

appname="food_pop"
key="ST2YCylWRuc0U2nWyJaIicvA6"
secret="LQmGWZZqfYrXutrQpOb9N9z665Otqw2fUYJNWhwQyYXCsNwc63"
access_token="445022752-ge4vhU99FeP9pGbnwFbZuY9lHOZHPKyesQuGal3Y"
access_secret="2jDMxRwKxmZSSHkETms8FHzUMsFryRTAuRHFiAL9DWW0E"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)
  #rstats_tweets <- search_tweets(q = "#food -filter:images", n = 500, lang = "en",include_rts = FALSE,type = "popular")
  #ordinato=rstats_tweets[order(-rstats_tweets$retweet_count),]
  limit=rate_limit(twitter_token)
  mylim=limit[limit$limit!=limit$remaining,]
  #ordinato_image=rstats_tweets_image[order(-rstats_tweets_image$retweet_count),]
  gordon=get_timeline("GordonRamsay",n=1500,include_rts=FALSE,token=twitter_token)
  #test=lookup_users("GordonRamsay",token = twitter_token) 
  write.csv(gordon$media_url[!is.na(gordon$media_url)],file="url.csv")
  epicurious=get_timeline("epicurious",n=1500,include_rts=FALSE,token=twitter_token)
  bonappetit=get_timeline("bonappetit",n=1500,include_rts=FALSE,token=twitter_token.q="-has:image")
  #epicurious_image=data.frame("url"=rep("",sum(is.na(epicurious$media_url))),"id"=)
  
  smittenkitchen=get_timeline("smittenkitchen",n=1500,include_rts=FALSE,token=twitter_token)
  write.csv(smittenkitchen$media_url[!is.na(smittenkitchen$media_url)],file="url.csv")
 
 # smittenkitchen=smittenkitchen[is.na(smittenkitchen$mentions_screen_name),]#eliminiamo le menzioni
 # gordon=gordon[is.na(gordon$mentions_screen_name),]#eliminiamo le menzioni
  
  text=gordon$text
  #text=corpus(text)
  text=gsub("http[^>]+$","",text) #remove last link
  text=gsub("http[^>]+ ","",text) #remove other link
  #text=gsub("#[^>]+$","",text) #remove last #
  #text=gsub("#[^>]+ ","",text) #remove other #
  text=gsub("@[^>]+$","",text) #remove last @
  text=gsub("@[^>]+ ","",text) #remove other @
  
  text=gsub("#","",text)
  
  text=tolower(text)
  text=str_replace_all(text, "[\r\n]" , "")
  text=stemDocument(text, language = "english")
  text=removeWords(text,stopwords(source = "smart")) #removeWords(text,stopwords("en"))
  text=stripWhitespace(text)
  #text=removeWords(text,smittenkitchen)
  text=removePunctuation(text)
  for( i in 1:20) {
    print(text[i])
    print(smittenkitchen$text[i])
    print("")
    print("")
  }
  
  

  #tentativo LDA
  library("seededlda")
  dict=readLines("food_corrected.txt",n=216)
  dict=stemDocument(dict, language = "english")
  dict=tolower(dict)
  
  text=corpus(text)
  token_text <- tokens(text)
  dfm_text <- dfm(token_text)
  df=convert(dfm_text,to="data.frame")
  
  nomi_colonne=intersect(names(df),dict)
  
  
  df=subset(df, select = c("doc_id",nomi_colonne))
  
  somma=rowSums(df[,2:ncol(df)])
  df$somma=somma
  df=subset(df,somma>0)
  
  # library("topicsmodel")
  # library("readtext")
  
  dict_other=readLines("popular.txt",n=25322)
  dict_other=stemDocument(dict_other, language = "english")
  
  dict_other=removeWords(dict_other,dict)
  dict_other=dict_other[dict_other!=""]
  

  dict_other=removeWords(dict_other,stopwords(source = "smart"))
  dict_other=dict_other[dict_other!=""]
  
  dict_other=unique(dict_other)
  library("quanteda")
  dict=dictionary(list(food = dict,other=dict_other))
  lda<- textmodel_seededlda(dfm_text,dictionary=dict)
  
  
  
  spettro=read.csv("spettro.csv")
  spettro2=read.csv("spettro2.csv")
  spettro3=read.csv("spettro3.csv")
  spettro=read.csv("out.csv")
  
  
  
  
  gordon=get_timeline("GordonRamsay",n=1500,include_rts=FALSE,token=twitter_token)
  
  
  
  
  
  dict=readLines("food_corrected.txt",n=725)
  big_data=search_tweets(i,n = 1000,
                  type = "mixed",
                  include_rts = FALSE,
                  token = twitter_token,
                  retryonratelimit = FALSE,
                  lang = "en")
  
  for(i in dict) { 
      a=search_tweets(i,n = 1000,
                type = "mixed",
                include_rts = FALSE,
                token = twitter_token,
                retryonratelimit = TRUE,
                lang = "en")
    big_data=rbind.data.frame(big_data,a)
    
  }
  
  
  
  