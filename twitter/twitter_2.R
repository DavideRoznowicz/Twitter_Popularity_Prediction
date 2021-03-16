library(rtweet)
library(tm)
library(stringr)
library(quanteda)
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
#,"GordonRamsay"

user=c("smittenkitchen","heyadamroberts","thedomesticman","nomnompaleo",
       "Food52","balancedbites","CookingChannel","bflay","GDeLaurentiis",
       "shakeshack","ottolenghi","testkitchen","therealweissman")

dataset=get_timeline("foodwishes",n=35000,include_rts=FALSE,token=twitter_token)


for(i in user) {
  temp=get_timeline(i,n=35000,include_rts=FALSE,token=twitter_token)
  dataset=rbind.data.frame(dataset,temp)
    }

#smittenkitchen=get_timeline("GordonRamsay",n=1500,include_rts=FALSE,token=twitter_token)
#dataset_backup=dataset
text=dataset$text

text=gsub("http[^>]+$"," ",text) #remove last link
text=gsub("http[^>]+ "," ",text) #remove other link
text=gsub("@[^>]+$"," ",text) #remove last @
text=gsub("@[^>]+ "," ",text) #remove other @
###rimuovere punteggiatura per bene, numeri, etc
text=gsub("#"," ",text)
text=tolower(text)
text=str_replace_all(text, "[\r\n]" , " ")
text=stemDocument(text, language = "english")
text=removeWords(text,stopwords(source = "smart")) #removeWords(text,stopwords("en"))
text=stripWhitespace(text)
#text=removeNumbers(text)
#text=gsub("-"," ",text)
#text=gsub("'"," ",text)
text=removePunctuation(text)
dict=readLines("food_corrected.txt",n=725)
dict=unique(dict)
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
df$doc_id=gsub("text","",df$doc_id)
df$doc_id=as.numeric(df$doc_id)
dataset=dataset[df$doc_id,]

write.csv(dataset$media_url[!is.na(dataset$media_url)],file="url.csv")

spettro=read.csv("out.csv")
dataset$media_url=basename(unlist(dataset$media_url))
spettro$vec<- data.matrix(spettro[,2:51])
library(dplyr)
#dataset %>%  filter(spettro$id %in% dataset$media_url)
#sub=subset(dataset, media_url %in% spettro$id)
dataset$media_url=gsub(".png",".jpg",dataset$media_url)

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

da_eliminare=outersect(dataset$media_url,spettro$id)
#dataset_tmp=data.frame(dataset)
for(i in da_eliminare) {
  
  dataset=subset(dataset,(media_url!=i | is.na(media_url)))
  
}

#dataset=data.frame(dataset_tmp)
black=spettro$vec[1,]
black[1:50]=rep(0.0,50)
black[1]=1.0

dataset$c0=rep(1,nrow(dataset))
dataset$c1=rep(0,nrow(dataset))
dataset$c2=rep(0,nrow(dataset))
dataset$c3=rep(0,nrow(dataset))
dataset$c4=rep(0,nrow(dataset))
dataset$c5=rep(0,nrow(dataset))
dataset$c6=rep(0,nrow(dataset))
dataset$c7=rep(0,nrow(dataset))
dataset$c8=rep(0,nrow(dataset))
dataset$c9=rep(0,nrow(dataset))
dataset$c10=rep(0,nrow(dataset))
dataset$c11=rep(0,nrow(dataset))
dataset$c12=rep(0,nrow(dataset))
dataset$c13=rep(0,nrow(dataset))
dataset$c14=rep(0,nrow(dataset))
dataset$c15=rep(0,nrow(dataset))
dataset$c16=rep(0,nrow(dataset))
dataset$c17=rep(0,nrow(dataset))
dataset$c18=rep(0,nrow(dataset))
dataset$c19=rep(0,nrow(dataset))
dataset$c20=rep(0,nrow(dataset))
dataset$c21=rep(0,nrow(dataset))
dataset$c22=rep(0,nrow(dataset))
dataset$c23=rep(0,nrow(dataset))
dataset$c24=rep(0,nrow(dataset))
dataset$c25=rep(0,nrow(dataset))
dataset$c26=rep(0,nrow(dataset))
dataset$c27=rep(0,nrow(dataset))
dataset$c28=rep(0,nrow(dataset))
dataset$c29=rep(0,nrow(dataset))
dataset$c30=rep(0,nrow(dataset))
dataset$c31=rep(0,nrow(dataset))
dataset$c32=rep(0,nrow(dataset))
dataset$c33=rep(0,nrow(dataset))
dataset$c34=rep(0,nrow(dataset))
dataset$c35=rep(0,nrow(dataset))
dataset$c36=rep(0,nrow(dataset))
dataset$c37=rep(0,nrow(dataset))
dataset$c38=rep(0,nrow(dataset))
dataset$c39=rep(0,nrow(dataset))
dataset$c40=rep(0,nrow(dataset))
dataset$c41=rep(0,nrow(dataset))
dataset$c42=rep(0,nrow(dataset))
dataset$c43=rep(0,nrow(dataset))
dataset$c44=rep(0,nrow(dataset))
dataset$c45=rep(0,nrow(dataset))
dataset$c46=rep(0,nrow(dataset))
dataset$c47=rep(0,nrow(dataset))
dataset$c48=rep(0,nrow(dataset))
dataset$c49=rep(0,nrow(dataset))
dataset$image<- data.matrix(dataset[,91:140])
dataset$image <- spettro[match(dataset$media_url,spettro$id),"vec"]
dataset_tmp=data.frame(dataset)

dataset[is.na(dataset$media_url),]$image=data.matrix(dataset[is.na(dataset$media_url),91:140])
dataset[is.na(dataset$media_url),]$media_url=""

hashtags=!is.na(dataset$hashtags)#
mention=!is.na(dataset$mentions_screen_name)#
status_id=dataset$status_id#
retweet_count=dataset$retweet_count#
favorite_count=dataset$favorite_count #
text=dataset$text#
display_text_width=dataset$display_text_width#
link=!is.na(dataset$urls_t.co)#
spettro=dataset$image
followers=dataset$followers_count#
friends=dataset$friends_count#
lists=dataset$listed_count#
trainino=data.frame(#status_id,
                    #retweet_count,
                    favorite_count,
                    #text,
                    display_text_width,
                    mention,hashtags,
                    followers,
                    friends,
                    lists,
                    link)
trainino$spettro=data.matrix(spettro[,1:50])
#trainino$retweet_count[trainino$retweet_count<=50]=0
#trainino$retweet_count[trainino$retweet_count>50]=1

#trainino$favorite_count[trainino$favorite_count>=50]=2
#trainino$favorite_count[trainino$favorite_count>0 & trainino$favorite_count<5]=0
#trainino$favorite_count[trainino$favorite_count>=5 & trainino$favorite_count<50]=1
#trainino$favorite_count[trainino$favorite_count>=50]=2

###################################################
trainino$favorite_count[trainino$favorite_count>=0 & trainino$favorite_count<50]=0
trainino$favorite_count[trainino$favorite_count>=50]=1
###################################################


#trainino$favorite_count[trainino$favorite_count>=200]=3
#trainino$retweet_count[trainino$retweet_count<=20]=0
#trainino$retweet_count[trainino$retweet_count>20]=1

#trainino$retweet_count[trainino$retweet_count==0]=0
#trainino$retweet_count[trainino$retweet_count>0 & trainino$retweet_count<=5]=1
#trainino$retweet_count[trainino$retweet_count>5 & trainino$retweet_count<=10]=2
#trainino$retweet_count[trainino$retweet_count>5]=2

#trainino$retweet_count[trainino$retweet_count>=0 & trainino$retweet_count<3]=0
#trainino$retweet_count[trainino$retweet_count>=3 & trainino$retweet_count<=10]=1
#trainino$retweet_count[trainino$retweet_count>10]=2





###########################################################################################################
text=dataset$text
library(stringr)
library(tm)
text=gsub("http[^>]+$"," ",text) #remove last link
text=gsub("http[^>]+ "," ",text) #remove other link
text=gsub("@[^>]+$"," ",text) #remove last @
text=gsub("@[^>]+ "," ",text) #remove other @
text=gsub("#"," ",text)
text=tolower(text)
text=str_replace_all(text, "[\r\n]" , " ")
text=stemDocument(text, language = "english")
text=removeWords(text,stopwords(source = "smart")) 
text=removeWords(text,stopwords("en"))
text=stripWhitespace(text)
text=gsub("-"," ",text)
text=gsub("'"," ",text)
#text=gsub("."," ",text)
text=gsub("…"," ",text)
text=gsub("[|]"," ",text)
text=gsub("pm"," ",text)
text=gsub("am"," ",text)
text=removeNumbers(text)
text=removePunctuation(text,ucp = TRUE)

ch=c("a")
for(i in 98:122)
{
  ch=c(ch,intToUtf8(i))
}

text=stemDocument(text, language = "english")
text=removeWords(text,ch)
#text=corpus(text)
token_text <- tokens(text)

dfm_text <- dfm(token_text)
###############################################################################
#tfv <- TfIdfVectorizer$new(max_features = 320, remove_stopwords = FALSE)

#dfm_text <- tfv$fit_transform(text)

###################################################################à###########

nomi_colonne=intersect(featnames(dfm_text),dict)
df=convert(dfm_text,to="data.frame")





to_plot=subset(df, select = c(nomi_colonne)) #prendiamo solo l'intersezione con il dictionary
to_plot=colSums(to_plot) #sommiamo le righe
occ_cucina=data.frame(to_plot) 
#to_plot=to_plot/sum(to_plot)
library("wordcloud")
#wordcloud(words =rownames(occ_cucina), freq =occ_cucina[,1],min.freq = 1,
#          max.words=400, random.order=FALSE, rot.per=0.35, 
#          colors=brewer.pal(8, "Dark2"))

nomi_colonne=intersect(featnames(dfm_text),outersect(featnames(dfm_text),dict))
to_plot=subset(df, select = c(nomi_colonne))
to_plot=colSums(to_plot)
occ=data.frame(to_plot)

to_plot=to_plot/sum(to_plot)

wordcloud(words =rownames(occ), freq =occ[,1],
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud(words =colnames(set_parole), freq =occ[,1],
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

parole_cibo=rownames(occ_cucina)[occ_cucina$to_plot>20]
parole_altro=rownames(occ)[occ$to_plot>150]
filtro=c(parole_altro,parole_cibo)
set_parole=df[filtro]

library(data.table)
x_train <- cbind(set_parole, trainino)
x_train=data.table(x_train)
indici=sample(seq_len(nrow(trainino)), size = 8000)
train=x_train[indici]
test=x_train[-indici]
library(superml)
library("wordcloud")

xgb <- RFTrainer$new(n_estimators = 500,classification=1)
xgb$fit(train, "favorite_count")
predictions <- xgb$predict(test)
#library(randomForest)
#train$retweet_count=as.factor(train$retweet_count)
#rf=randomForest(train,formula=retweet_count~.)
library(EvaluationMeasures)
EvaluationMeasures.FNR(Predicted = predictions,Real = test$favorite_count,Positive = 1)
EvaluationMeasures.FPR(Predicted = predictions,Real = test$favorite_count,Positive = 1)
EvaluationMeasures.F1Score(Predicted = predictions,Real = test$favorite_count)
EvaluationMeasures.Accuracy(Predicted = predictions,Real = test$favorite_count)


##################################################################################################à



text=dataset$text
text=gsub("http[^>]+$"," ",text) #remove last link
text=gsub("http[^>]+ "," ",text) #remove other link
text=gsub("@[^>]+$"," ",text) #remove last @
text=gsub("@[^>]+ "," ",text) #remove other @
text=gsub("#"," ",text)
text=tolower(text)
text=str_replace_all(text, "[\r\n]" , " ")
text=stemDocument(text, language = "english")
#text=removeWords(text,stopwords(source = "smart")) 
#text=removeWords(text,stopwords("en"))
text=stripWhitespace(text)
text=gsub("-"," ",text)
text=gsub("'"," ",text)
#text=gsub("."," ",text)
text=gsub("…"," ",text)
text=gsub("[|]"," ",text)
text=gsub("pm"," ",text)
text=gsub("am"," ",text)
text=removeNumbers(text)
text=removePunctuation(text,ucp = TRUE)
ch=c("a")
for(i in 98:122)
{
  ch=c(ch,intToUtf8(i))
}

text=removeWords(text,ch)
text=stemDocument(text, language = "english")
library(superml)

cfv <- CountVectorizer$new(max_features = 300, remove_stopwords = TRUE)
cf_mat <- cfv$fit_transform(text)

nomi_colonne=intersect(colnames(cf_mat),dict)
to_plot=subset(cf_mat, select = c(nomi_colonne)) #prendiamo solo l'intersezione con il dictionary
to_plot=colSums(to_plot) #sommiamo le righe
occ_cucina=data.frame(to_plot) 
#to_plot=to_plot/sum(to_plot)
library("wordcloud")
wordcloud(words =rownames(occ_cucina), freq =occ_cucina[,1],min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

nomi_colonne=intersect(colnames(cf_mat),outersect(colnames(cf_mat),dict))
to_plot=subset(cf_mat, select = c(nomi_colonne))
to_plot=colSums(to_plot)
occ=data.frame(to_plot)

#to_plot=to_plot/sum(to_plot)
wordcloud(words =rownames(occ), freq =occ[,1],
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
library(data.table)

indici=sample(seq_len(nrow(trainino)), size = 6000)
x_train <- cbind(cf_mat, trainino)
x_train=data.table(x_train)
train=x_train[indici]
test=x_train[-indici]
xgb <- RFTrainer$new(n_estimators = 358)
xgb$fit(train, "retweet_count")

err=abs(as.numeric(predictions[50:150])-as.numeric(test$retweet_count[50:150]))/as.numeric(test$retweet_count[50:150])
predictions <- xgb$predict(test)
plot(as.numeric(predictions[50:150]))
lines(as.numeric(test$retweet_count[50:150]),col="red")


plot(as.numeric(predictions[1:90])-1)
points(as.numeric(test$retweet_count[1:90]),col="red",pch = 4)
visualizza=data.frame(predictions,test$favorite_count,res=predictions==test$favorite_count)
c0=sum(train$favorite_count==0)
err0=sum(visualizza[visualizza$test.favorite_count==0,]$res==TRUE)/c0
c1=sum(train$favorite_count==1)
err1=sum(visualizza[visualizza$test.favorite_count==1,]$res==TRUE)/c1
c2=sum(train$favorite_count==2)
err2=sum(visualizza[visualizza$test.favorite_count==2,]$res==TRUE)/c2
c3=sum(train$favorite_count==3)
err3=sum(visualizza[visualizza$test.favorite_count==3,]$res==TRUE)/c3



wordcloud(words =rownames(occ_cucina), freq =occ_cucina[,1],
                      min.freq = 1,  max.words = 9000, random.order=FALSE,colors=brewer.pal(8, "Dark2"))
library(wordcloud2)
#devtools::install_github("lchiffon/wordcloud2")
wordcloud2(data=occ_cucina, size=1.6, color='random-dark')

figPath = system.file("examples/t.png",package = "wordcloud2")

test=data.frame("word"=rownames(occ_cucina),"freq"=occ_cucina[,1])
wordcloud2(data=test, figPath = figPath, size = 1.7,color = "skyblue")

test2=data.frame("word"=rownames(occ),"freq"=occ[,1])
wordcloud2(data=test2, figPath = figPath, size = 1.5,color = "skyblue")

wordcloud2(data=test, size=1.6)

test3=data.frame("word"=colnames(set_parole),"freq"=colSums(set_parole))
wordcloud2(data=test3, figPath = figPath, size = 1.5,color = "skyblue")
