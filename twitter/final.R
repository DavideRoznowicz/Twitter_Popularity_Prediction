
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
trainino$favorite_count[trainino$favorite_count>=0 & trainino$favorite_count<50]=0
trainino$favorite_count[trainino$favorite_count>=50]=1
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
text=gsub("pm "," ",text)
text=gsub(" am "," ",text)
text=removeNumbers(text)
text=removePunctuation(text,ucp = TRUE)

ch=c("a")
for(i in 98:122)
{
  ch=c(ch,intToUtf8(i))
}
text=stemDocument(text, language = "english")
text=removeWords(text,ch)
token_text <- tokens(text)
dfm_text <- dfm(token_text)
nomi_colonne=intersect(featnames(dfm_text),dict)
df=convert(dfm_text,to="data.frame")

to_plot=subset(df, select = c(nomi_colonne)) #prendiamo solo l'intersezione con il dictionary
to_plot=colSums(to_plot) #sommiamo le righe
occ_cucina=data.frame(to_plot) 
nomi_colonne=intersect(featnames(dfm_text),outersect(featnames(dfm_text),dict))
to_plot=subset(df, select = c(nomi_colonne))
to_plot=colSums(to_plot)
occ=data.frame(to_plot)
occ=occ[order(occ$to_plot, decreasing = TRUE),,drop = FALSE]  
occ_cucina=occ_cucina[order(occ_cucina$to_plot,decreasing = TRUE),,drop = FALSE]

numero_parole=seq(100,800,100)
fpr_rf3=c()
fnr_rf3=c()
fpr_sv2=c()
fnr_sv2=c()
count=0
library(EvaluationMeasures)
for(i in numero_parole) {
count=count+1
#numero_cibo=round(i*(2/3))
#numero_altro=round(i*(1/3))

#if(numero_cibo>=nrow(occ_cucina)){ numero_cibo=nrow(occ_cucina); numero_altro=i-numero_cibo}
print(paste("totale parole",i))
#print(paste("seleziono parole cibo",numero_cibo,"/",i))

#print(paste("seleziono parole altro",numero_altro,"/",i))

parole_cibo=rownames(occ_cucina)#[1:numero_cibo]
parole_altro=rownames(occ)#[1:numero_altro]
filtro=c(parole_altro,parole_cibo)
filtro=filtro[1:i]
set_parole=df[filtro]
x_train <- cbind(set_parole, trainino)
x_train=data.table(x_train)
set.seed(10)
indici=sample(seq_len(nrow(trainino)), size = 12000)

train=x_train[indici]
test=x_train[-indici]

xgb$fit(train, "favorite_count")
predictions <- xgb$predict(test)
print(paste("totale finale feature",ncol(train)))
fnr_rf3[count]=EvaluationMeasures.FNR(Predicted = predictions,Real = test$favorite_count,Positive = 1)
fpr_rf3[count]=EvaluationMeasures.FPR(Predicted = predictions,Real = test$favorite_count,Positive = 1)
print(paste("FNR_rf=",fnr_rf3[count]))
print(paste("FPR_rf=",fpr_rf3[count]))
#train=data.frame(train)
#train_fc=train$favorite_count
#train_sv<- subset( train, select = -favorite_count )
#sv=svm(x=train_sv,y=train_fc ,type="C-classification")
#fc=test$favorite_count
#test_sv<- subset( test, select = -favorite_count )
#predizione=predict(sv,test_sv)

#fnr_sv[count]=EvaluationMeasures.FNR(Predicted = predizione,Real = fc,Positive = 1)
#fpr_sv[count]=EvaluationMeasures.FPR(Predicted = predizione,Real = fc,Positive = 1)
#print(paste("FNR_sv=",fnr_sv[count]))
#print(paste("FPR_sv=",fpr_sv[count]))

}

aaa=data.frame(numero_parole,"fnr_rf"=fnr_rf2,"fpr_rf"=fpr_rf2)
bbb=data.frame(numero_parole=seq(100,1500,100),fnr_rf,fpr_rf)
ccc=rbind(aaa,bbb)
ccc=ccc[order(ccc$numero_parole),]

ggplot(ccc[1:10,], aes(x=numero_parole, y=fnr_rf))+scale_colour_manual(values=cbPalette)+
  geom_line(size=1,aes(x=numero_parole,y=fpr_rf,color="FPR"))+geom_line(size=1,aes(x=numero_parole,y=fnr_rf,color="FNR"))+
  labs(title="Random Forest", x="Numero paorle", y="FNR FPR") 



# #fb8b24
cbPalette <- c("#5F0F40","#9a031e","#EFAD2A","#e36414","#5f0f40")
update_geom_defaults("line", list(size = 1.2))
theme_set(theme_bw(base_size = 12))
ggplot(aaa, aes(x=numero_parole, y=fnr_rf))+scale_colour_manual(values=cbPalette)+
  geom_line(size=1,aes(x=numero_parole,y=fpr_rf,color="FPR"))+geom_line(size=1,aes(x=numero_parole,y=fnr_rf,color="FNR"))+
labs(title="Random Forest", x="Numero paorle", y="FNR FPR") 

numero_parole=seq(50,450,100)
fpr_sv2=c()
fnr_sv2=c()
count=0

for(i in numero_parole) {
  count=count+1
  numero_cibo=round(i*(2/3))
  numero_altro=round(i*(1/3))
  
  if(numero_cibo>=nrow(occ_cucina)){ numero_cibo=nrow(occ_cucina); numero_altro=i-numero_cibo}
  print(paste("totale parole",i))
  print(paste("seleziono parole cibo",numero_cibo,"/",i))
  
  print(paste("seleziono parole altro",numero_altro,"/",i))
  
  parole_cibo=rownames(occ_cucina)[1:numero_cibo]
  parole_altro=rownames(occ)[1:numero_altro]
  filtro=c(parole_altro,parole_cibo)
  set_parole=df[filtro]
  x_train <- cbind(set_parole, trainino)
  x_train=data.table(x_train)
  set.seed(10)
  indici=sample(seq_len(nrow(trainino)), size = 12000)
  
  train=x_train[indici]
  test=x_train[-indici]
  
  train=data.frame(train)
  train_fc=train$favorite_count
  train_sv<- subset( train, select = -favorite_count )
  sv=svm(x=train_sv,y=train_fc ,type="C-classification")
  fc=test$favorite_count
  test_sv<- subset( test, select = -favorite_count )
  predizione=predict(sv,test_sv)
  
  fnr_sv2[count]=EvaluationMeasures.FNR(Predicted = predizione,Real = fc,Positive = 1)
  fpr_sv2[count]=EvaluationMeasures.FPR(Predicted = predizione,Real = fc,Positive = 1)
  print(paste("FNR_sv=",fnr_sv2[count]))
  print(paste("FPR_sv=",fpr_sv2[count]))
  
}
bbb=data.frame(numero_parole,fnr_sv,fpr_sv)

ggplot(bbb, aes(x=numero_parole, y=fnr_sv))+scale_colour_manual(values=cbPalette)+
  geom_line(size=1,aes(x=numero_parole,y=fpr_sv,color="FPR"))+geom_line(size=1,aes(x=numero_parole,y=fnr_sv,color="FNR"))+
  labs(title="Random Forest", x="Numero paorle", y="FNR FPR") 

aaa1=data.frame(numero_parole=seq(50,450,100),"fnr_sv"=fnr_sv2,"fpr_sv"=fpr_sv2)
bbb1=data.frame(numero_parole=seq(100,500,100),fnr_sv,fpr_sv)
ccc1=rbind(aaa1,bbb1)
ccc1=ccc1[order(ccc1$numero_parole),]
totale=cbind(ccc[1:10,],ccc1)
totale=totale[,-4]

ggplot(totale)+scale_colour_manual(values=cbPalette)+
  geom_line(size=1,aes(x=numero_parole,y=fpr_rf,color="FPR RF"))+geom_line(size=1,aes(x=numero_parole,y=fnr_rf,color="FNR RF"),linetype="dotted")+
  geom_line(size=1,aes(x=numero_parole,y=fpr_sv,color="FPR SVM"))+geom_line(size=1,aes(x=numero_parole,y=fnr_sv,color="FNR SVM"),linetype="dotted")+
  labs(title="Random Forest", x="Numero paorle", y="FNR FPR") +
scale_colour_manual(labels = c("FNR RF","FNR SVM", "FPR RF","FPR SVM"), values = c("darkblue", "red","darkblue", "red"))+
  guides(color = guide_legend(override.aes = list(linetype = c( "solid","dotted","dotted","solid"))))
# list(linetype = c( "dotted","solid"))))





plot(fnr,type="l",ylim=c(0,1),col="red")
lines(fpr)

#regressione
###############################################
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
trainino$favorite_count[trainino$favorite_count>=0 & trainino$favorite_count<50]=0
trainino$favorite_count[trainino$favorite_count>=50]=1
parole_cibo=rownames(occ_cucina)[occ_cucina$to_plot>10]
parole_altro=rownames(occ)[occ$to_plot>30]
filtro=c(parole_altro,parole_cibo)
set_parole=df[filtro]

set_parole[set_parole>0]=1

library(data.table)
x_train <- cbind(set_parole, trainino)
x_train=data.table(x_train)
indici=sample(seq_len(nrow(trainino)), size = 12000)
train=x_train[indici]
test=x_train[-indici]
library(superml)
xgb <- RFTrainer$new(n_estimators = 500,classification=1)
xgb$fit(train, "favorite_count")
predictions <- xgb$predict(test)

library(EvaluationMeasures)
EvaluationMeasures.FNR(Predicted = predictions,Real = test$favorite_count,Positive = 1)
EvaluationMeasures.FPR(Predicted = predictions,Real = test$favorite_count,Positive = 1)
rmse(predictions,test$favorite_count)
##################################################
#SVM
##################################################
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
#trainino$spettro=data.matrix(spettro[,1:50])
trainino$favorite_count[trainino$favorite_count>=0 & trainino$favorite_count<50]=0
trainino$favorite_count[trainino$favorite_count>=50]=1
parole_cibo=rownames(occ_cucina)[occ_cucina$to_plot>120]
parole_altro=rownames(occ)[occ$to_plot>300]
filtro=c(parole_altro,parole_cibo)
set_parole=df[filtro]

set_parole[set_parole>0]=1

set.seed(100)
x_train <- cbind(set_parole, trainino)
x_train=data.table(x_train)
indici=sample(seq_len(nrow(trainino)), size = 13000)
train=x_train[indici]
test=x_train[-indici]
library(e1071)
train=data.frame(train)
train_fc=train$favorite_count
train<- subset( train, select = -favorite_count )
sv=svm(x=train,y=train_fc ,type="C-classification")

fc=test$favorite_count
test<- subset( test, select = -favorite_count )
predizione=predict(sv,test)
library(EvaluationMeasures)
EvaluationMeasures.FNR(Predicted = predizione,Real=fc,Positive = 1)
EvaluationMeasures.FPR(Predicted = predizione,Real=fc,Positive = 1)



 plot(fnr_rf3,type="l",ylim=c(0,0.5))
 lines(fnr_rf[1:8],col="red")

 
 plot(fpr_rf3,type="l",ylim=c(0,0.3))
 lines(fpr_rf[1:8],col="red")

