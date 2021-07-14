install.packages("plyr")
library(rtweet)
library(dplyr)
library(twitteR)
library(tidyverse)


appname <- "RStudio Mllerena"
api_key <- "iDa3T0AvK6MnopecvWytrBRLx"
api_secret <- "H6dpMDzWMjdl5wbptoA8yJzNxQnOCfjMtNl0XxvmMn1vRRzWcf"
access_token <- "1146148717-WcP9psJEXhQSfdJERKW52HMx14EcT8yV0pkJJWp"
access_token_secret <- "8owmaThLUyYaV4Mxlqac76cNLok3jZN0nFoTR7dGWDgOK"

token = create_token(app = appname, consumer_key = api_key,
                     consumer_secret = api_secret, access_token, 
                     access_secret = access_token_secret, set_renv = T)


query <- "cultura (#cultura) lang:es" 
tweets_c2u = search_tweets2(query, n=30000, geocode= "-77.5000000, -2.0000000")

#Filtramos el texto de los tuits y creamos una función cleaning para eliminar datos imnecesarios
texts <- sapply(tweetscultura, function(x) x$getText())

tweetscultura <- read_excel("C:/Users/Miguel/Desktop/dataset PRACTICASfinal/tweetscultura.xlsx")

#ANALISIS DE SENTIMIENTO
class_emo <- classify_emotion (tweetscultura, algorithm = "bayes", prior = 1)
head(class_emo)

emotion <- class_emo[,7]
emotion[is.na(emotion)]<-"unknown"
head(emotion)

class_pol <- classify_polarity(tweetscultura, algorithm = "bayes")
head(class_pol)
polarity<- class_pol[,4]


sent_df <- data.frame(text=tweetscultura, emotion=emotion, polarity = polarity, stringsAsFactors = F)

sent_df <- within(sent_df, emotion <- factor(emotion, levels = names(sort(table(emotion), decreasing= T))))

library(RColorBrewer)

ggplot(sent_df, aes(x=emotion))+
  geom_bar(aes(y=..count.., fill=emotion))+
  scale_fill_brewer(palette = "Set2")+
  labs(x="Categorias de emocion", y = "Numero de Tweets")+
  labs(title = "Analisis de Sentimiento acerca de Cultura")

ggplot(sent_df, aes(x=polarity))+
  geom_bar(aes(y = ..count.., fill = polarity))+
  scale_fill_brewer(palette = "Set3")+
  labs(x="Categorias de polaridad", y = "Numero de Tweets")+
  labs(title="Analisis de Sentimiento acerca de Cultura")



