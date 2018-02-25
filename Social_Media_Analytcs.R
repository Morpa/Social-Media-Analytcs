#Instalando as bibliotecase carregando:
install.packages("twitteR")
installed.packages("httr")
library(twitteR)
library(httr)

#Carregando a bibliteca responsável pela limpeza dos dados:
source("Limpeza.R")

#Autenticação na API:
key <- "7l0a0DfRbWRgUyBVsd2a6dyZW4"
secret <- "4OVz8W3RtW46cXo7ccVt3wxRukFdqN9glpRprcHX2Olc66wAyKXB"
token <- "947053364109627392-dVL1MyXDiD53NH2f0W2r2Ua3VnSlU7w9"
tokensecret <- "UKNOThjDqp2E7ddxtovLbr368wtjqKrAFPyy6Gqt8rvWXvF"

# Autenticação:
setup_twitter_oauth(key, secret, token, tokensecret)
userTimeline("morpa")

#Selecionando os dados a serem analisados:
tema <- "#Brasil"
qtd <- 1500
idioma <- "pt"
tweetdata = searchTwitter(tema, n = qtd, idioma)
head(tweetdata)

#Instalando e carregando as bibliotecas de Text Mining:
install.packages("tm")
install.packages("SnowballC")
library(tm)
library(SnowballC)

tweetlist <- sapply(tweetdata, function(x) x$getText())
tweetlist <- iconv(tweetlist, to = "utf-8", sub="")
tweetlist <- limpaTweets(tweetlist)
tweetcorpus <- Corpus(VectorSource(tweetlist))
tweetcorpus <- tm_map(tweetcorpus, removePunctuation)
tweetcorpus <- tm_map(tweetcorpus, content_transformer(tolower))
tweetcorpus <- tm_map(tweetcorpus, function(x)removeWords(x, stopwords("portuguese")))

#Instalando e carregando o WordClound:
install.packages("RColorBrewer")
install.packages("wordclound")
library(RColorBrewer)
library(wordcloud)

#Gerando a nuvem:
palavras <- brewer.pal(8, "Dark2")

wordcloud(tweetcorpus,
          min.freq = 2,
          scale = c(3,1),
          random.color = F,
          max.words = 100,
          random.order = F,
          colors = palavras)

#Instalando e carregando o classificador Naive Bayes para analise de sentimento:
install.packages("~/Documents/Materiais Curso/Big Data Analytics com R e Microsoft Azure Machine Learning/Projetos/Social Media Analytcs/Rstem_0.4-1.tar.gz", repos = NULL, type = "source")
install.packages("~/Documents/Materiais Curso/Big Data Analytics com R e Microsoft Azure Machine Learning/Projetos/Social Media Analytcs/sentiment_0.2.tar.gz", repos = NULL, type = "source")
install.packages("ggplot2")
install.packages("plyr")
library(Rstem)
library(sentiment)
library(ggplot2)
library(plyr)

tweetpt <- searchTwitter("#Brasil", n = 1500, lang = "pt")
tweetpt <- sapply(tweetpt, function(x) x$getText())

# Removendo caracteres especiais:
tweetpt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweetpt)
# Removendo @
tweetpt = gsub("@\\w+", "", tweetpt)
# Removendo pontuação
tweetpt = gsub("[[:punct:]]", "", tweetpt)
# Removendo digitos
tweetpt = gsub("[[:digit:]]", "", tweetpt)
# Removendo links html
tweetpt = gsub("http\\w+", "", tweetpt)
# Removendo espacos desnecessários
tweetpt = gsub("[ \t]{2,}", "", tweetpt)
tweetpt = gsub("^\\s+|\\s+$", "", tweetpt)

# Criando função para tolower:
try.error = function(x)
{
  # Criando missing value
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

#Lower case:
tweetpt = sapply(tweetpt, try.error)

#Removendo os NAs:
tweetpt = tweetpt[!is.na(tweetpt)]
names(tweetpt) = NULL

#Classificando Emoção:
class_emo = classify_emotion(tweetpt, algorithm = "bayes", prior = 1.0)
emotion = class_emo[,7]

#Substituindo NA por Desconhecido:
emotion[is.na(emotion)] = "Unknow"

#Classificar polaridade:
class_pol = classify_polarity(tweetpt, algorithm = "bayes")
polarity = class_pol[,4]

#Gerando data frame:
sent_df = data.frame(text = tweetpt, emotion = emotion,
                     polarity = polarity, stringsAsFactors = FALSE)

# Ordenando o dataframe:
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels = names(sort(table(emotion), 
                                                                decreasing=TRUE))))


# Emoções encontradas:
ggplot(sent_df, aes(x = emotion)) +
  geom_bar(aes(y = ..count.., fill = emotion)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Categorias", y = "Numero de Tweets") 

# Polaridade:
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x = "Polaridade do Sentimento", y = "Numero de Tweets")









