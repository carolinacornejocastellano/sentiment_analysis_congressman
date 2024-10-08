## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Librerías------------------------------------------------------------------------------------------------------------------
libraries <- c(
  "rtweet",
  "tidyverse",
  "twitteR",
  "ROAuth",
  "httr",
  "tm",
  "SnowballC",
  "rio",
  "wordcloud",
  "syuzhet",
  "lda",
  "RColorBrewer"
)

for (lib in libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib)
  }
  suppressPackageStartupMessages(library(lib, character.only = TRUE))
}

# remove vector and iterator
rm(lib, libraries)

## -------------------------------------------------------------------------------------------------------------------------------
Github <- "https://github.com/castellco/cavero-sentiments/raw/main/Cavero_Tweets.csv"

Cavero_All_Tweets <- import(Github, encoding ="UTF-8")


## -------------------------------------------------------------------------------------------------------------------------------
max(Cavero_All_Tweets$created_at); min(Cavero_All_Tweets$created_at)
head(Cavero_All_Tweets$text)


## -------------------------------------------------------------------------------------------------------------------------------
Es_Retweet <- Cavero_All_Tweets |> 
  group_by(is_retweet) |> 
  summarise(Cantidad = n()) |> 
  ggplot(aes(x = is_retweet, y = Cantidad, fill = is_retweet)) +
  geom_bar(position = "dodge", stat = 'identity') + 
  xlab('¿Es retweet?') + 
  ggtitle("Retweets y tweets propios de A. Cavero") + 
  theme(legend.position = "none")
Es_Retweet


## -------------------------------------------------------------------------------------------------------------------------------
Cavero_RT_Deleted <- Cavero_All_Tweets[Cavero_All_Tweets$is_retweet == 'FALSE',]

Cavero_Text <- Cavero_RT_Deleted$text


## -------------------------------------------------------------------------------------------------------------------------------
Corpus <- Corpus(VectorSource(Cavero_Text))
length(Cavero_Text)
content(Corpus[1])


## ----Minúsculas-----------------------------------------------------------------------------------------------------------------
content(Corpus[1])
Corpus <- tm_map(Corpus,content_transformer(tolower)) 
content(Corpus[1])


## ----Eliminar links-------------------------------------------------------------------------------------------------------------
content(Corpus[1])
Quitar_URL <- function(x) gsub("http[^[:space:]]*", "", x)
Corpus <- tm_map(Corpus, content_transformer(Quitar_URL))
content(Corpus[1])

## ----Eliminar tildes------------------------------------------------------------------------------------------------------------
content(Corpus[1])
Quitar_Tilde <- function(x) chartr('áéíóú','aeiou',x)
Corpus <- tm_map(Corpus, Quitar_Tilde)
content(Corpus[1])

## ----Eliminar signos de pregunta------------------------------------------------------------------------------------------------
content(Corpus[1])
Quitar_Interrogacion1 <- function(x) chartr('?',' ',x)
Quitar_Interrogacion2 <- function(x) chartr('¿',' ',x)
Corpus <- tm_map(Corpus, Quitar_Interrogacion1)
Corpus <- tm_map(Corpus, Quitar_Interrogacion2)
content(Corpus[1])

## ----Eliminar signos de exclamación---------------------------------------------------------------------------------------------
content(Corpus[1])
Quitar_Exclamacion1 <- function(x) chartr('¡',' ',x)
Quitar_Exclamacion2 <- function(x) chartr('!',' ',x)
Corpus <- tm_map(Corpus, Quitar_Exclamacion1)
Corpus <- tm_map(Corpus, Quitar_Exclamacion2)
content(Corpus[1])


## ----Eliminar handles-----------------------------------------------------------------------------------------------------------
content(Corpus[91])
Quitar_Usuarios <- function(x) gsub("@\\w+", "", x)
Corpus <- tm_map(Corpus, Quitar_Usuarios)
content(Corpus[91])


## ----Eliminar números-----------------------------------------------------------------------------------------------------------
content(Corpus[1])
Corpus <- tm_map(Corpus, removeNumbers)
content(Corpus[1])


## ----Quitar puntuación----------------------------------------------------------------------------------------------------------
content(Corpus[3])
Corpus <- tm_map(Corpus, removePunctuation)
content(Corpus[3])


## -------------------------------------------------------------------------------------------------------------------------------
content(Corpus[1])
Corpus <- tm_map(Corpus, removeWords,c(stopwords("spanish")))
content(Corpus[1])

## ----Stopwords específicos------------------------------------------------------------------------------------------------------
content(Corpus[1])
Corpus <- tm_map(Corpus, removeWords,c("mas", "asi", "ser", "aqui", ""))
content(Corpus[1])


## -------------------------------------------------------------------------------------------------------------------------------
content(Corpus[1])
Corpus <- tm_map(Corpus, stripWhitespace)
content(Corpus[1])


## ----Matriz de términos---------------------------------------------------------------------------------------------------------
Terminos <- TermDocumentMatrix(Corpus)

Terminos

## -------------------------------------------------------------------------------------------------------------------------------
inspect(Terminos)

## ----Frecuencia de palabras-----------------------------------------------------------------------------------------------------
findFreqTerms(Terminos,lowfreq = 10) # al menos 10 veces


## ----Matriz---------------------------------------------------------------------------------------------------------------------
Matriz <- as.matrix(Terminos)
head(Matriz)


## -------------------------------------------------------------------------------------------------------------------------------
Decreciente <- sort(rowSums(Matriz),decreasing=TRUE)
head(Decreciente)


## ----Crear DF-------------------------------------------------------------------------------------------------------------------
Cavero_DF <- data.frame(Palabra = names(Decreciente), freq=Decreciente)

Cavero_DF


## -------------------------------------------------------------------------------------------------------------------------------
Cavero_DF2 <- subset(Cavero_DF, Cavero_DF$freq >= 10)
Cavero_DF2


## -------------------------------------------------------------------------------------------------------------------------------
hist(Cavero_DF2$freq)


## ----Guardar gráfico para el reporte--------------------------------------------------------------------------------------------
jpeg(file="hist1.jpeg")
hist(Cavero_DF2$freq)
dev.off()


## -------------------------------------------------------------------------------------------------------------------------------
ggplot(Cavero_DF2, aes( x = Palabra, y = freq )) +
  geom_bar(stat = "identity", width = 0.6, fill="steelblue") +
  xlab("Términos") + 
  ylab("Frecuencia") + 
  coord_flip() +
  theme(axis.text = element_text(size = 9))


## -------------------------------------------------------------------------------------------------------------------------------
barplot(Cavero_DF2[1:20,]$freq, las = 2, names.arg = Cavero_DF2[1:20,]$Palabra,
        col ="gold", main ="Top 5 palabras más frecuentes",
        ylab = "Palabras más frecuentes")


## ----Guardar barplot de palabras más usadas en el mismo chunk-------------------------------------------------------------------
jpeg(file="barplot.jpeg")
barplot(Cavero_DF2[1:20,]$freq, las = 2, names.arg = Cavero_DF2[1:20,]$Palabra,
        col ="gold", main ="Top 5 palabras más frecuentes",
        ylab = "Palabras más frecuentes")
dev.off()


## -------------------------------------------------------------------------------------------------------------------------------
#head(Cavero_DF)
#Conteo_Palabras <- data.frame(freq = apply(Palabra,1,sum))
#head(Conteo_Palabras)

wordcloud(Cavero_DF$Palabra, 
          Cavero_DF$freq, 
          random.order = FALSE, 
          min.freq = 2, 
          colors = brewer.pal(8, "Dark2"))


## -------------------------------------------------------------------------------------------------------------------------------
findAssocs(Terminos, c("peru"), c(0.20))


## -------------------------------------------------------------------------------------------------------------------------------
findAssocs(Terminos, c("libertad"), c(0.30))


## -------------------------------------------------------------------------------------------------------------------------------
findAssocs(Terminos, c("congreso"), c(0.30))


## -------------------------------------------------------------------------------------------------------------------------------
Cluster <- removeSparseTerms(Terminos, sparse = 0.96)
m2 <- as.matrix(Cluster)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, k = 5)


## -------------------------------------------------------------------------------------------------------------------------------
Sentimientos_tm <- analyzeSentiment(Cavero_Text,language = "spanish")
head(Sentimientos_tm)


## -------------------------------------------------------------------------------------------------------------------------------
Sentimientos_tm_df <- data.frame(Cavero_Text,
                               sentiment = convertToDirection(Sentimientos_tm$SentimentGI))
Sentimientos_tm_df

## -------------------------------------------------------------------------------------------------------------------------------
table(Sentimientos_tm_df$sentiment)


## -------------------------------------------------------------------------------------------------------------------------------
Sentimientos_tm_df |> 
  group_by(sentiment) |>
  summarise(Cantidad = n() ) |> 
  ggplot(aes(x = sentiment,y=Cantidad)) +
  geom_bar(aes(fill=sentiment),stat = "identity") +
  xlab("Sentimientos") + ylab("Cantidad") + ggtitle("Sentimentos de A. Cavero, según el paquete {SentimentAnalysis}") + 
  theme(legend.position = "none")

## -------------------------------------------------------------------------------------------------------------------------------
p2 <- Sentimientos_tm_df |> 
  mutate(title_len = str_length(Cavero_Text)) |> 
  ggplot(aes(x = title_len)) +
  geom_density(fill = "Orange") +
  theme_minimal() +
  labs(x = "String Length", title = "Tamaño de los tweets de Cavero")
p2


## ----Promedio de caracteres de los tweets de Cavero-----------------------------------------------------------------------------
 Sentimientos_tm_df2 <- Sentimientos_tm_df |> 
  mutate(title_len = str_length(Cavero_Text))

Sentimientos_tm_df2$title_len <- as.numeric(Sentimientos_tm_df2$title_len)
mean(Sentimientos_tm_df2$title_len) 


## -------------------------------------------------------------------------------------------------------------------------------
# Este paquete asocia textos a 8 emociones y 2 sentimientos. Tanto más excede el valor a 0, más pronunciada es la emoción o sentimiento.
lang <- "spanish"
Sentimientos_nrc <- get_nrc_sentiment(Cavero_Text, language = lang)
#Sentimientos_nrc_2 <- cbind(Cavero_Text,Sentimientos_nrc)
head(Sentimientos_nrc)


## -------------------------------------------------------------------------------------------------------------------------------
Cavero_angry <- which(Sentimientos_nrc$anger > 0)
head(Cavero_Text[Cavero_angry])


## -------------------------------------------------------------------------------------------------------------------------------
Cavero_joy <- which(Sentimientos_nrc$joy > 0)
head(Cavero_Text[Cavero_joy])

## -------------------------------------------------------------------------------------------------------------------------------
Cavero_sad <- which(Sentimientos_nrc$sadness > 0)
head(Cavero_Text[Cavero_sad])


## -------------------------------------------------------------------------------------------------------------------------------
Sentimientos_nrc |> 
  summarise_all(sum) |> 
  gather(key = Sentimiento, value = Number)


## -------------------------------------------------------------------------------------------------------------------------------
Sentimientos_nrc |> 
  summarise_all(sum) |> 
  gather(key = Sentimiento, value = Number) |> 
  ggplot(aes(x = Sentimiento,fill = Sentimiento)) + 
  geom_bar(aes(x = Sentimiento, y = Number), position = "dodge", stat = "identity") +
  xlab("Sentimientos") + ylab("Cantidad") + ggtitle("Sentimentos de A. Cavero, según paquete {syuzhet}") +
  theme(legend.position = "none")


## -------------------------------------------------------------------------------------------------------------------------------
require(lda)
corpusLDA <- lexicalize(Corpus)

ldaModel=lda.collapsed.gibbs.sampler(corpusLDA$documents,K=5,vocab=corpusLDA$vocab,burnin=9999,num.iterations=1000,alpha=0.7,eta=0.1)
top.words <- top.topic.words(ldaModel$topics, 8, by.score=TRUE)
print(top.words) 


