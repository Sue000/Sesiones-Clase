# Programa Big Data y Business Intelligence
# M2.1.3-Integración de Datos - RStudio
# Enrique Onieva Caracuel


packages <- c("tm","NLP","reshape2","ggplot2","wordcloud","stringr","quanteda",
              "RWeka","dplyr")
new <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(new)) install.packages(new)
a=lapply(packages, require, character.only=TRUE)


rm(list = ls());cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()



#####################################################################
# Parte 01: Lectura y bag of words
#####################################################################
rm(list=ls())

twitter = read.csv("twitter.csv",stringsAsFactors = FALSE)

twitter = twitter[,c("ID.del.Tweet","tiempo","Texto.del.Tweet","impresiones")]
colnames(twitter) = c("id","fecha","texto","impresiones")



# Obtención y exploración del corpus
texto = twitter$texto
corpus = Corpus(VectorSource(texto))

corpus[[1]]$meta
corpus[[1]]$content

# Construcción de la matriz término/documento (o viceversa)
tdm = TermDocumentMatrix(corpus)
dtm = DocumentTermMatrix(corpus)

# Se trata de una matriz dispersa (representada como una lista fila-columna-valor)
head(cbind(tdm$i,tdm$j,tdm$v),100)
head(cbind(tdm$dimnames$Terms[1:100],tdm$dimnames$Docs[1:100]))

# Para trabajar, debemos convertir a una matriz no dispersa
word_matrix = t(as.matrix(tdm))
word_matrix2 = (as.matrix(dtm))

# Extracción de las frecuencias de palabras, y selección de las más frecuentes
palabras = melt(word_matrix)
palabras = palabras[palabras$value>0,]
frecuencias = aggregate(value~Terms,data=palabras,sum)
# 40 MÁS FRECUENTES
frecuencias_short = frecuencias[order(frecuencias$value,decreasing = TRUE)[1:40],]

# Dibujo de las palabras más frecuentes
ggplot(frecuencias_short, aes(x = reorder(Terms, value), y = value, fill = value))+
  geom_col() + coord_flip() + 
  ggtitle("Palabras Crudas") + 
  scale_fill_gradient(low = "gray", high = "darkgreen")

#Hacemos la nube de palabras :)
wordcloud(words = frecuencias$Terms, freq = frecuencias$value, 
          random.order = FALSE, colors = brewer.pal(8, "Accent"), min.freq = 1, max.words = 100)






#####################################################################
# Parte 02: Limpieza y preparación de texto
#####################################################################

twitter$textopreparado = tolower(twitter$texto)

# Usamos expresiones regulares para enriquecer el dataset con nuevas columnas
twitter$mencion = str_match(twitter$textopreparado, "@[\\w]+")
twitter$responde = str_match(twitter$textopreparado, "^@[\\w]+")
twitter$enlace = str_match(twitter$textopreparado, "http[a-zA-Z0-9:/._]*")
twitter$tag = str_match(twitter$textopreparado, "#[\\w]+")


# Y eliminamos la información extraída del texto
twitter$textopreparado = str_replace_all(twitter$textopreparado, "http[a-zA-Z0-9:/._]*","")
twitter$textopreparado = str_replace_all(twitter$textopreparado, "#[\\w]+","")
twitter$textopreparado = str_replace_all(twitter$textopreparado, "@[\\w]+","")
head(twitter$textopreparado)

# Además, eliminamos números, acentos...
twitter$textopreparado = str_replace_all(twitter$textopreparado,"[á]","a")
twitter$textopreparado = str_replace_all(twitter$textopreparado,"[é]","e")
twitter$textopreparado = str_replace_all(twitter$textopreparado,"[í]","i")
twitter$textopreparado = str_replace_all(twitter$textopreparado,"[ó]","o")
twitter$textopreparado = str_replace_all(twitter$textopreparado,"[ú]","u")

twitter$textopreparado = str_replace_all(twitter$textopreparado,"[^a-z ]","")

head(twitter$textopreparado)

# División de columnas (text mining - Todo igual que antes)
texto = twitter$textopreparado
corpus = Corpus(VectorSource(texto))
dtm = DocumentTermMatrix(corpus)
word_matrix = as.matrix(dtm)

# Extracción de palabras y dibujo (Igual que antes)
palabras = melt(word_matrix)
palabras = palabras[palabras$value>0,]
frecuencias = aggregate(value~Terms,data=palabras,sum)
frecuencias_short = frecuencias[order(frecuencias$value,decreasing = TRUE)[1:40],]
ggplot(frecuencias_short, aes(x = reorder(Terms, value), y = value, fill = value))+
  geom_col() + coord_flip() + 
  ggtitle("Palabras Crudas") + 
  scale_fill_gradient(low = "gray", high = "darkgreen")
wordcloud(words = frecuencias$Terms, freq = frecuencias$value, 
          random.order = FALSE, colors = brewer.pal(8, "Accent"), min.freq = 1, max.words = 100)






#####################################################################
# Parte 03: Eliminación de stopwords
#####################################################################

texto  = twitter$textopreparado
corpus = Corpus(VectorSource(texto))
corpus = tm_map(corpus, removeWords, stopwords("spanish"))
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removeWords, c("via"))
dtm = DocumentTermMatrix(corpus)
word_matrix = as.matrix(dtm)

# Dibujo (Todo igual que antes)
palabras = melt(word_matrix)
palabras = palabras[palabras$value>0,]
frecuencias = aggregate(value~Terms,data=palabras,sum)
frecuencias_short = frecuencias[order(frecuencias$value,decreasing = TRUE)[1:40],]
ggplot(frecuencias_short, aes(x = reorder(Terms, value), y = value, fill = value))+
  geom_col() + coord_flip() + 
  ggtitle("Palabras Crudas") + 
  scale_fill_gradient(low = "gray", high = "darkgreen")
wordcloud(words = frecuencias$Terms, freq = frecuencias$value, 
          random.order = FALSE, colors = brewer.pal(8, "Accent"), min.freq = 1, max.words = 100)



#####################################################################
# Parte 04: Uso de Bigramas
#####################################################################

# Un paréntesis interesante aquí es existen palabras "correlacionadas"
findAssocs(tdm, terms = c("data"), corlimit = 0.3)
findAssocs(tdm, terms = c("big"), corlimit = 0.3)
findAssocs(tdm, terms = c("science"), corlimit = 0.3)

# Construimos el corpus (ojo, con la función VCorpus) y eliminamos stopwords
texto  = twitter$textopreparado
corpus = VCorpus(VectorSource(texto)) #<- Aquí usamos VCorpus en lucar de Corpus
corpus = tm_map(corpus, removeWords, stopwords("spanish"))
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removeWords, c("via"))

# Al construir la matriz de documentos-términos, aplico el tokenizador
BigramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtm = DocumentTermMatrix(corpus,control=list(tokenize = BigramTokenizer))
word_matrix = as.matrix(dtm)

# Dibujo (Todo igual que antes)
palabras = melt(word_matrix)
palabras = palabras[palabras$value>0,]
frecuencias = aggregate(value~Terms,data=palabras,sum)
frecuencias_short = frecuencias[order(frecuencias$value,decreasing = TRUE)[1:40],]
ggplot(frecuencias_short, aes(x = reorder(Terms, value), y = value, fill = value))+
  geom_col() + coord_flip() + 
  ggtitle("Palabras Crudas") + 
  scale_fill_gradient(low = "gray", high = "darkgreen")
wordcloud(words = frecuencias$Terms, freq = frecuencias$value, 
          random.order = FALSE, colors = brewer.pal(8, "Accent"), min.freq = 1, max.words = 50)




# Vamos a elegir una parte, nada más
seleccion = frecuencias
seleccion$Terms = as.character(seleccion$Terms)
seleccion$aparece1 = str_detect(seleccion$Terms, "data ")
seleccion$aparece2 = str_detect(seleccion$Terms, " data")
seleccion = filter(seleccion, (aparece1 | aparece2) & value>1)
wordcloud(words = seleccion$Terms, freq = seleccion$value, 
          random.order = FALSE, colors = brewer.pal(8, "Accent"), min.freq = 1, max.words = 50)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Para casa (si te atreves)
# Repite el ejercicio de visualizar las frecuencias de palabras de
#   los resúmenes de las noticias de www.meneame.net y cuéntanos los
#   resultados por el foro
# Comparte los avances con tus compañeros por el foro
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

