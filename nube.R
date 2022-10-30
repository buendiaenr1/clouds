

#nube de palabras

#install.packages("pacman")
library("pacman")
#install.packages("tm")
library("tm")
#install("wordcloud2")
library("wordcloud2")

p_load("tm") # Biblioteca para realizar el preprocesado del texto,
p_load("tidyverse") # Biblioteca con funciones para manipular datos.
p_load("wordcloud") # Biblioteca para graficar nuestra nube de palabras.
p_load("RColorBrewer") # Biblioteca para seleccionar una paleta de colores de nuestra nube de palabras.


texto <- read_file("ia_r_mx.txt")   # de tidyverse


texto <- VCorpus(VectorSource(texto),readerControl = list(reader = readPlain, language = "es"))

# Preprocesado de texto
texto <- tm_map(texto, tolower)
#espanol
##texto <- texto %>%
##            tm_map(removePunctuation) %>%
##            tm_map(removeNumbers) %>%
##            tm_map(removeWords, stopwords("spanish"))
##texto <- tm_map(texto, removeWords, c("puede", "ser", "pues", "si", "aún", "cómo"))
##texto <- tm_map(texto, stripWhitespace)

#ingles
texto <- texto %>%
            tm_map(removePunctuation) %>%
            tm_map(removeNumbers) %>%
            tm_map(removeWords, stopwords("english"))
#texto <- tm_map(texto, removeWords, c("puede", "ser", "pues", "si", "aún", "cómo"))
texto <- tm_map(texto, stripWhitespace)



# Construyendo la tabla de frecuencia

texto <- tm_map(texto, PlainTextDocument)
tabla_frecuencia <- DocumentTermMatrix(texto)

tabla_frecuencia <- cbind(palabras = tabla_frecuencia$dimnames$Terms,
                          frecuencia = tabla_frecuencia$v)

# Convertimos los valores enlazados con cbind a un objeto dataframe.
tabla_frecuencia<-as.data.frame(tabla_frecuencia)
# Forzamos a que la columna de frecuencia contenga valores numéricos.
tabla_frecuencia$frecuencia<-as.numeric(tabla_frecuencia$frecuencia)
# Ordenamos muestra tabla de frecuencias de acuerdo a sus valores numéricos.
tabla_frecuencia<-tabla_frecuencia[order(tabla_frecuencia$frecuencia, decreasing=TRUE),]

#grafica
wordcloud(words = tabla_frecuencia$palabras,
          freq = tabla_frecuencia$frecuencia,
          min.freq = 2,
          max.words = 200,
          random.order = FALSE,
          colors = brewer.pal(8,"Paired"))

#mostrar grafica en el navegador
#wordcloud2(data=tabla_frecuencia, size=1.6, color='random-dark')

# guardar grafica
png("nube.png", width = 800,height = 800, res = 100)
    wordcloud(words = tabla_frecuencia$palabras,
          freq = tabla_frecuencia$frecuencia,
          min.freq = 2,
          max.words = 200,
          random.order = FALSE,
          colors = brewer.pal(8,"Paired"))
dev.off()

# cerrar todo
p_unload(all)



