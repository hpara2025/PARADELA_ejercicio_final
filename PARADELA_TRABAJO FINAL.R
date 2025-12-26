# SCRIPT TRABAJO FINAL PARADELA
getwd()
setwd("~/Personal/Maestria HUMANIDADES DIGITALES/Actualización y técnica herramientas digitales/Trabajo final PARADELA")
library("tidytext", "tidyverse")
library(dplyr)
library(ggplot2)
list.files()
list.files(, pattern='txt') 
archivos <- list.files(pattern = ".txt$", full.names = TRUE)
archivos
class(archivos) 
length(archivos) 
textos_archivo <- gsub(".txt", "", archivos, perl = TRUE)
list.files(, pattern='txt') 
mensajes <- tibble(textos_archivo = character(),
                   +                    parrafo = numeric(),
                   +                    texto = character()) 
mensajes
for (i in 1:length(archivos)){
  textos_iteracion <- read_lines(paste(archivos[i],
                                       sep = "/"))
  temporal <- tibble(textos_archivo = textos_archivo[i],
                     parrafo = seq_along(textos_iteracion),
                     texto = textos_iteracion)
  mensajes <- bind_rows(mensajes, temporal)
} 
mensajes
textos_palabras <- mensajes %>% 
  unnest_tokens(palabra, texto)
textos_palabras
install.packages("wordcloud")
library("wordcloud")
install.packages("wordcloud2")
library("wordcloud2")
textos_palabras %>%
  count(palabra, sort = T) %>%
  wordcloud2(data = ., size = 0.85, 
             #maxWords = 300,
             shape = 'circle',
             color = 'random-dark',
             #ellipticity = 0.6,
             backgroundColor = "white", 
             minRotation = 0,
             maxRotation = 0)
install.packages ("stopwords")
stopwords1 <- get_stopwords("es")
textos_limpio <- textos_palabras %>% 
  anti_join(stopwords1, by = c("palabra" = "word"))
textos_limpio
textos_limpio %>% 
  count(palabra, sort = T) %>%
  wordcloud2(data = ., size = 3, 
  #maxWords = 300,
  shape = 'circle',
  color = 'random-dark',
  #ellipticity = 0.6,
  backgroundColor = "white", 
  minRotation = 0,
  maxRotation = 0)

textos_limpio %>% 
  count(palabra, sort = TRUE) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(palabra, n), y = n)) +
  geom_col(fill = "Green") +   
  coord_flip() +
  labs(
  title = "Gráfico de frecuencias",
    x = "Palabra",
    y = "Frecuencia") 
  theme_minimal() +
    theme(
    plot.title = element_text(size = 16, face = "bold", color = "Black"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12))

