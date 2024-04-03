library(readxl)
library(tidyverse)
library(tm)

db <- read_excel("C:/Users/jabuitrago/OneDrive - International Organization for Migration - IOM/Min Minas/2 Febrero/SNIES.xlsx")

var <- c(3,10,15,24,25,26,27,35,36)
db <- db[var]


text <- db$'NOMBRE_DEL_PROGRAMA'
text <- Corpus(VectorSource(text))

text <- text %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

text <- tm_map(text, content_transformer(tolower))

db$text <- c(text$content)

text <- TermDocumentMatrix(text) 
text <- as.matrix(text) 
text <- sort(rowSums(text),decreasing=TRUE) 
text <- data.frame(text, text = names(text), freq=text)


db <- db%>%mutate(a1=str_detect(text, "ambient | electrica| telecomunica|
                                eléctrica|electrico|electircidad|eléctrico|
                                energ|mec|redes| 'servicios p'"))


db <- db%>%filter(a1=="TRUE" )

& RECONOCIMIENTO_DEL_MINISTERIO=="Acreditación de alta calidad")

write.csv(db, "C:/Users/jabuitrago/OneDrive - International Organization for Migration - IOM/Min Minas/2 Febrero/SNIES_todos.csv" 
          , row.names=FALSE)
