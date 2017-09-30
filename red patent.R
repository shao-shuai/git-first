setwd("D:/Stanford CNN course/text mining in R/synergy/")

#loading package
load <- c("NLP","tm","lsa","XML", "ggthemes")
lapply(load, library, character.only = TRUE)
library(tidyverse)

#inilization


#new comment
extractPatent <- function(patent_number){
  if (nchar(patent_number) == 11){
    baseurl <- "http://www.freepatentsonline.com/"
    tailurl <- ".html"
    patentID <- patent_number
    patentID <- strsplit(as.character(patentID),"")[[1]]
    patentID <- append(patentID, "y", after = 0)
    patentID <- append(patentID, "/", after = 5)
    patentID <- paste(patentID, collapse = "")
    patentURL <- paste(baseurl, patentID, tailurl, sep = "")
    myfile <- htmlTreeParse(patentURL, useInternal = TRUE)
    myfile.text = unlist(xpathApply(myfile, '//p', xmlValue))
    myfile.text <- gsub('\\n',' ', myfile.text)
    myfile.text <- paste(myfile.text, collapse = ' ')
    filename <- paste(as.character(patent_number), ".txt", sep = "")
    file <- file(filename)
    writeLines(myfile.text, file)
    close(file)
  } else {
    baseurl <- "http://www.freepatentsonline.com/"
    tailurl <- ".html"
    patentID <- patent_number
    patentID <- paste(strsplit(as.character(patentID),"")[[1]], collapse = "")
    patentURL <- paste(baseurl, patentID, tailurl, sep = "")
    myfile <- htmlTreeParse(patentURL, useInternal = TRUE)
    myfile.text = unlist(xpathApply(myfile, '//p', xmlValue))
    myfile.text <- gsub('\\n',' ', myfile.text)
    myfile.text <- paste(myfile.text, collapse = ' ')
    filename <- paste(patentID, ".txt", sep = "")
    file <- file(filename)
    writeLines(myfile.text, file)
    close(file)
  }
}

#??ȡexcel?ļ???package
library(readxl)
patent <- read_excel("red reexamination.xlsx", col_names = TRUE, na = "NA")
patent <- as.data.frame(patent)
patentNO <- patent[,1]

#this statement can be changed as a apply funciotn
#mapply(extracPatetn,  patentNO) works as well
map(patentNO,extractPatent)



#function to transform any charactor into a space
toSpace <- content_transformer(function(x, pattern){return (gsub(pattern, " ", x))})

#remove everything that is not alphanumerical symbol
#removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
removeSpecialChars <- content_transformer(function(x){return (gsub("[^a-zA-Z0-9 ]","",x))})

#function to clean corpus
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, toSpace, "\\[")
  corpus <- tm_map(corpus, toSpace, "\\]")
  corpus <- tm_map(corpus, toSpace, "\\(")
  corpus <- tm_map(corpus, toSpace, "\\)")
  corpus <- tm_map(corpus, toSpace, "\\,")
  corpus <- tm_map(corpus, toSpace, "\\.")
  corpus <- tm_map(corpus, toSpace, "\\;")
  corpus <- tm_map(corpus, toSpace, "\\-")
  corpus <- tm_map(corpus, toSpace, "\"")
  corpus <- tm_map(corpus, toSpace, "\\'")
  corpus <- tm_map(corpus, removeSpecialChars)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  corpus <- tm_map(corpus, removeWords, c("can","may","embodiments","embodiment","system","vehicle",
                                          "system","include","example","information","data","storage",
                                          "based","device","processing","unit"))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

p_corpus <- VCorpus(DirSource("D:/Stanford CNN course/text mining in R/synergy/"), readerControl = list(language="lat")) 
p_tdm <- p_corpus %>% clean_corpus() %>% 
  TermDocumentMatrix() %>% 
  as.matrix()

#calculate local weighting
LW <- log(1 + p_tdm)

#calculate global weighting
ptdm <- p_tdm/rowSums(p_tdm)
GW <- 1 - entropy(ptdm)/log(ncol(p_tdm))

#calculate entroy weighting 
X <- GW*LW

#apply las
mylsa <- lsa(X)

#calculate term vector
termVectors <- mylsa$tk*mylsa$sk

#calculate document vector
docVectors <- mylsa$dk*mylsa$sk

#calculate cosine similarity of document vector
docsim <- cosine(t(docVectors))

#visualization
simdis <- docsim["8200375.txt",]
simdis <- sort(simdis, decreasing = TRUE)
patent_names <- names(simdis)
simdis_df <-  data.frame(simdis)
simdis_df <- cbind(simdis_df, patent_names)
simdis_df <- subset(simdis_df, simdis_df$simdis>=0.01)
simdis_df <- simdis_df[order(simdis_df$simdis),]
simdis_df$patent_names <- factor(simdis_df$patent_names, levels = simdis_df$patent_names)

ggplot(simdis_df, aes(x=patent_names, y=simdis, label=simdis)) + 
  geom_bar(stat='identity', width=.5, fill="#00ba38")  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(title= "Similarity of US8174560 and its references") + 
  xlab("Publication Number")+
  ylab("Similariy")+
theme(axis.text.y = element_text(size = 10,face = "bold"),
        axis.text.x = element_text(size = 10,face = "bold"))+
  coord_flip()
