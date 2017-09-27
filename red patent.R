setwd("D:/Stanford CNN course/text mining in R/red 5//")

#loading package
load <- c("NLP","tm","lsa","XML")
lapply(load, library, character.only = TRUE)
library(tidyverse)

#inilization


#??FPO??ȡר??˵?????ĺ???
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

p_corpus <- VCorpus(DirSource("D:/Stanford CNN course/text mining in R/RED/"), readerControl = list(language="lat")) 
p_tdm <- p_corpus %>% clean_corpus() %>% 
  TermDocumentMatrix() 

mostterm <- findMostFreqTerms(p_tdm,10)

#termindex <- findFreqTerms(p_tdm, 100) 

#p_tdm <- as.matrix(p_tdm[termindex,])


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
simdis <- docsim["8174560.txt",]
simdis <- sort(simdis, decreasing = TRUE)
patent_names <- names(simdis)
simdis_df <-  data.frame(simdis)
simdis_df <- cbind(simdis_df, patent_names)
simdis_df <- subset(simdis_df, simdis_df$simdis>=0.01)
simdis_df <- simdis_df[order(simdis_df$simdis),]
simdis_df$patent_names <- factor(simdis_df$patent_names, levels = simdis_df$patent_names)

data("mtcars")  # load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.

ggplot(simdis_df, aes(x=patent_names, y=simdis, label=simdis)) + 
  geom_bar(stat='identity', width=.5, fill="#00ba38")  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip()

ggplot(simdis_df, aes(x=patent_names, y=round(simdis, 2), label=round(simdis,2))) + 
  geom_point(stat='identity',size=6)  +
  scale_color_manual(name="Mileage", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="Diverging Dot Plot", 
       subtitle="Normalized mileage from 'mtcars': Dotplot") + 
  ylim(-2.5, 2.5) +
  coord_flip()


ggplot(simdis_df, aes(x=patent_names, y=simdis, label=round(simdis, 2))) + 
  geom_point(stat='identity', aes(color="#00ba38"), size=6)  +
  geom_segment(aes(y = 0, 
                   x = patent_names, 
                   yend = simdis, 
                   xend = patent_names), 
               color = "black") +
  geom_text(color="white", size=2) +
  labs(title="Diverging Lollipop Chart", 
       subtitle="Normalized mileage from 'mtcars': Lollipop") + 
  ylim(-2.5, 2.5) +
  coord_flip()

ggplot(simdis_df, aes(x=patent_names, y=simdis, label=round(simdis, 2))) + 
  geom_point(stat='identity', size=6)  +
  scale_color_manual(name="Mileage", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="Diverging Dot Plot", 
       subtitle="Normalized mileage from 'mtcars': Dotplot") + 
  ylim(-2.5, 2.5) +
  coord_flip()
