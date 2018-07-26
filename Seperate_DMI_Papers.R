library(stringr)
library(pdftools)
library(dplyr)
library(SnowballC)
library(tm)
library(gtools)

DMI_Titles <- read.csv(file.choose(),header = F) # choose a CSV file with the DMI papers titles
 # if encoding issues
#DMI_Titles <- read.csv(file.choose(),header = F, fileEncoding = "UTF-8-BOM")
DMI_Titles <- as.character(DMI_Titles[,1]) 

# load
txt <- DMI_Titles
corpus <- Corpus(VectorSource(txt))

# clean
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english")) 
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- trimws(corpus[["content"]][["content"]])

clean_corpus <- strsplit(trimws(corpus)," ")
clean_corpus_counter <- 1
while (clean_corpus_counter < length(clean_corpus)+1){
  term <- clean_corpus[[clean_corpus_counter]]
  
  term <- term[term != "" & term != "pp"]
  term <- trimws(term)
  term <- paste0("^",term,"*")
  clean_corpus[[clean_corpus_counter]] <- term
  
  clean_corpus_counter <- clean_corpus_counter + 1
  term <- NULL
}


setwd("")                                                         #Path where all the papers are present
files <- list.files(pattern = "pdf$")
files<-mixedsort(files)

mainDir <- ""                                                     # enter path where all the papers are present
subDir <- "DMI Papers"                                            #name of the new folder where all the DMI papers would be present
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))
duplictae_files <- list.files(pattern = "pdf$")

if (length(duplictae_files) > 0){
  do.call(file.remove, list(list.files(file.path(mainDir, subDir), full.names = TRUE)))
}
setwd("")                                                         #Path where all the papers are present

file_counter <- 1

while (file_counter < length(files) + 1){
    title_counter <- 1
    #Cleaning the Text of PDF File
    text <- pdf_text(toString(files[file_counter]))               # reading the text from the pdf file
    clean_text <- strsplit(text, "\n")                            # cleaning each line of the pdf to make it more readable and sensible
    clean_text <- strsplit(unlist(clean_text), "\r")              # cleaning each line of the pdf to make it more readable and sensible
    clean_text <- unlist(strsplit(trimws(clean_text)," "))
    clean_text <- removePunctuation(clean_text)
    clean_text <- clean_text[clean_text != ""]
    clean_text <- tolower(clean_text)
   
  while (title_counter < length(clean_corpus)+1) {
    clean_corpus[[title_counter]]
    title_word_counter <- 1
    lol <- 1
    temp_counter <- 0
    while (lol < (length(clean_corpus[[title_counter]]) + 1)) {
      if ((sum(str_count(clean_text[1:50], clean_corpus[[title_counter]][title_word_counter]))) > 0){
        temp_counter <- temp_counter + 1
      }
      title_word_counter <- title_word_counter + 1
      lol <- lol + 1
    }
    if (temp_counter == length(clean_corpus[[title_counter]])) {
      file.copy(files[file_counter], file.path(mainDir, subDir))
      temp_counter <- 0
      break()
    }
    title_counter <- title_counter + 1
  }
  
  file_counter <- file_counter + 1
}


