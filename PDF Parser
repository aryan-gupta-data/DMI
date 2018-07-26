start.time <- Sys.time()

#install.packages("pdftools")
#install.packages("stringr")
library(stringr)
library(pdftools)
library(dplyr)
library(SnowballC)
library(tm)

setwd("")  #Enter the path where there are all the PDF Files are

#############Loop for introducing a loop to make a list to store file names to use it later in output CSV Files

#files <- list.files(pattern = "pdf$")
files_list <- list()
temp_year <- 2013
end_year <- 2017
files_list_counter <- 1
while (temp_year < end_year + 1) {
  files_list[[files_list_counter]] <- list.files(pattern = toString(temp_year))
  temp_year <- temp_year + 1
  files_list_counter <- files_list_counter + 1
}

temp_vector <- c()
FileSensor_data <- list()

#################### Getting the possible names for text to search in the PDF files ###################

setwd("") #Enter the path where the Excel Files for the query text is
PossibleNames <- read.csv("Possible Names of Sensors.csv",header = F,fileEncoding = "UTF-8-BOM")
setwd("") #Enter the path where there are all the PDF Files are

Col1 <- as.character(PossibleNames$V1)[as.character(PossibleNames$V1) != ""]  
Col2 <- as.character(PossibleNames$V2)[as.character(PossibleNames$V2) != ""]
Col3 <- as.character(PossibleNames$V3)[as.character(PossibleNames$V3) != ""]
sensors <- c(Col1,Col2,Col3)
Col1_WithMissing <- as.character(PossibleNames$V1) 
Col2_WithMissing <- as.character(PossibleNames$V2)
Col3_WithMissing <- as.character(PossibleNames$V3)

sensors_stem_cnt <- 1
while (sensors_stem_cnt < (length(sensors) + 1)){
  sensors[sensors_stem_cnt] <- paste0("^",sensors[sensors_stem_cnt],"$")
  sensors_stem_cnt <- sensors_stem_cnt + 1
}

#################### Main While Loop Starts ###################

Num_OF_Years <- 1
temp_year <- 2013
while (Num_OF_Years < length(files_list) + 1) {
  
#################### Here first it only selects files from the first vector in Files List made above..... It has 5 Vectors having the names of files of years from 2013 - 2017
  
  iter <- 1
  counter <- 1
  while(iter < length(files_list[[Num_OF_Years]]) + 1){
    counter <- 1
    text <- pdf_text(toString(files_list[[Num_OF_Years]][iter])) # reading the text from the pdf file
    clean_text <- strsplit(text, "\n") # cleaning each line of the pdf to make it more readable and sensible
    clean_text <- strsplit(unlist(clean_text), "\r") # cleaning each line of the pdf to make it more readable and sensible
    clean_text <- unlist(strsplit(trimws(clean_text)," "))
    clean_text <- removePunctuation(clean_text)
    clean_text <- clean_text[clean_text != ""]
    clean_text <- tolower(clean_text)
    
    
      while(counter < length(sensors) + 1){ #this loop converts the number of sensors to 1 if they are more that 1 or 0
        if ((sum(str_count(clean_text, sensors[counter]))) > 0){
          temp_vector[counter] <- 1
          #temp_vector[counter] <- (sum(str_count(clean_text, sensors[counter])))
        } else {
          temp_vector[counter] <- 0
          #temp_vector[counter] <- (sum(str_count(clean_text, sensors[counter])))
        }
        #temp_vector[counter] <- (sum(str_count(stemDocument(tolower(clean_text)), sensors[counter])))
        counter <- counter + 1
      }
      
      FileSensor_data[[iter]] <- temp_vector # temp vector has data from one files of that year.....as 'iter' increases FileSensor_data stores all the values of different files form the same year
   
  
    temp_vector <- c()
    text <- NULL
    clean_text <- NULL
    iter = iter + 1
  }

  
  ###### Below is to output the above single year list into CSV for a single year
    
  setwd("") #PAth where you want to save the o/p results in CSV Format
  FileSensor_data_dataframe <- as.data.frame(FileSensor_data)
  FileSensor_data_dataframe <- t(FileSensor_data_dataframe)
  # entering the labels of Rows and Columns
  rownames(FileSensor_data_dataframe) <- files_list[[Num_OF_Years]]
  colnames(FileSensor_data_dataframe) <- sensors
  
  #combining the columns of sensors that mean the same eg. accelerometer = inclinometer OR imu = inertial motion unit =	position tracker

  
  File <- FileSensor_data_dataframe
  names(File) <- trimws(names(File)) #removing unwated whitespaces
  names(File) <- gsub(x = names(File), pattern = "\\.", replacement = " ")#did this coz R was replacing spaces with "." and have to replace "." into spaces
  names(File) <- trimws(names(File)) #removing unwated whitespaces
  
  File_cnt <- 1
  
  #the loop below adds the valus of sensors and its synonyms......if loops are used to exclude the blank names
  
  
  
  File <- as.data.frame(File) 
  #File[,45:ncol(File)] <- NULL # Did this coz i just want the data of first 44 sensors.....others are just synonymns or the first 44
  
  write.csv(colSums(File), file = paste("Analysis_DMI_",temp_year,"_Total.csv"))
  write.csv(File, file = paste("Analysis_DMI_",temp_year,".csv"))
  
  FileSensor_data <- NULL
  FileSensor_data_dataframe <- NULL
  File <- NULL
  SameCols <- NULL
  Num_OF_Years <- Num_OF_Years + 1
  temp_year <- temp_year + 1
  
  setwd("") #Enter the path where there are all the PDF Files are
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
