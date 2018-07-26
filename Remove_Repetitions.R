setwd("") # Path where your where your PDF Parser.R output files are saved 

PossibleNames <- read.csv(file.choose(),header = F,fileEncoding = "UTF-8-BOM") #Select files which has the names of originals and possible duplicates
Col1 <- as.character(PossibleNames$V1)[as.character(PossibleNames$V1) != ""]  
Col2 <- as.character(PossibleNames$V2)[as.character(PossibleNames$V2) != ""]
Col3 <- as.character(PossibleNames$V3)[as.character(PossibleNames$V3) != ""]
sensors <- c(Col1,Col2,Col3)
Col1_WithMissing <- as.character(PossibleNames$V1) 
Col2_WithMissing <- as.character(PossibleNames$V2)
Col3_WithMissing <- as.character(PossibleNames$V3)

temp_year <- 2013
last_year <- 2017
while (temp_year < (last_year + 1)) {
  FileName <- paste0("ReAnalysis_Test_DMI_ ",temp_year, " .csv") #file name of the output files from -> PDF Parser.R
  File <- read.csv(FileName)
  #temp_file <- File
  File[,Col1_WithMissing[1]][1]
  File[,Col2_WithMissing[1]]
  names(File) <- trimws(names(File)) #removing unwated whitespaces
  names(File) <- gsub(x = names(File), pattern = "\\.", replacement = " ")#did this coz R was replacing spaces with "." and have to replace "." into spaces
  names(File) <- trimws(names(File)) #removing unwated whitespaces
  File_cnt <- 1
  while (File_cnt < length(File[,Col1_WithMissing[1]]) + 1 ) {
    Sensor_Counter <- 1
    SameCols <- list()
    SameCols_cnt <- 1
    cnt <- 1
    while (cnt < length(Col1) + 1){
      
      if (Col1_WithMissing[Sensor_Counter] != ""){
        SameCols[[SameCols_cnt]] <- File[,Col1_WithMissing[Sensor_Counter]][File_cnt]
      }
      
      if (Col2_WithMissing[Sensor_Counter] != ""){
        if (SameCols[[SameCols_cnt]] == 0) {
          SameCols[[SameCols_cnt]] <- SameCols[[SameCols_cnt]] + File[,Col2_WithMissing[Sensor_Counter]][File_cnt]
        }
      }
      
      if (Col3_WithMissing[Sensor_Counter] != ""){
        if (SameCols[[SameCols_cnt]] == 0) {
          SameCols[[SameCols_cnt]] <- SameCols[[SameCols_cnt]] + File[,Col3_WithMissing[Sensor_Counter]][File_cnt]
        }
      }
      File[,Col1_WithMissing[Sensor_Counter]][File_cnt] <- SameCols[[SameCols_cnt]] # have done this coz the total number of times sensors were used shoud come under the main 
      SameCols_cnt <- SameCols_cnt + 1
      Sensor_Counter <- Sensor_Counter + 1
      cnt <- cnt + 1
    }
    File_cnt <- File_cnt + 1
  }
  
  #dataframe_file <- as.data.frame(File[1:45])
  #dataframe_temp_file <- as.data.frame(temp_file)
  
  
  write.csv(colSums(File[2:45]), file = paste("ReAnalysis_Test_2_DMI_",temp_year,"_Total.csv"))
  write.csv((as.data.frame(File[1:45])), file = paste("ReAnalysis_Test_2_DMI_",temp_year,".csv"))
  
  temp_year <- temp_year + 1 
}
