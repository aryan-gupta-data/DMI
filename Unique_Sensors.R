#Code used to find uniques values in a column of a CSV file
setwd("") #Path with the sensor data
data <- read.csv(file.choose()) #Read the CSV
Ajin_Data <- data[1:486,] 
Mark_Data <- data[487:nrow(data),]
#####################For Ajin's Data###########################
sensors <- as.matrix(Ajin_Data["Sensors"])[as.matrix(Ajin_Data["Sensors"]) != ""] #Taking just the Sensor Column
sensors <- strsplit(sensors, ",") #String Splitting w.r.t comma
sensors <- unlist(sensors) #Converting List to a Chr Vector
sensors <- sensors[sensors != " " & sensors != "NULL" & sensors != ""] #Removing Blanks and NULL Values
sensors <- tolower(sensors) #Converting the sensor data in LCase
sensors <- trimws(sensors) #Removing the Unwated Whitespace
#sensors <- gsub("\\s*\\([^\\)]+\\)","",as.character(sensors)) #Removing the values within parenthesis
unique_sensors <- unique(sensors) #Getting the Unique Values
write.csv(unique_sensors, file = paste("UniqueSensor_AjinData_WithParenthesis.csv"))
#####################For Mark's Data###########################
sensors <- as.matrix(Mark_Data["Sensors"])[as.matrix(Mark_Data["Sensors"]) != ""] #Taking just the Sensor Column
sensors <- strsplit(sensors, ",") #String Splitting w.r.t comma
sensors <- unlist(sensors) #Converting List to a Chr Vector
sensors <- sensors[sensors != " " & sensors != "NULL"] #Removing Blank and NULL Values
sensors <- tolower(sensors) #Converting the sensor data in LCase
sensors <- trimws(sensors) #Remving the Unwated Whitespace
#sensors <- gsub("\\s*\\([^\\)]+\\)","",as.character(sensors)) #Removing the values within parenthesis
unique_sensors <- unique(sensors) #Getting the Unique Values
write.csv(unique_sensors, file = paste("UniqueSensor_MarkData_WithParenthesis.csv"))
