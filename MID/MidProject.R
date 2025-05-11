library("dplyr")
mydata<-read.csv("D:/dsdata.csv",header = TRUE,sep=",")
print(mydata)

numberOfCol <- ncol(mydata)
numberOfRow <- nrow(mydata)
cat("Number of Column: ", numberOfCol, "\n")
cat("Number of Row: ", numberOfRow, "\n")
str(mydata)

mydata <- distinct(mydata) 
mydata
numberOfCol <- ncol(mydata)
numberOfRow <- nrow(mydata)
cat("Number of Column: ", numberOfCol, "\n")
cat("Number of Row: ", numberOfRow, "\n")


handleInvaData <- mydata
handleInvaData$gender <- ifelse(
  substr(handleInvaData$gender, 1, 4) == "Male", "MALE",
  ifelse(
    substr(handleInvaData$gender, 1, 6) == "Female", "FEMALE",
    NA
  )
)
handleInvaData$gender
unique(handleInvaData$gender)

handleInvaData$bmi <- ifelse(handleInvaData$bmi < 0, NA, handleInvaData$bmi)
unique(handleInvaData$bmi)

handleInvaData$age <- ifelse(handleInvaData$age < 0, NA, handleInvaData$age)
unique(handleInvaData$age)

handleInvaData$blood_glucose_level <- as.numeric(handleInvaData$blood_glucose_level)
unique(handleInvaData$blood_glucose_level)



annoData <- handleInvaData
annoData$gender <- factor(annoData$gender, levels = c("FEMALE", "MALE"), labels = c(0, 1)) 
annoData$gender

annoData$smoking_history <- factor(annoData$smoking_history,levels = c("never","ever","No Info","current","not current","former"),labels = c(1,2,3,4,5,6))
annoData$smoking_history

annoData

summary(annoData)



numOfMissValue <- colSums(is.na(annoData)) 
numOfMissValue


barplot(numOfMissValue, 
        main = "Missing Value Per Column", 
        xlab = "Column", 
        ylab = "Number of Missing Values", 
        las = 2,  
        col = "lightblue")  




newData1 <- na.omit(annoData) 
numOfMissValue <- colSums(is.na(newData1)) 
numOfMissValue 


newData2 <- annoData 
newData2$gender <- as.numeric(newData2$gender)
newData2$smoking_history <- as.numeric(newData2$smoking_history)
for (col_name in names(newData2)) { 
  if (is.numeric(newData2[[col_name]])) {
    column_mean <- mean(newData2[[col_name]], na.rm = TRUE)
    newData2[[col_name]][is.na(newData2[[col_name]])] <- column_mean
    newData2[[col_name]] <- round(newData2[[col_name]], digits = 0) 
  } 
}
numOfMissValue <- colSums(is.na(newData2))
numOfMissValue
newData2

newData3 <- annoData 
newData3$gender <- as.numeric(newData3$gender)
newData3$smoking_history <- as.numeric(newData3$smoking_history)
for (col_name in names(newData3)) { 
  if (is.numeric(newData3[[col_name]])) {
    column_median <- median(newData3[[col_name]], na.rm = TRUE)
    newData3[[col_name]][is.na(newData3[[col_name]])] <- column_median
    newData3[[col_name]] <- round(newData3[[col_name]], digits = 0) 
  } 
}
numOfMissValue <- colSums(is.na(newData3))
numOfMissValue
newData3


get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


newData4 <- annoData
newData4$gender <- as.numeric(newData4$gender)
newData4$smoking_history <- as.numeric(newData4$smoking_history)

for (col_name in names(newData4)) { 
  if (is.numeric(newData4[[col_name]])) {
    column_mode <- get_mode(newData4[[col_name]])  
    newData4[[col_name]][is.na(newData4[[col_name]])] <- column_mode
    newData4[[col_name]] <- round(newData4[[col_name]], digits = 0)
  } 
}

numOfMissValue <- colSums(is.na(newData4))
numOfMissValue
newData4


age_summary <- annoData %>%
  group_by(gender) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    mode_age = get_mode(age)
  )


print(age_summary)


hypertension_summary <- annoData %>%
  group_by(hypertension) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    mode_age = get_mode(age)  
  )


print(hypertension_summary)


gender_spread_summary <- annoData %>%
  group_by(gender) %>%
  summarise(
    age_range = diff(range(age, na.rm = TRUE)), 
    age_IQR = IQR(age, na.rm = TRUE),  
    age_variance = var(age, na.rm = TRUE),  
    age_sd = sd(age, na.rm = TRUE)  
  )


print(gender_spread_summary)


find_outliers_iqr <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)  
  Q3 <- quantile(column, 0.75, na.rm = TRUE)  
  IQR <- Q3 - Q1  # Interquartile range
  lower_bound <- Q1 - 1.5 * IQR 
  upper_bound <- Q3 + 1.5 * IQR  
  outliers <- column[column < lower_bound | column > upper_bound]  
  return(outliers)
}


outliers <- find_outliers_iqr(annoData$age)
cat("Outliers are: ", outliers, "\n")


ageMean <- mean(annoData$age, na.rm = TRUE)
annoData$age[annoData$age %in% outliers] <- ageMean
cat("Outliers Mean is: ", ageMean, "\n")


ageMedian <- median(annoData$age, na.rm = TRUE)
annoData$age[annoData$age %in% outliers] <- ageMedian
cat("Outliers Median is: ", ageMedian, "\n")


ageMode <- as.numeric(names(sort(table(annoData$age[!is.na(annoData$age)]), decreasing = TRUE)[1]))
annoData$age[annoData$age %in% outliers] <- ageMode
cat("Outliers Mode is: ", ageMode, "\n")




min_max_normalization <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

annoData$blood_glucose_level <- as.numeric(annoData$blood_glucose_level)

normalized_blood_glucose_level <- min_max_normalization(annoData$blood_glucose_level)

print(normalized_blood_glucose_level)

annoData$normalized_blood_glucose_level <- normalized_blood_glucose_level


filtered_data <- filter(annoData, age > 30)
filtered_data <- filter(annoData, age > 30, gender == "MALE")
filtered_data <- filter(annoData, gender %in% c("MALE", "FEMALE"))
filtered_data <- filter(annoData, !is.na(age))
filtered_data <- filter(annoData, blood_glucose_level >= 100 & blood_glucose_level <= 200)
filtered_data <- filter(annoData, between(blood_glucose_level, 100, 200))
head(filtered_data)



annoData<- na.omit(annoData) 
annoData
write.csv(annoData, "D:/Group 3.csv")







