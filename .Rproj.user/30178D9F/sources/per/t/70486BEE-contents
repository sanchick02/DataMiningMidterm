# Required libraries
libraries <- c("dplyr", "ggplot2", "tidyr", "readr", "ggpubr", "caret", "MLmetrics", "ggcorrplot", "class", "caTools")

# Check for and install missing packages
for (lib in libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE) 
  }
}

library(dplyr)
library(caTools)
library(ggplot2)
library(tidyr)
library(readr)
library(ggpubr)
library(ggcorrplot)

# Import dataset 
data <- read.csv("healthcare-dataset-stroke-data.csv")

# Structure and summary
str(data)
summary(data)
head(data)

# Check for missing values
sum(is.na(data)) 

# Missing values in numerical columns
missing_nums <- colMeans(is.na(data)) > 0 
print(missing_nums)

# Missing values in categorical columns
non_numeric_cols <- sapply(data, function(x) !is.numeric(x)) 
missing_cats <- colMeans(is.na(data[, non_numeric_cols])) > 0 
print(missing_cats)

# Imputation (example with bmi, adjust strategy as needed)
data$bmi[is.na(data$bmi)] <- mean(data$bmi, na.rm = TRUE)

# Removing rows with remaining missing data if appropriate
data <- na.omit(data)

outlier_vars <- c('age', 'hypertension', 'heart_disease', 'avg_glucose_level', 'bmi', 'stroke')

for (var in outlier_vars) {
  ggplot(data, aes_string(x = var)) +
    geom_boxplot(fill = "lightblue", color = "blue") +
    labs(title = paste("Boxplot of", var), x = var, y = "Value") +
    theme_minimal() -> plot_obj
  
  # Save plot if desired
  ggsave(filename = paste("images/", var, ".png", sep = ""), plot = plot_obj) 
}

# Filtering (example with BMI, adjust thresholds)
data <- data %>% filter(bmi >= 16 & bmi < 44)

# Convert categorical variables to numeric
data$gender <- as.integer(factor(data$gender, levels = c("Male", "Female", "Other")))
data$smoking_status <- ifelse(data$smoking_status == "never smoked", 0, 1)

# Drop unnecessary columns
data <- data %>% select(-c(ever_married, work_type, Residence_type,id))

# Save clean dataset 
write.csv(data, "preprocessed_dataset.csv", row.names = FALSE)

preprocessed_dataset <- read.csv("preprocessed_dataset.csv") 
summary(preprocessed_dataset)

# Boxplots for preprocessed data
boxplot(preprocessed_dataset$gender)
boxplot(preprocessed_dataset$age)
boxplot(preprocessed_dataset$hypertension)
boxplot(preprocessed_dataset$heart_disease)          
boxplot(preprocessed_dataset$avg_glucose_level)
boxplot(preprocessed_dataset$bmi)
boxplot(preprocessed_dataset$smoking_status)
boxplot(preprocessed_dataset$stroke)


# Distributions
barplot(table(preprocessed_dataset$stroke), 
        main = 'Stroke Distribution', 
        xlab = 'Stroke (0: Absence, 1: Presence)', 
        ylab = 'Frequency', col = rainbow(2))

hist(preprocessed_dataset$age, main = 'Age Distribution', 
     xlab = 'Age', ylab = 'Frequency', col = 'purple')

# Relationships
plot(preprocessed_dataset$bmi, preprocessed_dataset$stroke, 
     main = 'BMI VS Stroke', xlab = 'BMI', ylab = 'Stroke')

cor(preprocessed_dataset$bmi, preprocessed_dataset$stroke)

ggcorrplot(cor(preprocessed_dataset), lab = TRUE)

# Set seed for reproducibility
set.seed(123)

# Data split 
sample_split <- sample.split(preprocessed_dataset$stroke, SplitRatio = 0.8)
train_data<- subset(preprocessed_dataset, sample_split == TRUE)
test_data<- subset(preprocessed_dataset, sample_split == FALSE)

# KNN
library(class)
knn_model <- knn(train = train_data[,-8], test = test_data[,-8], cl = train_data$stroke, k = 5)

# Confusion Matrix
library(caret)
confusionMatrix(knn_model, as.factor(test_data$stroke))

# Performance metrics
library(MLmetrics)
Precision(test_data$stroke, knn_model)
Recall(test_data$stroke, knn_model)
F1_Score(test_data$stroke, knn_model)








