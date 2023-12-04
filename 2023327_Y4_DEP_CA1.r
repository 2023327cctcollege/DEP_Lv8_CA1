# DEP_Lv8_CA1
# 
# CCT College Dublin  
# Bachelor of Science Honours in Computing in Information Technology  
# Data Exploration & Preparation - Y4M3  
# Year 4, Semester 7  
# Continuous Assessment 1
#   
# Lecturer name: Dr. Muhammad Iqbal  
# Lecturer email: miqbal@cct.ie
# 
# Student Name: Mateus Fonseca Campos  
# Student Number: 2023327  
# Student Email: 2023327@student.cct.ie
# 
# Submission date: 3 December 2023
# 
# GitHub: https://github.com/2023327cctcollege/DEP_Lv8_CA1
# ___


# installing all the necessary packages at once
packages <- c('tidyr', 'dplyr', 'skimr', 'ggplot2', 'devtools')
for (p in packages) {
  if (!(p %in% rownames(installed.packages()))) {
    install.packages(p, character.only = TRUE)
  }
  library(p, character.only = TRUE)
}

# installing ggbiplot from GitHub repository
# docs suggests that ggbiplot be loaded before dplyr
# unload dplyr, load ggbiplot, then load dplyr again
install_github("vqv/ggbiplot")
unload('dplyr')
library(ggbiplot)
library(dplyr)

# read dataset from CSV file
# available at https://data.cdc.gov/Vaccinations/COVID-19-Vaccination-Trends-in-the-United-States-N/rh2h-3yt2
# accessed on 3 December 2023
df <- read.csv('COVID-19_Vaccination_Trends_in_the_United_States_National_and_Jurisdictional.csv', stringsAsFactors = TRUE)


# 1. Data Preparation

# 1.1. Variable classification

# quick summary of variables in the dataset
# categorical, discrete or continuous
var_class <- function(df) {
  factors <- df %>% select_if(is.factor)
  numerics <- df %>% select_if(is.numeric)
  discretes <- numerics %>% select_if(function(x) all(x %% 1 == 0 | is.na(x)))
  continuous <- numerics %>% select_if(Negate(function(x) all(x %% 1 == 0 | is.na(x))))
  
  cat(sprintf('Categorical [%d]:\n', ncol(factors)))
  for (col in colnames(factors)) {
    cat(sprintf('\t%s\n', col))
  }
  
  cat(sprintf('\nDiscrete [%d]:\n', ncol(discretes)))
  for (col in colnames(discretes)) {
    cat(sprintf('\t%s\n', col))
  }
  
  cat(sprintf('\nContinuous [%d]:\n', ncol(continuous)))
  for (col in colnames(continuous)) {
    cat(sprintf('\t%s\n', col))
  }
}

# write var_class to file before change
sink('./out/fig_2.txt')
var_class(df)
sink()

# make MMWR_week a factor
# variable is numeric however it labels the week number
# to be used for categorization rather than calculation
df$MMWR_week <- as.factor(df$MMWR_week)

# write var_class to file after change
sink('./out/fig_3.txt')
var_class(df)
sink()

# write skim to file before change
sink('./out/fig_4-6.txt')
skim(df)
sink()

# replace negative values with NA
# drop rows that contain NA
df[df < 0] <- NA
df <- drop_na(df)

# write skim to file after change
sink('./out/fig_7-9.txt')
skim(df)
sink()

# keep only desired columns, drop the rest
df <- df[, c(1:6, 8, 9, 11, 14, 15, 17:19, 21)]


# 1.2. Feature scaling

# min-max normalization function
MMnorm <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# z-score standardization function
zSd <- function(x) {
  return((x - mean(x)) / (sd(x)))
}

# robust scaler scaling function
robSc <- function(x) {
  return((x - median(x)) / (quantile(x, 0.75) - quantile(x, 0.25)))
}

# normalized/standardized/scaled versions of dataframe
df_MMnorm <- df
df_zSd <- df
df_robSc <- df

# apply scaling functions respectively
df_MMnorm[, 5:15] <- apply(df[, 5:15], 2, MMnorm)
df_zSd[, 5:15] <- apply(df[, 5:15], 2, zSd)
df_robSc[, 5:15] <- apply(df[, 5:15], 2, robSc)


# 2. Exploratory Data Analysis (EDA)

# Question 1: "Are people who took the 1st dose more likely to complete the series?"
# Question 2: "Are people who completed the series more likely to take the booster?"

# 2.1. Feature correlation

# Q1
# linear regression model
# correlation factor in summary
model_q1 <- lm(Series_Complete_Cumulative ~ Admin_Dose_1_Cumulative, df_robSc)

# Q2
# linear regression model
# correlation factor in summary
model_q2 <- lm(Booster_Cumulative ~ Series_Complete_Cumulative, df_robSc)


# 2.2. Data exploration

# Q1
# line + scatter plot with linear regression
ggplot(data = df_robSc, aes(x = Admin_Dose_1_Cumulative, y = Series_Complete_Cumulative)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = 'lm')

# Q2
# line + scatter plot with linear regression
ggplot(data = df_robSc, aes(x = Series_Complete_Cumulative, y = Booster_Cumulative)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = 'lm')


# 3. Principal Component Analysis (PCA)

# 3.1. Dummy encoding

# add dummy encoding flag for date_type
df_robSc <- transform(df, admin_flag=ifelse(date_type == 'Admin', 1, 0))


# 3.2. Component profile

# plotting a barplot of all components sorted by variance
pca <- princomp(df_robSc[, c(5:15)])
summary(pca)
pca$var$exp <- pca$sdev^2 / sum(pca$sdev^2)
barplot(pca$var$exp)


# 3.3. Dimensionality reduction

# plotting a biplot of PC1 against PC2 (the two main components)
pca <- prcomp(df_robSc[, c(5:15)], center = TRUE, scale. = FALSE)
summary(pca)
ggbiplot(pca)
