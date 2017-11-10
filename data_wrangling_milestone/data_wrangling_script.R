# File-Name:      capstone_main_script.R
# Date:           2017-11-5
# Author:         Monisha Gopalakrishnan
# Purpose:        Contains steps taken to clean up the data
#
# Data Used:      train
# Packages Used:  dplyr, ggplot2, car, plyr


## Library/Source -------------------------------
library(dplyr)
library(ggplot2)
library(car)
library(plyr)

source("~/SpringboardCapstone/capstone_data_wrangling.R")
source("~/SpringboardCapstone/capstone_feature_enrichment.R")



## Load data ------------------------------------

train <- read.csv("/Users/monishagopal/Desktop/insurance/train.csv")
train <- tbl_df(train)
# Remove ID column
train <- train[-1]

# Look at structure of train data
str(train)
summary(train)
head(train)



## Data wrangling -------------------------------

# Replace blank spaces with NA values 
train <- remove_blank_spaces(train)

# Remove variables with more than 25% NA values
train_2 <- remove_high_na_cols(train, 0.25)

# Remove outliers in continuous variables and replace with NA
train_3 <- remove_outlier_values(train_2)

# Replace NAs with mean in continuous variables/mode in cat
train_4 <- replace_na_continuous(train_3)
train_5 <- replace_na_categorical(train_4)

# Remove features with zero or near-zero variance
train_6 <- remove_near_zero_variability(train_5)

# Remove factors with more than 20 categories
train_7 <- remove_lots_categories(train_6, 20)
  
# Remove features with perfect multicollinearity
# (So if any category in a feature is perfectly linearly dependent
# with any other feature, then it is removed)
model <- lm(target ~ ., data = train_7)
names(model$coefficients[is.na(model$coefficients)])  # Prints names of features

model2 <- lm(target ~ . - v107, data = train_7)
names(model2$coefficients[is.na(model2$coefficients)])
 
model3 <- lm(target ~ . - v107-v79, data = train_7)
names(model3$coefficients[is.na(model3$coefficients)])

model4 <- lm(target ~ . - v107-v79-v110, data = train_7)
names(model4$coefficients[is.na(model4$coefficients)])
summary(model4) # Check to make sure no coefficients are omitted

train_8 <- select(train_7, -v79, -v107, -v110)

# Step-wise VIF, eliminate largest VIF until max VIF <= 2
car::vif(lm(target ~ ., data = train_8))
car::vif(lm(target ~ . -v40, data = train_8))
car::vif(lm(target ~ . -v40-v75, data = train_8))
car::vif(lm(target ~ . -v40-v75-v34, data = train_8)) 
car::vif(lm(target ~ . -v40-v75-v34-v14, data = train_8)) 
car::vif(lm(target ~ . -v40-v75-v34-v14-v31, data = train_8)) 
train_9 <- select(train_8, -v40, -v75, -v34, -v14, -v31)



## Save cleaned data ----------------------------
write.csv(train_9, 'train_clean.csv')
