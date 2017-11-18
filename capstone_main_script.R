# File-Name:      capstone_main_script.R
# Date:           2017-11-8
# Author:         Monisha Gopalakrishnan
# Purpose:        Contains script to clean data and then use machine learning models
#                 on it
#
# Data Used:      train
# Packages Used:  dplyr, ggplot2, car, plyr


## Library/Source -------------------------------
library(dplyr)
library(ggplot2)
library(car)
library(plyr)

source("/Users/monishagopal/Desktop/SpringboardCapstone/capstone_data_wrangling.R")
source("/Users/monishagopal/Desktop/SpringboardCapstone/capstone_feature_enrichment.R")



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



## Data split -----------------------------------
set.seed(1234)
trainLoc <- createDataPartition(train_9$target, p = 0.5, list = FALSE, times=1)

trainSplit <- train_9[trainLoc,]
testSplit <- train_9[-trainLoc,]



## Feature enrichment on Training set -----------

trainSplit$target <- as.numeric(trainSplit$target) # Change this back when done

# WOE binning
# v10
summary(trainSplit$v10)

ggplot(trainSplit, aes(x = v10, fill = factor(target))) + 
  geom_histogram(color = "white", 
                 aes(y=..count../sum(..count..)), bins = 20) +
  labs(y = "Proportion Observations", title = "v10 Info - 20 bins")

bins10_1 <- seq(from = min(trainSplit$v10), to = max(trainSplit$v10), length.out = 21)
woe10_1 <- calculate_woe(trainSplit$target, trainSplit$v10, bins10_1)
generate_woe_plot_con(woe10_1, "v10 WOE1 - 20 bins")
iv10_1 <- sum(woe10_1$iv, na.rm=TRUE)
iv10_1

# Combine bins 1-2-3, 4-5, 6, 7, 8, 9-10, 11-13-18, 12-15-17-14-19-20-16 
bins10_2 <- list(c(1, 2, 3), c(4, 5), 6, c(7, 8), c(9, 10), c(11, 12, 13, 14),
                 c(15, 16, 17, 19, 20))
woe10_2 <- combine_bins_con(woe10_1, bins10_2)
generate_woe_plot_cat(woe10_2, "v10 WOE2 - from 20 bins")
iv10_2 <- sum(woe10_2$iv, na.rm=TRUE)
iv10_2

ggplot(trainSplit, aes(x = v10, fill = factor(target))) + 
  geom_histogram(color = "white", 
                 aes(y=..count../sum(..count..)), bins = 10) +
  labs(y = "Proportion Observations", title = "v10 Info - 10 bins")

bins10_3 <- seq(from = min(trainSplit$v10), to = max(trainSplit$v10), length.out = 11)
woe10_3 <- calculate_woe(trainSplit$target, trainSplit$v10, bins10_3)
generate_woe_plot_con(woe10_3, "v10 WOE3 - 10 bins")
iv10_3 <- sum(woe10_3$iv, na.rm=TRUE)
iv10_3

# v10: Combine bin 1-2, 3, 4, 5, 6-7, 8-9-10
bins10_4 <- list(c(1, 2), 3, 4, 5, c(6, 7), c(8, 9, 10))
woe10_4 <- combine_bins_con(woe10_3, bins10_4)
generate_woe_plot_cat(woe10_4, "v10 WOE4 - from 10 bins")
iv10_4 <- sum(woe10_4$iv, na.rm=TRUE)
iv10_4

#iv10_2 is the highest at 0.1461525

---
# v12
summary(trainSplit$v12)
ggplot(trainSplit, aes(x = v12, fill = factor(target))) + 
  geom_histogram(color = "white", 
                 aes(y=..count../sum(..count..)), bins = 20) +
  labs(y = "Proportion Observations", title = "v12 Info - 20 bins")

bins12_1 <- seq(from = min(trainSplit$v12), to = max(trainSplit$v12), length.out = 21)
woe12_1 <- calculate_woe(trainSplit$target, trainSplit$v12, bins12_1)
generate_woe_plot_con(woe12_1, "v12 WOE1 - 20 bins")
iv12_1 <- sum(woe12_1$iv, na.rm=TRUE)
iv12_1

# v12: Combine bin 1-7, 8, 9, 10, 11-12, 13-15, 16-20
bins12_2 <- list(c(1:7), 8, 9, 10, c(11, 12), c(13:15), c(16:20))
woe12_2 <- combine_bins_con(woe12_1, bins12_2)
generate_woe_plot_cat(woe12_2, "v12 WOE2 - from 20 bins")
iv12_2 <- sum(woe12_2$iv, na.rm=TRUE)
iv12_2

ggplot(trainSplit, aes(x = v12, fill = factor(target))) + 
  geom_histogram(color = "white", 
                 aes(y=..count../sum(..count..)), bins = 10) +
  labs(y = "Proportion Observations", title = "v12 Info - 10 bins")

bins12_3 <- seq(from = min(trainSplit$v12), to = max(trainSplit$v12), length.out = 11)
woe12_3 <- calculate_woe(trainSplit$target, trainSplit$v12, bins12_3)
generate_woe_plot_con(woe12_3, "v12 WOE3 - 10 bins")
iv12_3 <- sum(woe12_3$iv, na.rm=TRUE)
iv12_3

# Combine bins 1-4, 5, 6, 7, 8-10
bins12_4 <- list(c(1:4), 5, 6, 7, c(8:10))
woe12_4 <- combine_bins_con(woe12_3, bins12_4)
generate_woe_plot_cat(woe12_4, "v12 WOE4 - from 10 bins")
iv12_4 <- sum(woe12_4$iv, na.rm=TRUE)
iv12_4

# IV_2 is highest at 0.05399145

---
# v21
summary(trainSplit$v21)
ggplot(trainSplit, aes(x = v21, fill = factor(target))) + 
  geom_histogram(color = "white", 
                 aes(y=..count../sum(..count..)), bins = 20) +
  labs(y = "Proportion Observations", title = "v21 Info - 20 bins")

bins21_1 <- seq(from = min(trainSplit$v21), to = max(trainSplit$v21), length.out = 21)
woe21_1 <- calculate_woe(trainSplit$target, trainSplit$v21, bins21_1)
generate_woe_plot_con(woe21_1, "v21 WOE1 - 20 bins")
iv21_1 <- sum(woe21_1$iv, na.rm=TRUE)
iv21_1 # 0.0925253113

# v21: Combine bin 1-6, 7, 8-10, 11, 12, 13, 14, 15-20
bins21_2 <- list(c(1:6), 7, c(8:10), 11, 12, 13, 14, c(15:20))
woe21_2 <- combine_bins_con(woe21_1, bins21_2)
generate_woe_plot_cat(woe21_2, "v21 WOE2 - from 20 bins")
iv21_2 <- sum(woe21_2$iv, na.rm=TRUE)
iv21_2 # 0.08575021

ggplot(trainSplit, aes(x = v21, fill = factor(target))) + 
  geom_histogram(color = "white", 
                 aes(y=..count../sum(..count..)), bins = 10) +
  labs(y = "Proportion Observations", title = "v21 Info - 10 bins")

bins21_3 <- seq(from = min(trainSplit$v21), to = max(trainSplit$v21), length.out = 11)
woe21_3 <- calculate_woe(trainSplit$target, trainSplit$v21, bins21_3)
generate_woe_plot_con(woe21_3, "v21 WOE3 - 10 bins")
iv21_3 <- sum(woe21_3$iv, na.rm=TRUE)
iv21_3 # 0.08693143

# Combine bins 1-3, 4, 5, 6, 7, 8-10
bins21_4 <- list(c(1:3), 4, 5, 6, 7, c(8:10))
woe21_4 <- combine_bins_con(woe21_3, bins21_4)
generate_woe_plot_cat(woe12_4, "v21 WOE4 - from 10 bins")
iv21_4 <- sum(woe21_4$iv, na.rm=TRUE)
iv21_4 # 0.08159985


# v24 - Toss this variable
ggplot(trainSplit, aes(x = v24, fill = factor(target))) + 
  geom_bar(color = "white", 
                 aes(y=..count../sum(..count..))) +
  labs(y = "Proportion Observations", title = "v24 Info")
  
woe24_1 <- calculate_woe(trainSplit$target, trainSplit$v24, levels(factor(trainSplit$v24)), cont=FALSE)
generate_woe_plot_cat(woe24_1, "v24 WOE1")
iv24_1 <- sum(woe24_1$iv, na.rm=TRUE)
iv24_1 # 0.01096248


# v47 
ggplot(trainSplit, aes(x = v47, fill = factor(target))) + 
  geom_bar(color = "white", 
           aes(y=..count../sum(..count..))) +
  labs(y = "Proportion Observations", title = "v47 Info")

woe47_1 <- calculate_woe(trainSplit$target, trainSplit$v47, levels(factor(trainSplit$v47)), cont=FALSE)
generate_woe_plot_cat(woe47_1, "v47 WOE1")
iv47_1 <- sum(woe47_1$iv, na.rm=TRUE)
iv47_1 # 0.1621444

# v47 - A-C-D, B-F-I, E-G-J
woe47_2 <- calculate_woe(trainSplit$target, trainSplit$v47, list(c("A", "C", "D"), c("B", "F", "I"),
                                                                c("E", "G", "J")), cont=FALSE)
generate_woe_plot_cat(woe47_2, "v47 WOE2")
iv47_2 <- sum(woe47_2$iv, na.rm=TRUE)
iv47_2 # 0.1573225


# v50
summary(trainSplit$v50)
ggplot(trainSplit, aes(x = v50, fill = factor(target))) + 
  geom_histogram(color = "white", 
                 aes(y=..count../sum(..count..)), bins = 20) +
  labs(y = "Proportion Observations", title = "v50 Info - 20 bins")

bins50_1 <- seq(from = min(trainSplit$v50), to = max(trainSplit$v50), length.out = 21)
woe50_1 <- calculate_woe(trainSplit$target, trainSplit$v50, bins50_1)
generate_woe_plot_con(woe50_1, "v50 WOE1 - 20 bins")
iv50_1 <- sum(woe50_1$iv, na.rm=TRUE)
iv50_1 # 0.4523824

# v50: Combine bin 1, 2, 3, 4, 5, 6-7, 8-9, 10-11, 12-20
bins50_2 <- list(1, 2, 3, 4, 5, c(6, 7), c(8, 9), c(10, 11), c(12:20))
woe50_2 <- combine_bins_con(woe50_1, bins50_2)
generate_woe_plot_cat(woe50_2, "v50 WOE2 - from 20 bins")
iv50_2 <- sum(woe50_2$iv, na.rm=TRUE)
iv50_2 # 0.4500647


# v52 - Toss this variable
summary(trainSplit$v52)
ggplot(trainSplit, aes(x = v52, fill = factor(target))) + 
  geom_bar(color = "white", 
           aes(y=..count../sum(..count..))) +
  labs(y = "Proportion Observations", title = "v52 Info")

woe52_1 <- calculate_woe(trainSplit$target, trainSplit$v52, levels(factor(trainSplit$v52)), cont=FALSE)
generate_woe_plot_cat(woe52_1, "v52 WOE1")
iv52_1 <- sum(woe52_1$iv, na.rm=TRUE)
iv52_1 # 0.002302835


# v62
summary(trainSplit$v62)
ggplot(trainSplit, aes(x = v62, fill = factor(target))) + 
  geom_bar(color = "white", 
           aes(y=..count../sum(..count..))) +
  labs(y = "Proportion Observations", title = "v62 Info")

woe62_1 <- calculate_woe(trainSplit$target, trainSplit$v62, levels(factor(trainSplit$v62)), cont=FALSE)
generate_woe_plot_cat(woe62_1, "v62 WOE1")
iv62_1 <- sum(woe62_1$iv, na.rm=TRUE)
iv62_1 # Contains empty bins

# v62 - Combine categories 0-4-5-6-7, 1, 2-3
bins62_2 <- list(1, c(2, 3), c(0, 4, 5, 6, 7))
woe62_2 <- calculate_woe(trainSplit$target, trainSplit$v62, bins62_2, cont=FALSE)
generate_woe_plot_cat(woe62_2, "v62 WOE2")
iv62_2 <- sum(woe62_2$iv, na.rm=TRUE)
iv62_2 # 0.146551


# v66
summary(trainSplit$v66)
ggplot(trainSplit, aes(x = v66, fill = factor(target))) + 
  geom_bar(color = "white", 
           aes(y=..count../sum(..count..))) +
  labs(y = "Proportion Observations", title = "v66 Info")

woe66_1 <- calculate_woe(trainSplit$target, trainSplit$v66, levels(factor(trainSplit$v66)), 
                         cont=FALSE)
generate_woe_plot_cat(woe66_1, "v66 WOE1")
iv66_1 <- sum(woe66_1$iv, na.rm=TRUE)
iv66_1 # 0.1282981


# 71 - toss this variable
summary(trainSplit$v71)
ggplot(trainSplit, aes(x = v71, fill = factor(target))) + 
  geom_bar(color = "white", 
           aes(y=..count../sum(..count..))) +
  labs(y = "Proportion Observations", title = "v71 Info")

woe71_1 <- calculate_woe(trainSplit$target, trainSplit$v71, levels(factor(trainSplit$v71)), 
                         cont=FALSE)
generate_woe_plot_cat(woe71_1, "v71 WOE1")
iv71_1 <- sum(woe71_1$iv, na.rm=TRUE)
iv71_1 # Empty bins

# v71 - Combine bins A-B, C-I, D-F-G-L
bins71_2 <- list(c("A", "B"), c("C", "I"), c("D", "F", "G", "L"))
woe71_2 <- calculate_woe(trainSplit$target, trainSplit$v71, bins71_2, cont=FALSE)
generate_woe_plot_cat(woe71_2, "v71 WOE2")
iv71_2 <- sum(woe71_2$iv, na.rm=TRUE)
iv71_2 # 0.00593427


# 72
summary(trainSplit$v72)
ggplot(trainSplit, aes(x = v72, fill = factor(target))) + 
  geom_bar(color = "white", 
           aes(y=..count../sum(..count..))) +
  labs(y = "Proportion Observations", title = "v72 Info")

woe72_1 <- calculate_woe(trainSplit$target, trainSplit$v72, levels(factor(trainSplit$v72)), 
                         cont=FALSE)
generate_woe_plot_cat(woe72_1, "v72 WOE1")
iv72_1 <- sum(woe72_1$iv, na.rm=TRUE)
iv72_1 # Empty bins

# v72 - Combine bins 10-11 and keep rest (doesn't meet req of 5%)
bins72_2 <- list(0, 1, c(10, 11), 12, 2, 3, 4, 5, 6, 7, 8, 9)
woe72_2 <- calculate_woe(trainSplit$target, trainSplit$v72, bins72_2, cont=FALSE)
generate_woe_plot_cat(woe72_2, "v72 WOE2")
iv72_2 <- sum(woe72_2$iv, na.rm=TRUE)
iv72_2 # 0.05237443

# v72 - Combine bins 0-12-2, 3-4-5-6-7-8-9-10-11
bins72_3 <- list(c(0, 12, 2), c(3, 4, 5, 6, 7, 8, 9, 10, 11))
woe72_3 <- calculate_woe(trainSplit$target, trainSplit$v72, bins72_3, cont=FALSE)
generate_woe_plot_cat(woe72_3, "v72 WOE3")
iv72_3 <- sum(woe72_3$iv, na.rm=TRUE)
iv72_3 # 0.04107886


# v91 - toss this variable
summary(trainSplit$v91)
ggplot(trainSplit, aes(x = v91, fill = factor(target))) + 
  geom_bar(color = "white", 
           aes(y=..count../sum(..count..))) +
  labs(y = "Proportion Observations", title = "v91 Info")

woe91_1 <- calculate_woe(trainSplit$target, trainSplit$v91, levels(factor(trainSplit$v91)), 
                         cont=FALSE)
generate_woe_plot_cat(woe91_1, "v91 WOE1")
iv91_1 <- sum(woe91_1$iv, na.rm=TRUE)
iv91_1 # 0.006104932


# v114
summary(trainSplit$v114)
ggplot(trainSplit, aes(x = v114, fill = factor(target))) + 
  geom_histogram(color = "white", 
                 aes(y=..count../sum(..count..)), bins = 20) +
  labs(y = "Proportion Observations", title = "v114 Info - 20 bins")

bins114_1 <- seq(from = min(trainSplit$v114), to = max(trainSplit$v114), length.out = 21)
woe114_1 <- calculate_woe(trainSplit$target, trainSplit$v114, bins114_1)
generate_woe_plot_con(woe114_1, "v114 WOE1 - 20 bins")
iv114_1 <- sum(woe114_1$iv, na.rm=TRUE)
iv114_1 # 0.03760965

# v114: Combine bin 1-6, 7-8, 9, 10-11, 12, 13-14-15, 16, 17-20
bins114_2 <- list(c(1:6), c(7, 8), 9, c(10, 11), 12, c(13, 14, 15), c(17:20))
woe114_2 <- combine_bins_con(woe114_1, bins114_2)
generate_woe_plot_cat(woe114_2, "v114 WOE2 - from 20 bins")
iv114_2 <- sum(woe114_2$iv, na.rm=TRUE)
iv114_2 # 0.03501795


# v129
summary(trainSplit$v129)
ggplot(trainSplit, aes(x = v129, fill = factor(target))) + 
  geom_bar(color = "white", 
           aes(y=..count../sum(..count..))) +
  labs(y = "Proportion Observations", title = "v129 Info")

woe129_1 <- calculate_woe(trainSplit$target, trainSplit$v129, levels(factor(trainSplit$v129)), 
                         cont=FALSE)
generate_woe_plot_cat(woe129_1, "v129 WOE1")
iv129_1 <- sum(woe129_1$iv, na.rm=TRUE)
iv129_1 # Empty bins

# v129 - Combine 0-10, 1-2-5, 6-11-7-8-3-4
bins129_2 <- list(c(0, 10), c(1, 2, 5, 6, 11, 7, 8, 3, 4))
woe129_2 <- calculate_woe(trainSplit$target, trainSplit$v129, bins129_2, cont=FALSE)
generate_woe_plot_cat(woe129_2, "v129 WOE2")
iv129_2 <- sum(woe129_2$iv, na.rm=TRUE)
iv129_2 # 0.175497


# Summary of the highest IV values for the each of the variables:
#
# v10 - iv10_2 = 0.1461525 **
# v12 - iv12_2 = 0.05399145 *
# v21 - iv21_2 = 0.08575021 *
#   v24 - iv24_1 = 0.01096248 (TOSS)
# v47 - iv47_2 = 0.1573225 **
# v50 - iv50_2 = 0.4500647 ***
#   v52 - iv52_1 = 0.002302835 (TOSS)
# v62 - iv62_2 = 0.146551 **
# v66 - iv66_1 = 0.1282981 **
#   v71 - iv71_2 = 0.00593427 (TOSS)
# v72 - iv72_3 = 0.04107886 *
#   v91 - iv91_1 = 0.006104932 (TOSS)
# v114 - iv114_2 = 0.03501795 *
# v129 - iv129_2 = 0.175497 **


# Apply the weight of evidence on both training and testing 

# Remove lowest IV variables
trainSplit$v24 = NULL
testSplit$v24 = NULL

trainSplit$v52 = NULL
testSplit$v52 = NULL

trainSplit$v71 = NULL
testSplit$v71 = NULL

trainSplit$v91 = NULL
testSplit$v91 = NULL

# Replace rest
trainSplit$v10 = replace_with_woe(trainSplit$v10, woe10_2, cont=FALSE)
testSplit$v10 = replace_with_woe(testSplit$v10, woe10_2, cont=FALSE)

trainSplit$v12 = replace_with_woe(trainSplit$v12, woe12_2, cont=FALSE)
testSplit$v12 = replace_with_woe(testSplit$v12, woe12_2, cont=FALSE)

trainSplit$v21 = replace_with_woe(trainSplit$v21, woe21_2, cont=FALSE)
testSplit$v21 = replace_with_woe(testSplit$v21, woe21_2, cont=FALSE)

trainSplit$v47 = replace_with_woe(trainSplit$v47, woe47_2, cont=FALSE)
testSplit$v47 = replace_with_woe(testSplit$v47, woe47_2, cont=FALSE)

trainSplit$v50 = replace_with_woe(trainSplit$v50, woe50_2, cont=FALSE)
testSplit$v50 = replace_with_woe(testSplit$v50, woe50_2, cont=FALSE)

trainSplit$v62 = replace_with_woe(trainSplit$v62, woe62_2, cont=FALSE)
testSplit$v62 = replace_with_woe(testSplit$v62, woe62_2, cont=FALSE)

trainSplit$v66 = replace_with_woe(trainSplit$v66, woe66_1, cont=FALSE)
testSplit$v66 = replace_with_woe(testSplit$v66, woe66_1, cont=FALSE)

trainSplit$v72 = replace_with_woe(trainSplit$v72, woe72_3, cont=FALSE)
testSplit$v72 = replace_with_woe(testSplit$v72, woe72_3, cont=FALSE)

trainSplit$v114 = replace_with_woe(trainSplit$v114, woe114_2, cont=FALSE)
testSplit$v114 = replace_with_woe(testSplit$v114, woe114_2, cont=FALSE)

trainSplit$v129 = replace_with_woe(trainSplit$v129, woe129_2, cont=FALSE)
testSplit$v129 = replace_with_woe(testSplit$v129, woe129_2, cont=FALSE)

# Apply logistic regression
trainSplit$target = factor(trainSplit$target)
testSplit$target = factor(testSplit$target)

model1 <- glm(target ~ v50 + v47 + v10 + v62 + v66,
               data=trainSplit, family="binomial")
summary(model1)
pred1 <- predict(model1, newdata = testSplit, type = "response")
table(testSplit$target, pred1 > 0.5)
confusionMatrix(factor(as.numeric(pred1 > 0.5)), testSplit$target)

table(testSplit$target, pred1 > 0.8)  
confusionMatrix(factor(as.numeric(pred1 > 0.8)), testSplit$target)

