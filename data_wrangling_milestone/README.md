Summary of most important steps taken to clean up the dataset (train):

1. Replaced all the blank spaces in the categorical variables with NA values
2. Removed all features that had more than 25% NA values
3. Removed the outliers in continuous variables and replaced with NA values
4. Replaced the NA values in continuous variables with the mean
5. Replaced the NA values in categorical variables with the mode
6. Removed features with zero/near-zero variance
7. Removed categorical variables with more than 20 categories
8. Removed features that were highly correlated with others using VIF
