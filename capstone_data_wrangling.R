# File-Name:      
# Date:           
# Author:         Monisha Gopalakrishnan
# Purpose:        
#
# Data Used:      
# Packages Used:  dplyr


## Library/Sources
library(dplyr)




## Function(s)


remove_high_na_cols <- function(d_frame, perc) {
  # Removes the columns in the data frame that have more
  # than a certain percentage of NA values
  #
  # Args:
  #   d_frame - Original data (data.frame)
  #   perc - Max percentage of NA values in any column (numeric)
  #
  # Returns:
  #   A data frame with only the columns in d_frame with
  #   fewer than percent NA values
  
  percent_col_is_na <- function(column, max_percent) {
    percent <- sum(is.na(column)) / length(column)
    return(percent <= max_percent)
  }
  
  new_d_frame <- d_frame[sapply(d_frame, percent_col_is_na, max_percent = perc)]
  return(new_d_frame)
}



only_categorical_df <- function(d_frame) {
  # Returns a data frame with only categorical variables
  # (note that here we consider both integer and character columns
  # as discrete)
  #
  # Args:
  #   d_frame - Original data (data.frame)
  #
  # Returns:
  #   New data frame with only categorical attributes
  
  new_d_frame <- d_frame[sapply(d_frame, is.integer) | 
                         sapply(d_frame, is.character)]
  return(new_d_frame)
}

only_continuous_df <- function(d_frame) {
  # Returns a data frame with only continuous variables
  #
  # Args:
  #   d_frame - Original data (data.frame)
  #
  # Returns:
  #   New data frame with only continuous attributes
  
  new_d_frame <- d_frame[sapply(d_frame, is.double)]
  return(new_d_frame)
}
 

replace_na_continuous <- function(d_frame) {
  # Returns a data frame of continuous variables where
  # all the NA values in a given column is replaced
  # by the non-NA mean
  #
  # Args:
  #   d_frame - Original data containing only continuous
  #             variables
  #
  # Returns:
  #   New data frame with all the NA values filled
  
  replace_na_mean <- function(column) {
    mn <- mean(column, na.rm = TRUE)
    column[is.na(column)] <- mn
    return(column)
  }
  
  return(tbl_df(lapply(d_frame, replace_na_mean)))
  
}

replace_na_categorical <- function(d_frame) {
  # Returns a data frame where all the NA values
  # in a missing attribute are replaced with the
  # most frequently occuring value
  #
  # Args:
  #   d_frame - original data frame with all categorical 
  #             attributes (data.frame)
  #
  # Returns: 
  #   Data frame with replaced NA values
  
  replace_na_mode <- function(column) {
    md <- names(sort(table(factor(column)), decreasing=TRUE)[1])
    column[is.na(column)] <- md
    return(column)
  }
  
  return(tbl_df(lapply(d_frame, replace_na_mode)))
}

remove_outlier_values <- function(d_frame, k = 3) {
  # Removes the outlier values in each numerical attribute 
  # and replaces them with a NA value.
  #
  # Args:
  #   d_frame - original data (data.frame)
  #   k - factor of range (numeric)
  #
  # Returns:
  #   The original data frame with outliers removed
  
  remove_outlier_by_column <- function(column) {
    iqr <- IQR(column, na.rm = TRUE)
    mn <- mean(column, na.rm = TRUE)
    column[column >= mn + (k*iqr) | column <= mn - (k*iqr)] = NA
    return(column)
  }
  
  return(tbl_df(lapply(d_frame, remove_outlier_by_column)))
}



