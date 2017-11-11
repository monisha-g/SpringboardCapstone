# File-Name:      
# Date:           
# Author:         Monisha Gopalakrishnan
# Purpose:        
#
# Data Used:      
# Packages Used:  dplyr, caret, plyr


## Library/Sources
library(dplyr)
library(caret)
library(plyr)




## Function(s)
remove_blank_spaces <- function(d_frame) {
  # Removes the blank spaces and replaces them with NA
  # values to make it easier to process
  #
  # Args:
  #   d_frame - original data
  #
  # Returns:
  #   New data frame with replaced values
  
  remove_blanks_by_column <- function(column) {
    if(is.factor(column)) {
      levels(column)[levels(column)==""] <- NA
    }
    return(column)
  }
  
  new_d_frame <- lapply(d_frame, remove_blanks_by_column)
  return(tbl_df(new_d_frame))
}


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


replace_na_continuous <- function(d_frame, loc_vec) {
  # Returns a data frame of continuous variables where
  # all the NA values in a given column is replaced
  # by the non-NA mean
  #
  # Args:
  #   d_frame - Original data containing only continuous
  #             variables
  #
  # Returns:
  #   New data frame with all the NA values filled in cont variables
  
  replace_na_mean <- function(column) {
    if(is.double(column) & !is.integer(column)) {
      mn <- mean(column, na.rm = TRUE)
      column[is.na(column)] <- mn
    }
    return(column)
  }
  
  return(tbl_df(lapply(d_frame[loc_vec], replace_na_mean)))
  
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
    if(!is.double(column)) {
      md <- names(sort(table(factor(column)), decreasing=TRUE)[1])
      column[is.na(column)] <- md
    }
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
  #   loc_vec
  #
  # Returns:
  #   The original data frame with outliers removed
  
  remove_outlier_by_column <- function(column) {
    if(is.double(column) & !is.integer(column)) {
      iqr <- IQR(column, na.rm = TRUE)
      mn <- mean(column, na.rm = TRUE)
      column[column >= mn + (k*iqr) | column <= mn - (k*iqr)] = NA
    }
    return(column)
  }
  
  return(tbl_df(lapply(d_frame, remove_outlier_by_column)))
}


remove_near_zero_variability <- function(d_frame) {
  # Removes columns with near zero variability
  #
  # Args:
  #   d_frame - original data frame
  #   min_var - minimum allowable variability
  #
  # Returns:
  #   A data frame where columns with near zero
  #   variability are removed.
    
  new_d_frame <- d_frame[-nearZeroVar(d_frame)]
  return(new_d_frame)
}


remove_lots_categories <- function(d_frame, k) {
  # Removes categorical variables with more than k number of
  # categories
  #
  # Args:
  #   d_frame - original data frame
  #   k - maximum number of allowable categories
  #
  # Return:
  #   A data frame with those categorical variables removed
  
  loc <- sapply(d_frame, function(x) length(levels(factor(x))) <= k | 
                  is.double(x))
  return(d_frame[loc])
}


