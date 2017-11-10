# File-Name:      capstone_feature_enrichment.R
# Date:           
# Author:         Monisha Gopalakrishnan
# Purpose:        
#
# Data Used:      
# Packages Used:  dplyr, ggplot2


## Library/Sources ------------------------------
library(dplyr)
library(ggplot2)



## Function(s) ----------------------------------

calculate_woe <- function(target, ind_var, ranges, cont=TRUE) {
  # Creates a data frame containing information needed
  # to plot WOE and to calculate IV
  #
  # Args:
  #   target - Target variable (dependent variable) vector
  #   ind_var - Independent variable vector
  #   bins - Vector containing start value of each bin (if continuous var);
  #          List containing vectors with categories, 
  #          each vector is bin (if cat var)
  #   cont - default = TRUE. Whether dealing with continuous or
  #          categorical data
  #
  # Returns:
  #   Data frame containing the following information:
  #   Range, Bin number, No. non-events, No. events, 
  #   percentage non-events, percentage events, WOE, IV

  dframe <- data.frame(target = target, ind_var = ind_var)
  
  n_dframe <- data.frame(ranges = character(length(ranges)))
  n_dframe$ranges <- ranges
  colnames(n_dframe) <- "ranges"
  n_dframe$bins <- 1:nrow(n_dframe)
  n_dframe$num_events <- 0
  n_dframe$num_non_events <- 0
  
  if(cont == TRUE) {
    for(i in n_dframe$bins) {
      if(i == nrow(n_dframe)) {
        n_dframe$num_non_events[i] = 0
        n_dframe$num_events[i] = 0
        break
      } else {
        new_targ <- filter(dframe, ind_var >= ranges[i], ind_var < ranges[i+1])$target
        n_dframe$num_events[i] = sum(new_targ)
        n_dframe$num_non_events[i] =  length(new_targ) - n_dframe$num_events[i]
      }
    }
  } else {
    # Categorical data
    for(i in n_dframe$bins) {
      for(cat in n_dframe$ranges[i]) {
        new_targ <- filter(dframe, ind_var == cat)$target
        n_dframe$num_events[i] = n_dframe$num_events[i] + sum(new_targ)
        n_dframe$num_non_events[i] = (length(new_targ) - sum(new_targ)) +
                                     n_dframe$num_non_events[i]
      }
    }
  }
  
  print("here")
  total_events <- sum(n_dframe$num_events)
  total_non_events <- sum(n_dframe$num_non_events)
  n_dframe <- mutate(n_dframe, perc_events = num_events/total_events, 
                               perc_non_events = num_non_events/total_non_events)
  n_dframe <- mutate(n_dframe, 
                     woe = log((num_non_events/total_non_events)/(num_events/total_events)),
                     iv = ((num_non_events/total_non_events)-(num_events/total_events))*
                        log((num_non_events/total_non_events)/(num_events/total_events)))
  return(n_dframe)
}

generate_woe_plot_cat <- function(info) {
  # Generates a bar plot of the woe values
  # for categorical variables
  #
  # Args:
  #   info - data frame with woe information
  #   
  # Returns:
  #   WOE plot
  
  # Convert the ranges into characters for labelling
  # bar plot
  
  
}

generate_woe_plot_con <- function(info) {
  # Generates a bar plot of the woe values 
  # for continuous variables
  #
  # Args:
  #   info - data frame with woe information
  #
  # Returns:
  #   WOE plot
  ggplot(info, aes(x = ranges, y = woe)) + 
    geom_bar(stat="identity")
}



