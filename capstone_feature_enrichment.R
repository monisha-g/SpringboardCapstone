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
        new_targ <- filter(dframe, ind_var >= n_dframe$ranges[i], 
                           ind_var < n_dframe$ranges[i+1])$target
        n_dframe$num_events[i] = sum(new_targ)
        n_dframe$num_non_events[i] = length(new_targ) - n_dframe$num_events[i]
      }
    }
  } else {
    # Categorical data
    for(i in n_dframe$bins) {
      for(cat in n_dframe$ranges[i]) {
        if(length(cat) > 1) {
          for(item in cat) {
            new_targ <- filter(dframe, ind_var == item)$target
            n_dframe$num_events[i] = n_dframe$num_events[i] + sum(as.integer(new_targ))
            n_dframe$num_non_events[i] = (length(new_targ) - sum(as.integer(new_targ))) +
              n_dframe$num_non_events[i]
          }
        } else {
          new_targ <- filter(dframe, ind_var == cat)$target
          n_dframe$num_events[i] = sum(as.integer(new_targ))
          n_dframe$num_non_events[i] = (length(new_targ) - sum(as.integer(new_targ)))
        }
      }
    }
  }
  
  total_events <- sum(n_dframe$num_events)
  total_non_events <- sum(n_dframe$num_non_events)
  n_dframe <- mutate(n_dframe, perc_obs = 100*(num_events+num_non_events)/nrow(dframe))
  n_dframe <- mutate(n_dframe, perc_events = num_events/total_events, 
                               perc_non_events = num_non_events/total_non_events)
  n_dframe <- mutate(n_dframe, 
                     woe = log((num_non_events/total_non_events)/(num_events/total_events)),
                     iv = ((num_non_events/total_non_events)-(num_events/total_events))*
                        log((num_non_events/total_non_events)/(num_events/total_events)))
  return(tbl_df(n_dframe))
}


combine_bins_con <- function(orig_woe, new_bins_list) {
  # Return a new woe table with the new binnings 
  # 
  # Args:
  #   orig_woe - the original woe table
  #   new_bins_list - list of vectors where each vector includes all the 
  #                   old bins that should be in this new bin
  #
  # Return:
  #   A new woe table where the ranges now have lists of the old bins. New
  #   WOE and IV values are calculated. The woe table structure should be
  #   similar to a categorical variable's woe table.
  
  n_dframe <- data.frame(ranges = character(length(new_bins_list)))
  n_dframe$ranges <- new_bins_list
  colnames(n_dframe) <- "ranges"
  n_dframe$bins <- 1:length(new_bins_list)
  n_dframe$num_events <- 0
  n_dframe$num_non_events <- 0
  
  print(new_bins_list)
  print(n_dframe$bins)
  for(bin in n_dframe$bins) {
    print("bin")
    print(bin)
    for(cat in n_dframe$ranges[bin]) {
      print(cat)
      if(length(cat) > 1) {
        print(cat)
        filt_woe <- filter(orig_woe, bins %in% cat)
        n_dframe$num_events[bin] = sum(filt_woe$num_events)
        n_dframe$num_non_events[bin] = sum(filt_woe$num_non_events)
      } else {
        n_dframe$num_events[bin] = orig_woe$num_events[cat]
        n_dframe$num_non_events[bin] = orig_woe$num_non_events[cat]
      }
    }
  }
  
  total_events <- sum(n_dframe$num_events)
  total_non_events <- sum(n_dframe$num_non_events)
  total_obs <- total_events + total_non_events
  n_dframe <- mutate(n_dframe, perc_obs = 100*(num_events+num_non_events)/total_obs)
  n_dframe <- mutate(n_dframe, perc_events = num_events/total_events, 
                     perc_non_events = num_non_events/total_non_events)
  n_dframe <- mutate(n_dframe, 
                     woe = log((num_non_events/total_non_events)/(num_events/total_events)),
                     iv = ((num_non_events/total_non_events)-(num_events/total_events))*
                       log((num_non_events/total_non_events)/(num_events/total_events)))
  return(tbl_df(n_dframe))
}


generate_woe_plot_cat <- function(info, title) {
  # Generates a bar plot of the woe values
  # for categorical variables
  #
  # Args:
  #   info - data frame with woe information
  #   title - name of variable
  #   
  # Returns:
  #   WOE plot
  
  # Convert the ranges into characters for labelling
  # bar plot
  info$range_labels <- sapply(info$ranges, paste0, collapse=" ")
  ggplot(info, aes(x = range_labels, y=woe, 
                   fill = perc_obs >= 5, alpha = perc_obs)) +
    geom_bar(stat="identity", position = "identity") + 
    geom_text(aes(label = bins), alpha = 1) +
    labs(x = "Bin contents", y = "WOE score", title = title)
}

generate_woe_plot_con <- function(info, title) {
  # Generates a bar plot of the woe values 
  # for continuous variables
  #
  # Args:
  #   info - data frame with woe information
  #   title - name of variable 
  #
  # Returns:
  #   WOE plot
  ggplot(info, aes(x = ranges, y = woe,
                   fill = perc_obs >= 5, alpha = perc_obs)) + 
    geom_bar(stat="identity", position="dodge") +
    geom_text(aes(label = bins), alpha = 1) +
    labs(x = "Range", y = "WOE score", title = title)
}


replace_with_woe <- function(var, woe_frame, cont=TRUE) {
  # Returns the feature replaced with
  # it's woe values
  #
  # Args:
  #   var - variable vector to be replaced
  #   woe_frame - data frame with woe information
  #
  # Returns:
  #   Vector where the values of a feature are replaced with
  #   respective WOE values given by the table
  
  if(cont == TRUE) {
    # Continuous
    for(i in woe_frame$bins) {
      if(i != length(woe_frame$bins)) {
        lower <- woe_frame$ranges[i]
        upper <- woe_frame$ranges[i+1]
        var <- replace(var, var >= lower & var < upper, woe_frame$woe[i])
      }
    }
  } else {
    # Categorical
    if(is.factor(var)) {
      var <- as.character(var)
    }
    for(bin in woe_frame$bins) {
      for(cat in woe_frame$ranges[bin]) {
        if(length(cat) > 1) {
          for(item in cat) {
            var <- replace(var, var==item, woe_frame$woe[bin])
          }
        } else {
          var <- replace(var, var==cat, woe_frame$woe[bin])
        }
      }
    }
  }
  return(as.numeric(var))
}


