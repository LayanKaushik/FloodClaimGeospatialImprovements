# utils_fractional_regression.R

# Load necessary libraries
library(dplyr)

# Function to scale numeric variables excluding binary and the target variable
scale_variables <- function(df, target_variable, scale = TRUE) {
  if (scale) {
    # Identify numeric variables excluding binary and the target variable
    numeric_vars <- sapply(df, is.numeric) & 
      !sapply(df, function(x) all(x %in% c(0, 1))) & 
      !(names(df) %in% target_variable)
    
    # Apply scaling on the identified numeric variables
    df[numeric_vars] <- scale(df[numeric_vars])
  }
  
  return(df)
}

# Function to drop specified columns from the dataframe
drop_columns <- function(df, columns_to_drop) {
  df <- df[, !(names(df) %in% columns_to_drop)]
  return(df)
}

# Function to calculate mean absolute error
mae_cost <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# Function to conditionally sample a dataframe
sample_dataframe <- function(df, sample_size, sampling_required = TRUE) {
  if (sampling_required) {
    if(sample_size > nrow(df)) {
      warning("Sample size is greater than the number of rows in the dataframe. Returning the original dataframe.")
      return(df)
    }
    sampled_indices <- sample(nrow(df), sample_size)
    sampled_df <- df[sampled_indices, ]
    return(sampled_df)
  } else {
    return(df)
  }
}