rm(list=ls())
set.seed(420) # for reproducibility

library(lmtest)
library(sandwich)
library(arrow)
library(mgcv)
library(boot)
library(dplyr)
library(glmnet)
library(ggplot2)

# read this: https://m-clark.github.io/posts/2019-08-20-fractional-regression/

# read dataset with encoding
df <- read_parquet('C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/filtered_dataset_with_geo_info_with_encoding.parquet.gzip')

# drop all county columns
columns_to_remove <- grep("countyCode_", names(df), value = TRUE)
df <- df[, !(names(df) %in% columns_to_remove)]

# drop index column from the dataframe
if ("__index_level_0__" %in% names(df)) {
  df <- df[ , !names(df) %in% "__index_level_0__"]
}

# Randomly sample 10,000 rows from df to test the code
sampled_indices <- sample(nrow(df), 5000)

sampled_df <- df[sampled_indices, ] #careful when using the entire dataset, use sampled_df instead of df for the code to run

# select geoinfo variables and remove them from df to create the baseline model
svd_vars = paste0("svd_", 1:30) # creates svd_1, svd_2, ..., svd_30
df_modified = sampled_df[, !(names(sampled_df) %in% svd_vars)]

# Identifying numeric variables excluding binary and the target variable
numeric_vars = sapply(df_modified, is.numeric) & !sapply(df_modified, function(x) all(x %in% c(0, 1))) & !(names(df_modified) %in% "buildingrelativeDamage")

# Scaling if required
df_modified[numeric_vars] = scale(df_modified[numeric_vars])

# Initialize a vector to store the MAE for baseline and each SVD variable inclusion
mae_values <- c(Baseline = 0, setNames(numeric(length(svd_vars)), svd_vars))

# Cost function for cv.glm that calculates MAE
mae_cost <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

#Baseline Model
# Perform 5 fold cross-validation with cv.glm
baseline_model <- glm(buildingrelativeDamage ~ ., data=df_modified, family=binomial(link="probit"))
baseline_mae <- cv.glm(df_modified, baseline_model, K=5, cost=mae_cost)

# The delta contains two elements: the first is the raw cross-validation estimate of prediction error,
# and the second is the adjusted estimate. For MAE, you'd look at the raw (first) value.
mae_values["Baseline"] <- baseline_mae$delta[1]

#######################################################################################################
## check geoinfo column inclusion ##

# Variable selection and iterative cross-validation
included_vars <- character() # to keep track of included SVD variables

for (i in 1:length(svd_vars)) {
  mae_per_var <- setNames(numeric(length(svd_vars)), svd_vars)
  
  for (svd_var in svd_vars[!svd_vars %in% included_vars]) {
    
    temp_df <- cbind(df_modified, sampled_df[, svd_var]) # Temporarily include the SVD variable
    
    # Perform cross-validation and calculate MAE
    cv_result <- cv.glm(temp_df, glm(buildingrelativeDamage ~ ., data=temp_df, family=binomial(link="probit")), K=2, cost=mae_cost)
    mae_per_var[svd_var] <- mean(cv_result$delta[1])
  }
  
  # Identify the SVD variable with the lowest MAE
  best_svd_var <- names(which.min(mae_per_var))
  included_vars <- c(included_vars, best_svd_var)
  
  # Add the best_svd_var to the original dataframe
  df_modified <- cbind(df_modified, sampled_df[, best_svd_var])
  
  # Rerun 5-fold CV with the newly included SVD variable
  final_model_mae <- cv.glm(df_modified, glm(buildingrelativeDamage ~ ., data=df_modified, family=binomial(link="probit")), K=5, cost=mae_cost)
  mae_values[best_svd_var] <- final_model_mae$delta[1]
  
  svd_vars <- svd_vars[!svd_vars %in% included_vars]
  
  print(mae_values)
  
}

## Plot

# Convert mae_values to a data frame for ggplot
mae_df <- data.frame(Variable = names(mae_values), MAE = mae_values)

# Plot
ggplot(mae_df, aes(x = Variable, y = MAE)) +
  geom_line(aes(group = 1), color = "black", size = 0.5) +
  geom_point(color = "darkgreen", size = 3) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) + 
  labs(title = "MAE Across Models with SVD Variable Inclusion",
       x = "SVD Variable Included",
       y = "Mean Absolute Error (MAE)")



# save.image(file = paste0("fractional_regression_output_", "-", timestamp, ".RData"))

####Fin#####
################################################################################################################


# check here for documentation: https://www.rdocumentation.org/packages/sandwich/versions/3.1-0/topics/vcovHC

# 
# # Define a function to calculate MAE for cross-validation
# mae_func <- function(data, indices) {
#   
#   d_train <- data[indices$train, ]
#   d_test <- data[indices$test, ]
#   
#   # Fit the model using all predictors except 'buildingrelativeDamage'
#   fit <- glm(buildingrelativeDamage ~ ., data=d_train, family=binomial(link="probit"))  #can use logit as well
#   
#   # Make predictions on the test set
#   predictions <- predict(fit, newdata=d_test, type="response")
#   
#   # Calculate and return MAE
#   actual <- d_test$buildingrelativeDamage
#   mean(abs(predictions - actual))
# }
# 
# K <- 5  # Number of folds
# 
# # Create indices for K-fold cross-validation
# folds_indices <- createFolds(df_modified$buildingrelativeDamage, k = K, list = TRUE, returnTrain = TRUE)
# 
# # Initialize a vector to store MAE for each fold
# mae_values <- numeric(K)
# 
# # Loop through each fold and calculate MAE
# for(i in seq_along(folds_indices)) {
#   indices <- list(train = folds_indices[[i]], test = setdiff(1:nrow(df_modified), folds_indices[[i]]))
#   mae_values[i] <- mae_func_corrected(df_modified, indices)
# }
# 
# # Calculate average MAE across all folds
# average_mae <- mean(mae_values)



## other ways to do fractional regression
 
# model_quasi = glm(
#   prate ~ mrate + ltotemp + age + sole,
#   data = df,
#   family = quasibinomial(link = "probit")
# )
# 
# 
# model_gam_std = gam(
#   prate ~ mrate + ltotemp + age + sole, 
#   data = df, 
#   family = quasibinomial(link = "probit")
# )
# 
# library(frm)
# 
# x_mat = model.matrix(prate ~ mrate + ltotemp + age + sole, df)
# y = d$prate
# 
# model_frm = frm(
#   y,
#   x_mat,
#   linkfrac = 'probit',
#   intercept = FALSE  # included in model matrix
# )
