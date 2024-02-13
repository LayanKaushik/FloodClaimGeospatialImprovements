rm.list()
set.seed(420) # for reproducibility

library(lmtest)
library(sandwich)
library(arrow)
library(mgcv)
library(boot)

# read this: https://m-clark.github.io/posts/2019-08-20-fractional-regression/

#dataset with encoding
df <- read_parquet('C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/filtered_dataset_with_geo_info_with_encoding.parquet.gzip')

vars_to_exclude = paste0("svd_", 1:30) # creates svd_1, svd_2, ..., svd_30
df_modified = df[, !(names(df) %in% vars_to_exclude)]

# Identifying numeric variables excluding binary and the target variable
numeric_vars = sapply(df_modified, is.numeric) & !sapply(df_modified, function(x) all(x %in% c(0, 1))) & !(names(df_modified) %in% "buildingrelativeDamage")

# Scaling if required
df_modified[numeric_vars] = scale(df_modified[numeric_vars])


# Define a function to calculate MAE for cross-validation
mae_func <- function(data, indices) {
  d_train <- data[indices$train, ]
  d_test <- data[indices$test, ]
  
  # Fit the model using all predictors except 'buildingrelativeDamage'
  fit <- glm(buildingrelativeDamage ~ ., data=d_train, family=binomial(link="probit")) #can use logit as well
  
  se_glm_robust = coeftest(fit, vcov = vcovHC(fit, type="HC3"))
  
  # Make predictions on the test set
  predictions <- predict(se_glm_robust, newdata=d_test, type="response")
  
  # Calculate and return MAE
  actual <- d_test$buildingrelativeDamage
  mean(abs(predictions - actual))
}

# Prepare data for cross-validation
folds <- cv.glm(df_modified, glm(buildingrelativeDamage ~ ., data=df_modified, family=binomial(link="probit")), K=5, cost=mae_func)

# Average MAE on the left out fold
average_mae <- mean(folds$delta)

#############################


svd_vars <- paste0("svd_", 1:30)

# Initialize a vector to store the MAE for baseline and each SVD variable inclusion
mae_values <- numeric(length(svd_vars) + 1)

# Function to calculate MAE
calc_mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# Calculate baseline MAE with 5-fold CV excluding all SVD variables
baseline_model <- glm(buildingrelativeDamage ~ ., data=df_modified, family=binomial(link="probit"))
baseline_mae <- cv.glm(df_modified, baseline_model, K=5, cost=calc_mae)
mae_values[1] <- mean(baseline_mae$delta)

# Variable selection and iterative cross-validation
included_vars <- character() # to keep track of included SVD variables
for (i in 1:length(svd_vars)) {
  mae_per_var <- setNames(numeric(length(svd_vars)), svd_vars)
  
  for (svd_var in svd_vars[!svd_vars %in% included_vars]) {
    temp_df <- cbind(df_modified, df[, svd_var]) # Temporarily include the SVD variable
    temp_mae <- replicate(30, {
      folds <- cv.glm(temp_df, glm(buildingrelativeDamage ~ . + included_vars + svd_var, data=temp_df, family=binomial(link="probit")), K=2, cost=calc_mae)
      mean(folds$delta)
    })
    mae_per_var[svd_var] <- mean(temp_mae)
  }
  
  # Identify the SVD variable with the lowest MAE
  best_svd_var <- names(which.min(mae_per_var))
  included_vars <- c(included_vars, best_svd_var)
  
  # Rerun 5-fold CV with the newly included SVD variable
  final_df <- cbind(df_modified, df[, included_vars])
  final_model_mae <- cv.glm(final_df, glm(buildingrelativeDamage ~ ., data=final_df, family=binomial(link="probit")), K=5, cost=calc_mae)
  mae_values[i + 1] <- mean(final_model_mae$delta)
  
  # Update svd_vars to exclude the newly included variable
  svd_vars <- svd_vars[!svd_vars %in% included_vars]
}


###########################################################################################################


# check here for documentation: https://www.rdocumentation.org/packages/sandwich/versions/3.1-0/topics/vcovHC



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
