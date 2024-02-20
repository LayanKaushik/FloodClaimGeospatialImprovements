# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.14.4
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# +
import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.ensemble import AdaBoostRegressor
from sklearn.model_selection import cross_validate

def scale_variables(df, target_variable, scale=True):
    """
    Scales numeric variables in a dataframe, excluding binary variables and a specified target variable.
    
    Parameters:
    - df: pandas DataFrame.
    - target_variable: string, name of the target variable to exclude from scaling.
    - scale: boolean, indicates whether scaling should be performed (default is True).
    
    Returns:
    - DataFrame with scaled numeric variables if scale is True; otherwise, returns the original DataFrame.
    """
    if scale:
        # Identify numeric columns, excluding the target variable and binary variables
        numeric_vars = df.select_dtypes(include=[np.number]).columns.tolist()
        numeric_vars = [
            var for var in numeric_vars 
            if len(df[var].unique()) > 2 and var != target_variable
        ]
        
        # Scale the identified numeric variables
        scaler = StandardScaler()
        df[numeric_vars] = scaler.fit_transform(df[numeric_vars])
    
    return df

def drop_columns(df, columns_to_drop):
    """
    Drops specified columns from a DataFrame.
    
    Parameters:
    - df: pandas DataFrame.
    - columns_to_drop: list of strings, names of the columns to drop.
    
    Returns:
    - DataFrame with the specified columns removed.
    """
    df = df.drop(columns=columns_to_drop)
    return df

def sample_dataframe(df, sample_size, sampling_required=True):
    """
    Conditionally samples a specified number of rows from a DataFrame.
    
    Parameters:
    - df: pandas DataFrame.
    - sample_size: int, number of rows to sample.
    - sampling_required: boolean, indicates whether sampling should be performed (default is True).
    
    Returns:
    - Sampled DataFrame if sampling_required is True and sample_size is less than the number of rows in df; 
      otherwise, returns the original DataFrame.
    """
    if sampling_required:
        if sample_size > len(df):
            print("Sample size is greater than the number of rows in the dataframe. Returning the original dataframe.")
            return df
        # Perform sampling
        sampled_df = df.sample(n=sample_size)
        return sampled_df
    else:
        # Return the original DataFrame if no sampling is required
        return df

def svd_selection_rf(X, svd_df, Y):
    """
    Evaluates the contribution of each SVD feature to the model performance and selects the best one.
    
    Parameters:
    - X: DataFrame containing the original features.
    - svd_df: DataFrame containing SVD-derived features.
    - Y: Series or DataFrame containing the target variable.
    
    Returns:
    - best_col: The name of the SVD column that, when added to the original features, results in the best model performance.
    - best_score: The mean absolute error associated with the best SVD feature.
    """
    
    # Initialize a dictionary to hold the mean absolute error for each SVD feature
    scores = {}
    
    # Iterate over each SVD column
    for col in svd_df.columns:
        # Combine the original features with the current SVD feature
        XSvd = np.concatenate([X, svd_df[[col]].values], axis=1)
        
        # Initialize the RandomForest regressor with specified parameters
        regr = RandomForestRegressor(criterion='absolute_error', max_depth=40, random_state=420)
        
        # Perform cross-validation and calculate negative mean absolute error
        cv_results = cross_validate(regr, XSvd, Y, cv=2, scoring='neg_mean_absolute_error')
        
        # Convert negative MAE to positive and store it
        scores[col] = -np.mean(cv_results['test_score'])
        print('Evaluating:', col, 'with MAE:', scores[col])
    
    # Determine the SVD feature with the lowest mean absolute error
    best_col = min(scores, key=scores.get)
    best_score = scores[best_col]
    
    # Print the best SVD feature and its score
    print(f"Best SVD Feature: {best_col}, MAE: {best_score}")

    return best_col, best_score

def svd_selection_ada(X, svd_df, Y):
    """
    Evaluates the contribution of each SVD feature to the model performance using AdaBoost and selects the best one.
    
    Parameters:
    - X: DataFrame containing the original features.
    - svd_df: DataFrame containing SVD-derived features.
    - Y: Series or DataFrame containing the target variable.
    
    Returns:
    - best_col: The name of the SVD column that, when added to the original features, results in the best model performance.
    - best_score: The mean absolute error associated with the best SVD feature.
    """
    
    # Initialize a dictionary to hold the mean absolute error for each SVD feature
    scores = {}

    # Define the base estimator with a criterion of absolute error
    base_estimator = DecisionTreeRegressor(criterion='absolute_error', random_state=420)
    
    # Iterate over each SVD column
    for col in svd_df.columns:
        # Combine the original features with the current SVD feature
        XSvd = np.concatenate([X, svd_df[[col]].values], axis=1)
        
        # Initialize the AdaBoost regressor with specified parameters
        regr = AdaBoostRegressor(estimator=base_estimator, n_estimators=100, learning_rate=0.1, loss='linear', random_state=420)
        
        # Perform cross-validation and calculate negative mean absolute error
        cv_results = cross_validate(regr, XSvd, Y, cv=2, scoring='neg_mean_absolute_error')
        
        # Convert negative MAE to positive and store it
        scores[col] = -np.mean(cv_results['test_score'])
        print('Evaluating:', col, 'with MAE:', scores[col])
    
    # Determine the SVD feature with the lowest mean absolute error
    best_col = min(scores, key=scores.get)
    best_score = scores[best_col]
    
    # Print the best SVD feature and its score
    print(f"Best SVD Feature: {best_col}, MAE: {best_score}")
    
    return best_col, best_score