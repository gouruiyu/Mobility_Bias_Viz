# Bias correction model for undercounting
# Wrap in the function purely to make the global env clean
# i.e. dataframe loaded should not be accessible from the global env

uc_correction_model = NULL

# return an OLS model to predict counted from detected
fitModel <- function(df) {
  uc_correction_model = lm(counted ~ detected, data=df)
  # TODO: Add Q/F category after labeled data is available
}


