preprocess_imputeDataset <- function(trainData,fullData)
{
  # This function imputes missing values on all data using train data information,
  # uses medians for continuous variables and modes for categorical variables.
  
  library(imputeMissings)
  
  #only impute if there is any missing value in the full dataset
  if (any(is.na(fullData)))
  {
    imputationValues <- compute(trainData, method = 'median/mode') # compute medians and modes from trainData
    imputedFullData <- impute(fullData, object = imputationValues, flag = TRUE) # replace missing values in the full dataset with medians/modes from trainData; add indicator variables for each column where missing values were imputed
  }
  else
  {
    imputedFullData = fullData # if there aren't any missing values, just return the original fullData variable
  }
  
  return(imputedFullData)
}