preprocess_removeZeroVarianceColumns <- function(trainData,testData)
{
  # This function removes zero variance columns in all data based on train data.
  
  columnsToKeep_levels = sapply(trainData, function(col) length(unique(col))) # count unique (including NA) entries per column in the train data
  columnsToKeep_nas = sapply(trainData, function(col) any(is.na(unique(col)))) # check if at least one NA is present per column in the train data
  columnsToKeep_levels[columnsToKeep_nas] = columnsToKeep_levels[columnsToKeep_nas]-1 # count unique (excluding NA) entries per column in the train data
  columnsToKeep = columnsToKeep_levels>1 # TRUE if column is not unique thus keep column
  newTrainData = trainData[,columnsToKeep] #keep all columns that are not unique in trainData
  newTestData = testData[,columnsToKeep] # keep the same columns also in testData
  return(list(newTrainData,newTestData))
}