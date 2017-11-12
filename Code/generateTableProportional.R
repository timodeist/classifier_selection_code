generateTableProportional = function(inputTable,datasetNamesToPatientNumbersMapping)
{
  # This function creates a copy of the inputTable with copies of each row 
  # proportional to the number of patients in the respective data set.
  library(plyr)
  # generate vector of patient numbers (entry i corresponds to the number of patients in the dataset used in row i of data frame inputTable)
  frequency = as.numeric(revalue(as.character(inputTable$dataset),datasetNamesToPatientNumbersMapping))
  
  # weigh each row by repeating each row i times the number in frequency[i] (the number of patients of the respective dataset)
  table_proportional = inputTable[rep(row.names(inputTable),frequency),]
  return(table_proportional)
}
