preprocess_dataset = function(data,data_class,outerFolds,kOuter)
{
  # This function imputes missing values on all data using train data information, 
  # dummy codes categoricals of all data based on all data, 
  # removes zero variance columns in all data based on train data, 
  # rescales columns to [0,1] for all data & dummied data based on train data.
  
  debugSource('preprocess_imputeDataset.R')
  debugSource('preprocess_removeZeroVarianceColumns.R')
  
  # initalize all fold-related variables
  trainFoldLabels = list() 
  trainIndicesOuter = list() 
  testIndicesOuter = list()
  trainDataOuter = list()
  trainDummiedDataOuter = list()
  trainClassOuter = list()
  testDataOuter = list()
  testDummiedDataOuter = list()
  testClassOuter = list()
  trainDataOuter_preImputation = list()
  imputedData = list()
  dummiedImputedData = list()
  dummies = list()
  
  # remove zero variance columns otherwise dummy coding might fail. Second input is rendundant (improve in net version)
  firstZeroVarianceRemovalOutput = preprocess_removeZeroVarianceColumns(data,data)
  data = firstZeroVarianceRemovalOutput[[1]]
  
  # create variables containing indices for folds, create data frames containing the training and test data 
  for (i_kOuter in 1:kOuter)
  {
    trainFoldLabels[[i_kOuter]] = setdiff(1:kOuter,i_kOuter) # list of vectors containing indices of all folds for the training slice
    
    trainIndicesOuter[[i_kOuter]] = unlist(outerFolds[trainFoldLabels[[i_kOuter]]], use.names = FALSE) # list of vectors containing all patient indices for the training slice
    testIndicesOuter[[i_kOuter]] = outerFolds[[i_kOuter]] # list of vectors containing all patient indices for the test slice
    
    trainDataOuter_preImputation[[i_kOuter]] = data[trainIndicesOuter[[i_kOuter]],] # list of data frames containing the training data slice before it is imputed
    
    imputedData[[i_kOuter]] = preprocess_imputeDataset(trainDataOuter_preImputation[[i_kOuter]], data) # run imputation on all data using imputed values from the training slice
    
    # dummy code all factors for the imputed data
    dummies[[i_kOuter]] = dummyVars(' ~.', data = imputedData[[i_kOuter]], fullRank = TRUE) 
    dummiedImputedData[[i_kOuter]] = data.frame(predict(dummies[[i_kOuter]], newdata = imputedData[[i_kOuter]]))
    
    # assign train data
    trainDataOuter[[i_kOuter]] = imputedData[[i_kOuter]][trainIndicesOuter[[i_kOuter]],] # list of data frames containing the training data slice after imputation
    trainDummiedDataOuter[[i_kOuter]] = dummiedImputedData[[i_kOuter]][trainIndicesOuter[[i_kOuter]],] # list of data frames containing the DUMMIED training data slice after imputation
    trainClassOuter[[i_kOuter]] = data_class[trainIndicesOuter[[i_kOuter]]] # list of logical vectors containing the classes for the training data slice
    
    # assign test data
    testDataOuter[[i_kOuter]] = imputedData[[i_kOuter]][testIndicesOuter[[i_kOuter]],] # list of data frames containing the test data slice after imputation
    testDummiedDataOuter[[i_kOuter]] = dummiedImputedData[[i_kOuter]][testIndicesOuter[[i_kOuter]],] # list of data frames containing the test data slice after imputation
    testClassOuter[[i_kOuter]] = data_class[testIndicesOuter[[i_kOuter]]] # list of logical vectors containing the classes for the test data slice
    
    zeroVarianceRemovalOutput = preprocess_removeZeroVarianceColumns(trainDataOuter[[i_kOuter]],testDataOuter[[i_kOuter]])
    trainDataOuter[[i_kOuter]] = zeroVarianceRemovalOutput[[1]]
    testDataOuter[[i_kOuter]] = zeroVarianceRemovalOutput[[2]]
    
    zeroVarianceRemovalOutput = preprocess_removeZeroVarianceColumns(trainDummiedDataOuter[[i_kOuter]],testDummiedDataOuter[[i_kOuter]])
    trainDummiedDataOuter[[i_kOuter]] = zeroVarianceRemovalOutput[[1]]
    testDummiedDataOuter[[i_kOuter]] = zeroVarianceRemovalOutput[[2]]
    
    # rescale data
    preProcessValues = preProcess(trainDataOuter[[i_kOuter]], method = 'range')
    trainDataOuter[[i_kOuter]] = predict(preProcessValues,trainDataOuter[[i_kOuter]])
    testDataOuter[[i_kOuter]] = predict(preProcessValues,testDataOuter[[i_kOuter]])
    
    preProcessDummiedValues = preProcess(trainDummiedDataOuter[[i_kOuter]], method = 'range')
    trainDummiedDataOuter[[i_kOuter]] = predict(preProcessDummiedValues,trainDummiedDataOuter[[i_kOuter]])
    testDummiedDataOuter[[i_kOuter]] = predict(preProcessDummiedValues,testDummiedDataOuter[[i_kOuter]])
  }  
  return(list(trainDataOuter,testDataOuter,trainDummiedDataOuter,testDummiedDataOuter,trainClassOuter,testClassOuter))
}