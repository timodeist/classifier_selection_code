runCvForClassifiers = function(data,data_class,kOuter,kInner,classifierNames,defaultTuning,mySeed)
{
  # This function determines folds and creates train/test sets, 
  # preprocesses the sets,
  # loops over each classifier for each train/test set combination.
  
  library(caret) # for training/testing/everything
  
  # load all subfunctions (including debugging functionality)
  debugSource('runClassifier.R')
  debugSource('preprocess_dataset.R')
  
  # initalize modelFitList
  modelFitList = list(length = length(classifierNames))
  for (i_classifierNames in 1:length(classifierNames))
  {
    # initialize modelFitList which is a list of lists. One list for each classifier, each contains one entry for each fold.
    modelFitList[[i_classifierNames]] = list(length = kOuter)
  }
  
  # initialize outputTable
  outputTable = data.frame(outerFold = numeric(), classifier = character(), innerCvAuc = numeric(), auc = numeric(), calibrationIntercept = numeric(), calibrationSlope = numeric(), brierScore = numeric(), HosLemPvalue = numeric(), myAccuracy = numeric(), myKappa = numeric())
  
  # create outer folds
  set.seed(mySeed) # set seed for reproducibility of folds
  outerFolds = createFolds(y = data_class, k = kOuter) # create folds #NOTE: sometimes the folds are not stratified for the outcome if there are too few events or nonEvents
  
  # create variables containing indices for folds, create data frames containing the training and test data 
  preprocess_datasetOutput = preprocess_dataset(data,data_class,outerFolds,kOuter)
  trainDataOuter = preprocess_datasetOutput[[1]]
  testDataOuter = preprocess_datasetOutput[[2]]
  trainDummiedDataOuter = preprocess_datasetOutput[[3]]
  testDummiedDataOuter = preprocess_datasetOutput[[4]]
  trainClassOuter = preprocess_datasetOutput[[5]]
  testClassOuter = preprocess_datasetOutput[[6]]
  
  # loop over all folds (repeat analysis kOuter-many times)
  for (i_kOuter in 1:kOuter)
  {
    # print current iteration nr
    cat(sprintf('      Outer fold %d/%d\t\t\t\t\t\t\t\t prevalence=%g%%\t (%d/%d)\n', i_kOuter, kOuter, round(100*sum(trainClassOuter[[i_kOuter]] == 'event')/length(trainClassOuter[[i_kOuter]])), sum(trainClassOuter[[i_kOuter]] == 'event'), length(trainClassOuter[[i_kOuter]]))) # DEBUG
    
    # loop over each classifier
    for (i_classifierNames in 1:length(classifierNames))
    {
      # print current classifier to be run (newline is printed after AUC inside the runCLassifier function)
      cat(sprintf('        Classifier %d/%d\t %-10s\t', i_classifierNames, length(classifierNames), classifierNames[i_classifierNames]))
      
      # use dummied data for some classifiers
      if (!is.na(match(classifierNames[i_classifierNames],c('da','svm','pls','glmnet','glmnet_h2o','enet','plr','LogitBoost','knn'))))
      {
        # for glmnet check for near-zero variance columns
        if(classifierNames[i_classifierNames] == 'glmnet')
        {
          trainDummiedNonNearZeroVarianceDataOuter = trainDummiedDataOuter[[i_kOuter]]
          testDummiedNonNearZeroVarianceDataOuter = testDummiedDataOuter[[i_kOuter]]
          toRemove = nearZeroVar(trainDummiedNonNearZeroVarianceDataOuter)
          
          # if there are near-zero variance variables, remove them from train and test data
          if (length(toRemove)>0) 
            {
            trainDummiedNonNearZeroVarianceDataOuter = trainDummiedNonNearZeroVarianceDataOuter[,-toRemove]
            testDummiedNonNearZeroVarianceDataOuter = testDummiedNonNearZeroVarianceDataOuter[,-toRemove]  
          }
          
          runClassifierOutput = runClassifier(classifierNames[i_classifierNames],trainDummiedNonNearZeroVarianceDataOuter,trainClassOuter[[i_kOuter]],testDummiedNonNearZeroVarianceDataOuter,testClassOuter[[i_kOuter]],kInner,defaultTuning,mySeed)
        }
        else
        {
          runClassifierOutput = runClassifier(classifierNames[i_classifierNames],trainDummiedDataOuter[[i_kOuter]],trainClassOuter[[i_kOuter]],testDummiedDataOuter[[i_kOuter]],testClassOuter[[i_kOuter]],kInner,defaultTuning,mySeed)
        }        
      }
      else
      {
        runClassifierOutput = runClassifier(classifierNames[i_classifierNames],trainDataOuter[[i_kOuter]],trainClassOuter[[i_kOuter]],testDataOuter[[i_kOuter]],testClassOuter[[i_kOuter]],kInner,defaultTuning,mySeed)  
      }
      newRow = runClassifierOutput[[1]]
      newRow['classifier'] = classifierNames[i_classifierNames] # add classifier string to new result
      newRow['outerFold'] = i_kOuter # add outer fold number to new result
      outputTable = rbind(outputTable, newRow) # attach new result to outputTable
      modelFitList[[i_classifierNames]][[i_kOuter]]= runClassifierOutput[[2]] # store fitted model for current dataset/rep combination: store the fitted model for fold i_kOuter in the list for classifier i_classifierNames (in the i_kOuter-th position in the i_classifierNames-th list)
    }
  }
  return(list(outputTable,modelFitList))
}