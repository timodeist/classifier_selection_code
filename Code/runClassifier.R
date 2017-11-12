runClassifier = function(classifierName,trainData,trainClass,testData,testClass,kInner,defaultTuning,mySeed)
{
  # This function fits one classifier on the current training set using a kInner-fold inner CV (using caret),
  # hyperparameters are tuned in the inner CV,
  # evaluates the model on the test set,
  # computes metrics on test set.
  
  library(ResourceSelection) # used for Hosmer-Lemeshow test
  
  debugSource('fitRf.R')
  debugSource('fitGlmnet.R')
  debugSource('fitNnet.R')
  debugSource('fitSvm.R')
  debugSource('fitLogitBoost.R')
  debugSource('fitRpart.R')
  
  # set fitControl for all classifiers.
  fitControl = trainControl( method = 'cv', number = kInner, classProbs = TRUE, allowParallel = FALSE, summaryFunction = twoClassSummary, verboseIter = FALSE)

  # given the classifierName, fit the correct model
    modelFit = switch(classifierName,
                       rf = fitRf(trainData,trainClass,mySeed,fitControl,defaultTuning),
                       nnet = fitNnet(trainData,trainClass,mySeed,fitControl,defaultTuning),
                       svm = fitSvm(trainData,trainClass,mySeed,fitControl,defaultTuning),
                       glmnet = fitGlmnet(trainData,trainClass,mySeed,fitControl,defaultTuning),
                       LogitBoost = fitLogitBoost(trainData,trainClass,mySeed,fitControl,defaultTuning),
                       rpart = fitRpart(trainData,trainClass,mySeed,fitControl,defaultTuning))
    
    # retrieve the highest AUC achieved by a hyperparameter-combination in the inner-CV. This should be by the model selected by the inner-CV.
    innerCvAuc = max(modelFit$results$ROC)
    
    # get AUCs via caret::twoClassSummary
    testDf = as.data.frame(predict(modelFit,newdata = testData, type = 'prob')) # create data frame with probabilities for 'event' and 'nonEvent'
    # NOTE: the columns 'event' and 'nonEvent' in testDf are probabilities for those classes! Names cannot be changed because these names are required by twoClassSummary()
    testDf$obs = testClass # assign the true test classes as observations
    testDf$pred = predict(modelFit,newdata = testData) # assign predictions (event or nonEvent) as pred (they are either event or nonEvent). Uses 0.5-cutoff
    testPerformance = twoClassSummary(data = testDf,lev = levels(testDf$obs),model = modelFit$method) # compute statistics for test set
    auc = testPerformance[1]
    cat(sprintf('AUC=%.2f\n', auc))
    
    # calibration
    testDf$numericObs = ifelse(testDf$obs == 'event',1,0) # code event as 1 and nonEvent as 0 for regression
    calibrationRegression = lm(numericObs ~ event, data = testDf)
    calibrationIntercept = calibrationRegression$coefficients[1]
    calibrationSlope = calibrationRegression$coefficients[2] 
    
    # mean squared error or Brier score
    brierScore = mean((testDf$numericObs - testDf$event)^2)
    
    # Hosmer-Lemeshow GOF test
    hlTest = hoslem.test(x = testDf$event,y = testDf$numericObs,g = 10)
    HosLemPvalue = hlTest$p.value
    
    # accuracy and kappa via caret::confusionMatrix
    cmOutput = caret::confusionMatrix(data = testDf$pred, reference = testDf$obs) # specify the use of confusionMatrix() from the caret function to avoid a conflict with package 'ModelMetrics'
    myAccuracy = cmOutput$overall['Accuracy']
    myKappa = cmOutput$overall['Kappa']
    
    # put statistics in a data frame
    statsDf = data.frame(innerCvAuc, auc, calibrationIntercept, calibrationSlope, brierScore, HosLemPvalue, myAccuracy, myKappa)
  
  # return the data frame with statistics and the fitted model (for saving purposes)
  return(list(statsDf,modelFit))
}