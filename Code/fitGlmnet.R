fitGlmnet <- function(trainData,trainClass,mySeed,fitControl,defaultTuning)
{
  library(glmnet)
  set.seed(mySeed)
  if (defaultTuning)
  {
    modelFit <- train(x = trainData, y = trainClass, method = 'glmnet' , trControl = fitControl, metric = 'ROC')
  }
  else
  {
    #myTuneGrid <- expand.grid()
    modelFit <- train(x = trainData, y = trainClass, method = 'glmnet' , trControl = fitControl, metric = 'ROC', tuneGrid = myTuneGrid)
  }
  return(modelFit)
}