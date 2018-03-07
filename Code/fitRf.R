fitRf <- function(trainData,trainClass,mySeed,fitControl,defaultTuning)
{
  library(randomForest)
  set.seed(mySeed)
  if (defaultTuning)
  {
    modelFit <- train(x = trainData, y = trainClass, method = 'rf' , trControl = fitControl, metric = 'ROC')
  }
  else
  {
    fitControl$search = "random"
    tuneLengthSize = 25
    modelFit <- train(x = trainData, y = trainClass, method = 'rf' , trControl = fitControl, metric = 'ROC', tuneLength = tuneLengthSize)
  }
  return(modelFit)
}