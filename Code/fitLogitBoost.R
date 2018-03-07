fitLogitBoost <- function(trainData,trainClass,mySeed,fitControl,defaultTuning)
{
  library(caTools)
  set.seed(mySeed)
  if (defaultTuning)
  {
    modelFit <- train(x = trainData, y = trainClass, method = 'LogitBoost' , trControl = fitControl, metric = 'ROC')
  }
  else
  {
    fitControl$search = "random"
    tuneLengthSize = 25
    modelFit <- train(x = trainData, y = trainClass, method = 'LogitBoost' , trControl = fitControl, metric = 'ROC', tuneLength = tuneLengthSize)
  }
  return(modelFit)
}