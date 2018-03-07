fitRpart <- function(trainData,trainClass,mySeed,fitControl,defaultTuning)
{
  library(rpart)
  set.seed(mySeed)
  if (defaultTuning)
  {
    modelFit <- train(x = trainData, y = trainClass, method = 'rpart' , trControl = fitControl, metric = 'ROC')
  }
  else
  {
    fitControl$search = "random"
    tuneLengthSize = 25
    modelFit <- train(x = trainData, y = trainClass, method = 'rpart' , trControl = fitControl, metric = 'ROC', tuneLength = tuneLengthSize)
  }
  return(modelFit)
}