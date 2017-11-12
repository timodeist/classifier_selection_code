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
    myTuneGrid <- expand.grid(mtry = seq(2,29,3)) # set a range for the mtry parameter to on which caret should tune
    modelFit <- train(x = trainData, y = trainClass, method = 'rf' , trControl = fitControl, metric = 'ROC', ntree = 500, tuneGrid = myTuneGrid) 
  }
  return(modelFit)
}