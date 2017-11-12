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
    # rpartControl <-  rpart.control(minsplit = 3) # if you want to adjust some parameters manually
    # myTuneGrid <- expand.grid()
    modelFit <- train(x = trainData, y = trainClass, method = 'rpart' , trControl = fitControl, metric = 'ROC', tuneGrid = myTuneGrid, trials = 1)
  }
  return(modelFit)
}