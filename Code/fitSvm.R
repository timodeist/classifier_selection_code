fitSvm <- function(trainData,trainClass,mySeed,fitControl,defaultTuning)
{
  library(kernlab)
  set.seed(mySeed)
  if (defaultTuning)
  {
    modelFit <- train(x = trainData, y = trainClass, method = 'svmRadial' , trControl = fitControl, metric = 'ROC', scale = FALSE) 
  }
  else
  {
    myTuneGrid <- expand.grid(C = c(2^(-2), 2^(-1), 1, 2, 4),sigma = c(10^(-2), 10^(-1), 1, 10, 100))
    modelFit <- train(x = trainData, y = trainClass, method = 'svmRadial' , trControl = fitControl, metric = 'ROC', tuneGrid = myTuneGrid, scale = FALSE) 
  }
  return(modelFit)
}