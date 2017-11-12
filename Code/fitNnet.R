fitNnet <- function(trainData,trainClass,mySeed,fitControl,defaultTuning)
{
  library(nnet)
  set.seed(mySeed)
  if (defaultTuning)
  {
    modelFit <- train(x = trainData, y = trainClass, method = 'nnet' , trControl = fitControl, metric = 'ROC', trace = FALSE) # added trace = FALSE to suppress console output
  }
  else
  {
    myTuneGrid <- expand.grid(size = seq(1,9,2), decay = c(0, 0.1, 0.01, 0.001, 0.0001))
    modelFit <- train(x = trainData, y = trainClass, method = 'nnet' , trControl = fitControl, metric = 'ROC', tuneGrid = myTuneGrid, trace = FALSE) # added trace = FALSE to suppress console output
  }
  return(modelFit)
}