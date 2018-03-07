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
    fitControl$search = "random"
    tuneLengthSize = 25
    MaxWeights = 5000
    modelFit <- train(x = trainData, y = trainClass, method = 'nnet' , trControl = fitControl, metric = 'ROC', tuneLength = tuneLengthSize, trace = FALSE, MaxNWts = MaxWeights) # added trace = FALSE to suppress console output
  }
  return(modelFit)
}