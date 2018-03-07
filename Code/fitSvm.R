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
    fitControl$search = "random"
    tuneLengthSize = 25
    modelFit <- train(x = trainData, y = trainClass, method = 'svmRadial' , trControl = fitControl, metric = 'ROC', scale = FALSE, tuneLength = tuneLengthSize) 
  }
  return(modelFit)
}