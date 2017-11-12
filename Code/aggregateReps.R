aggregateReps = function(aggOutputTable,classifierSelection,datasetSelection,kOuter,maxRep)
{
  # select columns to be mean())'ed or mean(abs())'ed
  colSelection_mean = c('auc','rankAuc','rankCvAuc','brierScore','myAccuracy','myKappa','calibrationIntercept','calibrationSlope')

  # aggregate some columns by mean() over all repetitions & folds   
  aggTable = aggregate(aggOutputTable[,colSelection_mean], 
                            by = list('dataset' = aggOutputTable$dataset,'classifier' = aggOutputTable$classifier),
                            FUN = mean)
  
  # fix the ordering based on the preferred classifier and dataset order (classifierSelection and datasetSelection have this correct order)
  aggTable = aggTable[order(match(aggTable$dataset,datasetSelection),match(aggTable$classifier,classifierSelection)),]
  
  return(aggTable)
}
