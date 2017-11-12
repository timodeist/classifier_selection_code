aggregateFolds = function(outputTable,classifierSelection,datasetSelection,kOuter,maxRep)
{
  # select columns to be mean())'ed or mean(abs())'ed
  colSelection_mean = c('auc','rankAuc','brierScore','myAccuracy','myKappa')
  colSelection_meanAbs = c('calibrationIntercept')
  colSelection_meanAbsMinusOne = c('calibrationSlope')
  
  # aggregate some columns by mean() over all folds   
  aggTable_mean = aggregate(outputTable[,colSelection_mean], 
                                           by = list('dataset' = outputTable$dataset, 'rep' = outputTable$rep ,'classifier' = outputTable$classifier),
                                           FUN = mean)
  # aggregate calibration intercept by mean(abs(x))  over all folds   
  aggTable_meanAbs = aggregate(outputTable[,colSelection_meanAbs],
                                              by = list('dataset' = outputTable$dataset, 'rep' = outputTable$rep ,'classifier' = outputTable$classifier),
                                              FUN = function(x){mean(abs(x))})
  # rename the column to correct name (when only one variable is aggregated, the object becomes a list which ignores variable names)
  names(aggTable_meanAbs)[4] = colSelection_meanAbs
  
  # aggregate calibration slope by mean(abs(x-1)) over all folds
  aggTable_meanAbsMinusOne = aggregate(outputTable[,colSelection_meanAbsMinusOne],
                                                      by = list('dataset' = outputTable$dataset, 'rep' = outputTable$rep,'classifier' = outputTable$classifier),
                                                      FUN = function(x){mean(abs(x-1))})
  # rename the column to correct name (when only one variable is aggregated, the object becomes a list which ignores variable names)
  names(aggTable_meanAbsMinusOne)[4] = colSelection_meanAbsMinusOne
  
  # merge the first two data frames
  intermediateMerge = merge(aggTable_mean,
                            aggTable_meanAbs, 
                            by = c('dataset', 'rep','classifier'))
  # merge the intermediate data frame with the third data frame
  aggTable = merge(intermediateMerge,
                                  aggTable_meanAbsMinusOne, 
                                  by = c('dataset', 'rep','classifier'))
  
  # fix the ordering based on the preferred classifier and dataset order (classifierSelection and datasetSelection have this correct order)
  aggTable = aggTable[order(aggTable$rep,match(aggTable$dataset,datasetSelection),match(aggTable$classifier,classifierSelection)),]
  
  
  return(aggTable)
}
