generateAppendixStatsTable = function(aggOutputTable_aggFolds,classifierSelection)
{
  # generates a table with means (!) of all statistics per classifier
  
  names(aggOutputTable_aggFolds)[names(aggOutputTable_aggFolds) == 'auc'] = 'AUC'
  names(aggOutputTable_aggFolds)[names(aggOutputTable_aggFolds) == 'rankCvAuc'] = 'AUC rank'
  names(aggOutputTable_aggFolds)[names(aggOutputTable_aggFolds) == 'brierScore'] = 'Brier score'
  names(aggOutputTable_aggFolds)[names(aggOutputTable_aggFolds) == 'calibrationIntercept'] = 'Cal. intercept'
  names(aggOutputTable_aggFolds)[names(aggOutputTable_aggFolds) == 'calibrationSlope'] = 'Cal. slope'
  names(aggOutputTable_aggFolds)[names(aggOutputTable_aggFolds) == 'myAccuracy'] = 'Accuracy'
  names(aggOutputTable_aggFolds)[names(aggOutputTable_aggFolds) == 'myKappa'] = 'Cohen\'s kappa'
  
  colSelection = c('AUC','Brier score','Accuracy','Cohen\'s kappa','Cal. intercept','Cal. slope')
  # aggregate over datasets
  statsTable = aggregate(aggOutputTable_aggFolds[,colSelection], 
                                           by = list('classifier' = aggOutputTable_aggFolds$classifier),
                                           FUN = function(myInput){median(myInput,na.rm = TRUE)})  
  
  # fix the ordering based on the preferred classifier order (classifierSelection has this correct order)
  statsTable = statsTable[order(match(statsTable$classifier,classifierSelection)),]
  
  return(statsTable)
}