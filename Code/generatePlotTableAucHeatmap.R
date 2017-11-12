generatePlotTableAucHeatmap = function(outputTable_aggRepsFolds,classifierSelection,datasetSelection)
{
  # select 4 columns from outputTable_aggRepsFolds and place in new data frame
  plotTable = outputTable_aggRepsFolds[,c('dataset','classifier','auc','rankCvAuc')]
  plotTable$classifier = as.factor(outputTable_aggRepsFolds$classifier)
  plotTable$dataset = as.factor(outputTable_aggRepsFolds$dataset)
  
  # force order of datasets and classifiers (classifierSelection and datasetSelection have this correct order)
  plotTable$classifier = factor(plotTable$classifier, levels = rev(classifierSelection)) # the y-axis needs to have the reversed order
  plotTable$dataset = factor(plotTable$dataset, levels = datasetSelection)
  
  # create strings of Auc and rankCvAuc with 2 decimal precision
  plotTable$displayTextAuc = sprintf('%.2f',plotTable$auc)
  plotTable$displayTextRankCvAuc = sprintf('%.2f',plotTable$rankCvAuc)
  
  return(plotTable)
}