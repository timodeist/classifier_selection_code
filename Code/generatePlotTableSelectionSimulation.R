generatePlotTableSelectionSimulation = function(inputTable,aggOutputTable,classifierSelection,datasetSelection,kOuter,maxRep,mappingDatasetToPatientNumbers)
{
  # This function creates 3 data frames.
  # The first contains test set AUCs and test set AUC ranks for the classifier selection method 
  # (random, oracle, pre-selection, set-specific) simulation experiment (3rd analysis in paper). 
  # For each fold of each repetition of each dataset, four classifier selection methods are applied:
  # a) random classifier selection
  # b) oracle (selecting the classifier with the best AUC in the current test set)
  # c) pre-selection (selecting the classifier with the highest AUC rank of all other datasets)
  # d) set-specific selection (selecting the classifier with the highest inner-CV AUC, the inner-CV is done within the training set)
  # For each selected classifier, the corresponding AUCs and AUC ranks on the test set are placed in the first output data frame.
  # The second data frame contains the results of the first data frame aggregated by data sets.
  # The third data frame contains Wilcoxon signed rank test results to test significant improvements in AUC by methods c) and d) over a).

  library(plyr)
  plotTableSelectionSimulation = data.frame(dataset = character(),
                      rep = numeric(),
                      outerFold = numeric(),
                      randomClassifier = character(),
                      randomAuc = numeric(),
                      randomRankAuc = numeric(),
                      oracleClassifier = character(),
                      oracleAuc = numeric(),
                      oracleRankAuc = numeric(),
                      oracleImprovement = numeric(),
                      oracleRankImprovement = numeric(),
                      setSpecificClassifier = character(),
                      setSpecificAuc = numeric(),
                      setSpecificRankAuc = numeric(),
                      setSpecificImprovement = numeric(),
                      setSpecificRankImprovement = numeric(),
                      preSelectedClassifier = character(),
                      preSelectedAuc = numeric(),
                      preSelectedRankAuc = numeric(),
                      preSelectedImprovement = numeric(),
                      preSelectedRankImprovement = numeric()
  )
  
  preSelectedClassifierNameDf = data.frame(dataset = character(), preSelectedClassifier = character())
  
  for (i_dataset in datasetSelection) # loop through each dataset
  {
    # Note: the pre-selected classifier is chosen once per dataset before looping through all folds and repetitions
    
    # select all datasets except the one used in this loop (i.e., all sets except i_dataset)
    otherDatasets = setdiff(datasetSelection,i_dataset)
    
    # compute mean auc and mean rankCvAuc per classifier on the 'other datasets'
    meanAucPerClassifier = aggregate(aggOutputTable[aggOutputTable$dataset %in% otherDatasets,c('auc','rankCvAuc')],
                                          by = list('classifier' = aggOutputTable[aggOutputTable$dataset %in% otherDatasets,'classifier']),
                                          FUN = mean)
    
    # select the classifier with the lowest mean rankCvAuc on the 'other datasets'
    preSelectedClassifierName = meanAucPerClassifier$classifier[which.min(meanAucPerClassifier$rankCvAuc)]
    
    # add name of selected classifier to data frame (for merging with the aggregation later on) 
    preSelectedClassifierNameRow = data.frame(dataset = i_dataset, preSelectedClassifier = preSelectedClassifierName)
    preSelectedClassifierNameDf = rbind(preSelectedClassifierNameDf,preSelectedClassifierNameRow)
    
    for (i_rep in 1:maxRep) # loop through each repetition
    {
      for (i_kOuter in 1:kOuter) # loop through each fold
      {
        # Note: random, oracle, set-specific classifiers are chosen once per fold/repetition/dataset instance
        
        # select rows corresponding to current fold/repetition/dataset instance
        curInd = (inputTable$dataset == i_dataset & inputTable$rep == i_rep & inputTable$outerFold == i_kOuter)
        # create smaller table for convenience
        curTable = inputTable[curInd,]
        
        # random classifier selection
        randomClassifierName = sample(curTable$classifier,1)
        randomAuc = curTable[(curTable$classifier == randomClassifierName),'auc']
        randomRankAuc = curTable[(curTable$classifier == randomClassifierName),'rankAuc']
        
        # oracle classifier selection
        oracleClassifierName = curTable[which.max(curTable$auc),'classifier']
        oracleAuc = curTable[which.max(curTable$auc),'auc']
        oracleRankAuc = curTable[which.max(curTable$auc),'rankAuc']
        oracleImprovement = oracleAuc - randomAuc
        oracleRankImprovement = oracleRankAuc - randomRankAuc
        
        # set-specific classifier selection
        setSpecificClassifierName = curTable[which.max(curTable$innerCvAuc),'classifier']
        setSpecificAuc = curTable[which.max(curTable$innerCvAuc),'auc']
        setSpecificRankAuc = curTable[which.max(curTable$innerCvAuc),'rankAuc']
        setSpecificImprovement = setSpecificAuc - randomAuc
        setSpecificRankImprovement = setSpecificRankAuc - randomRankAuc
        
        # pre-selected classifier selection (reusing choice made earlier outside this loop)
        preSelectedClassifierName = preSelectedClassifierName
        preSelectedAuc = curTable[(curTable$classifier == preSelectedClassifierName),'auc']
        preSelectedRankAuc = curTable[(curTable$classifier == preSelectedClassifierName),'rankAuc']
        preSelectedImprovement = preSelectedAuc - randomAuc
        preSelectedRankImprovement = preSelectedRankAuc - randomRankAuc
        
        # place all those variables in a data frame row (note that the column labels are the same as the variable names)
        newRow = data.frame(dataset = i_dataset,
                            rep = i_rep,
                            outerFold = i_kOuter,
                            randomClassifier = randomClassifierName,
                            randomAuc = randomAuc,
                            randomRankAuc = randomRankAuc,
                            oracleClassifier = oracleClassifierName,
                            oracleAuc = oracleAuc,
                            oracleRankAuc = oracleRankAuc,
                            oracleImprovement = oracleImprovement,
                            oracleRankImprovement = oracleRankImprovement,
                            setSpecificClassifier = setSpecificClassifierName,
                            setSpecificAuc = setSpecificAuc,
                            setSpecificRankAuc = setSpecificRankAuc,
                            setSpecificImprovement = setSpecificImprovement,
                            setSpecificRankImprovement = setSpecificRankImprovement,
                            preSelectedClassifier = preSelectedClassifierName,
                            preSelectedAuc = preSelectedAuc,
                            preSelectedRankAuc = preSelectedRankAuc,
                            preSelectedImprovement = preSelectedImprovement,
                            preSelectedRankImprovement = preSelectedRankImprovement
                            )

        # append row to output
        plotTableSelectionSimulation = rbind(plotTableSelectionSimulation,newRow)
        
      }
    }
  }
  
  # test significance of improvement (setSpecific vs random, preSelected vs random)
  wilcoxResults = data.frame(preSelectedAucWilcoxP = numeric(),
                             preSelectedAucWilcoxReject = logical(),
                             setSpecificAucWilcoxP = numeric(),
                             setSpecificAucWilcoxReject = logical()
  )
  wilcoxResult_preSelected = wilcox.test(x = plotTableSelectionSimulation$preSelectedAuc, y = plotTableSelectionSimulation$randomAuc, paired = TRUE, alternative = 'greater') 
  wilcoxResults[1,'preSelectedAucWilcoxP'] = wilcoxResult_preSelected$p.value
  wilcoxResults[1,'preSelectedAucWilcoxReject'] = wilcoxResult_preSelected$p.value < 0.05
  
  wilcoxResult_setSpecific = wilcox.test(x = plotTableSelectionSimulation$setSpecificAuc, y = plotTableSelectionSimulation$randomAuc, paired = TRUE, alternative = 'greater') 
  wilcoxResults[1,'setSpecificAucWilcoxP'] = wilcoxResult_setSpecific$p.value
  wilcoxResults[1,'setSpecificAucWilcoxReject'] = wilcoxResult_setSpecific$p.value < 0.05
  
  # code dataset variable as factor
  plotTableSelectionSimulation$dataset = as.factor(plotTableSelectionSimulation$dataset)
  preSelectedClassifierNameDf$dataset = as.factor(preSelectedClassifierNameDf$dataset)
  
  ### aggregation of selection simulation results over repetitions and folds
  
  # aggregate results over repetitions and folds by computing the mean for some variables
  plotTableSelectionSimulation_aggRepsFolds_mean = aggregate(plotTableSelectionSimulation[,c('randomAuc','randomRankAuc','oracleAuc','oracleRankAuc','oracleImprovement','oracleRankImprovement','setSpecificAuc','setSpecificRankAuc','setSpecificImprovement','setSpecificRankImprovement','preSelectedAuc','preSelectedRankAuc','preSelectedImprovement','preSelectedRankImprovement')], 
            by = list('dataset' = plotTableSelectionSimulation$dataset),FUN = mean
            )
  
  # aggregate results over repetitions and folds by computing the variance for some variables
  plotTableSelectionSimulation_aggRepsFolds_var = aggregate(plotTableSelectionSimulation[,c('randomAuc','randomRankAuc','oracleAuc','oracleRankAuc','oracleImprovement','oracleRankImprovement','setSpecificAuc','setSpecificRankAuc','setSpecificImprovement','setSpecificRankImprovement','preSelectedAuc','preSelectedRankAuc','preSelectedImprovement','preSelectedRankImprovement')], 
                                  by = list('dataset' = plotTableSelectionSimulation$dataset),FUN = var
                                  )
  
  # merge the mean and variance data frames (columns from the mean-data frame get the suffix '_mean', and '_var' for the variance-data frame columns)                      
  plotTableSelectionSimulation_aggRepsFolds_intermediate = merge(plotTableSelectionSimulation_aggRepsFolds_mean,plotTableSelectionSimulation_aggRepsFolds_var, by = 'dataset', suffixes = c('_mean','_var'))
  
  # add classifier names of the pre-selection by merging the aggregated data frame and the data frame containing the classifier names (preSelectedClassifierNameDf)                             
  plotTableSelectionSimulation_aggRepsFolds = merge(plotTableSelectionSimulation_aggRepsFolds_intermediate,preSelectedClassifierNameDf, by = 'dataset')
  
  
  # multiply all improvements by -1 so that the rank improvement is positive and rank worsening is negative
  plotTableSelectionSimulation$oracleRankImprovement = -1 * plotTableSelectionSimulation$oracleRankImprovement
  plotTableSelectionSimulation$setSpecificRankImprovement = -1 * plotTableSelectionSimulation$setSpecificRankImprovement
  plotTableSelectionSimulation$preSelectedRankImprovement = -1 * plotTableSelectionSimulation$preSelectedRankImprovement
  
  plotTableSelectionSimulation_aggRepsFolds$oracleRankImprovement_mean = -1 * plotTableSelectionSimulation_aggRepsFolds$oracleRankImprovement_mean
  plotTableSelectionSimulation_aggRepsFolds$setSpecificRankImprovement_mean = -1 * plotTableSelectionSimulation_aggRepsFolds$setSpecificRankImprovement_mean
  plotTableSelectionSimulation_aggRepsFolds$preSelectedRankImprovement_mean = -1 * plotTableSelectionSimulation_aggRepsFolds$preSelectedRankImprovement_mean
  
  ### add mean and weighted mean rows to table
  # create vector of weights by replacing dataset names by patient numbers
  rowWeights = revalue(plotTableSelectionSimulation_aggRepsFolds$dataset,mappingDatasetToPatientNumbers) # weights based on 
  # cast as integers
  rowWeights = as.integer(as.character(rowWeights))
  # compute weighted column means
  weightedRow = lapply(plotTableSelectionSimulation_aggRepsFolds, weighted.mean, w = rowWeights) # returns 2 warnings because it tries to weighted.mean() string columns, it returns NA for those columns, which is later replaced by sensible labels
  cat('returns 2 warnings because it tries to weigthed.mean() string columns, it returns NA for those columns, which is later replaced by sensible labels')
  # compute column means
  averageRow = lapply(plotTableSelectionSimulation_aggRepsFolds, mean) # returns 2 warnings because it tries to mean() string columns, it returns NA for those columns, which is later replaced by sensible labels
  cat('returns 2 warnings because it tries to mean() string columns, it returns NA for those columns, which is later replaced by sensible labels')
  # abuse dataset entry as label for computation
  weightedRow$dataset = 'WeightedMean'
  averageRow$dataset = 'Mean'
  # add the weighted mean and mean rows to the plotTable
  levels(plotTableSelectionSimulation_aggRepsFolds$dataset) = c(levels(plotTableSelectionSimulation_aggRepsFolds$dataset),"Mean","WeightedMean") # expand the dataset factor levels with Mean/WeightedMean, otherwise NAs are forced
  plotTableSelectionSimulation_aggRepsFolds = rbind(plotTableSelectionSimulation_aggRepsFolds,averageRow)
  plotTableSelectionSimulation_aggRepsFolds = rbind(plotTableSelectionSimulation_aggRepsFolds,weightedRow)
  # fix the ordering based on the preferred classifier and dataset order (classifierSelection and datasetSelection have this correct order)
  plotTableSelectionSimulation_aggRepsFolds = plotTableSelectionSimulation_aggRepsFolds[order(match(plotTableSelectionSimulation_aggRepsFolds$dataset,datasetSelection)),]
  
  return(list(plotTableSelectionSimulation,plotTableSelectionSimulation_aggRepsFolds,wilcoxResults))
}
  