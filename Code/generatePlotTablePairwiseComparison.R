generatePlotTablePairwiseComparison = function(orderedPairsTable,classifierSelection,datasetSelection,testOn)
{
  # This function generates a data frame for plotting the pairwise comparison. 
  # An example: Imagine we have 100 repetitions, 9 datasets, 6 classifiers.
  # For each of the combination of classifiers (6*6=36) we select 32400/36=900 rows in orderedPairsTable 
  # with this combinatin of classifiers.
  # Each row contains the AUC values for classifier 1 and 2, and we do a Wilcoxon signed rank pairwise comparison 
  # of these 900 paired AUC values to get one p-value. We end up with a data frame that is 6*6=36 long. 
  # We don't consider cases where classifiers are compared to themselves, and cases where the first classifier
  # performs worse than the second classifier (redundant to the opposite case). We apply a Holm-Bonferroni
  # correction on the remaining cases (36-6-15=15).
  
  wilcoxList = list() # to track statistical tests
  
  # initialize data frame
  pairCompPlotTable = data.frame(classifierOne = character(),
                                 classifierTwo = character(),
                                 countAucOneGreaterAucTwo = numeric(),
                                 wilcoxP = numeric(),
                                 wilcoxReject = logical(),
                                 wilcoxPAdj = numeric(),
                                 wilcoxrejectAdj = logical(),
                                 boxColor = character(),
                                 displayText = character()
  )
  for (i_classifierOne in classifierSelection) # loop through each classifier (first)
  {
    wilcoxSublist = list() # to track statistical tests
    for (i_classifierTwo in classifierSelection) # loop through each classifier (second)
    {
      # find indices of all rows for pairs with classifier i_classifierOne vs classifier i_classifierTwo (i_classifierOne in first position, i_classifierTwo in second position)
      curInd = (orderedPairsTable$classifierOne == i_classifierOne & orderedPairsTable$classifierTwo == i_classifierTwo)
      # select the meanAuc columns of classifiers i_classifierOne and i_classifierTwo
      myX = orderedPairsTable[curInd,'meanAucOne']
      myY = orderedPairsTable[curInd,'meanAucTwo']
      
      # do wilcoxon signed rank test
      wilcoxResult = wilcox.test(x = myX, y = myY,paired = TRUE, alternative = 'greater') # when two classifiers have the same AUC, a warning occurs
      # store result in new row
      newRow = data.frame(classifierOne = i_classifierOne, 
                          classifierTwo = i_classifierTwo, 
                          wilcoxP = wilcoxResult$p.value, 
                          wilcoxReject = wilcoxResult$p.value < 0.05,
                          countAucOneGreaterAucTwo = sum(myX>myY)/length(myX)
      )
      pairCompPlotTable = rbind(pairCompPlotTable, newRow)
      # store test-object in list
      wilcoxSublist[[i_classifierTwo]] = wilcoxResult
    }
    wilcoxList[[i_classifierOne]] = wilcoxSublist # to track statistical tests
  }
  
  ### apply Holm-Bonferroni correction for pairs where AUC of i_classifierOne > AUC of i_classifierTwo in more than 50% of the times
  cat('It reports Wilcoxon warnings for comparisons of a classifier to itself. This number of warnings should be equal to the number of classifiers in the analysis. Otherwise, the warning might have a different origin.')
  # initalize columns with NA
  pairCompPlotTable$wilcoxPAdj = NA
  # test only the pairs with more than 0.5 better for classifierOne - Holm-Bonferroni correction
  pairCompPlotTable[(pairCompPlotTable$countAucOneGreaterAucTwo > 0.5),'wilcoxPAdj'] = p.adjust(pairCompPlotTable[(pairCompPlotTable$countAucOneGreaterAucTwo > 0.5),'wilcoxP'], method = 'holm')
  # check for significance (NAs stay NA)
  pairCompPlotTable$wilcoxRejectAdj = pairCompPlotTable$wilcoxPAdj < 0.05
  
  ### create variables for plotting
  
  ## colors
  
  # initialize as 'not tested'
  pairCompPlotTable$boxColor = 'not tested'
  if (testOn)
  {
    # all cases which do not have NA as rejection decision and do have TRUE as rejection decision are 'significant'
    pairCompPlotTable[(!is.na(pairCompPlotTable$wilcoxRejectAdj) & pairCompPlotTable$wilcoxRejectAdj == 'TRUE'),'boxColor'] = 'significant'
    # all cases which do not have NA as rejection decision and do have FALSE as rejection decision are 'insignificant'
    pairCompPlotTable[(!is.na(pairCompPlotTable$wilcoxRejectAdj) & pairCompPlotTable$wilcoxRejectAdj == 'FALSE'),'boxColor'] = 'insignificant'
    # code variable as factor
    pairCompPlotTable$boxColor = as.factor(pairCompPlotTable$boxColor)
    # reorder factors for better looks
    pairCompPlotTable$boxColor = factor(pairCompPlotTable$boxColor, levels = c('not tested','insignificant','significant'))
  }
  
  ## display text
  # assign the increased AUC percentage as character
  # pairCompPlotTable$displayText = as.character(pairCompPlotTable$countAucOneGreaterAucTwo) # if you want decimals
  pairCompPlotTable$displayText = paste(as.character(100 * round(pairCompPlotTable$countAucOneGreaterAucTwo,2)),'%', sep = '') # if you want percentages
  # delete text for cases < 0.5
  pairCompPlotTable[pairCompPlotTable$countAucOneGreaterAucTwo < 0.5,'displayText'] = ''
  
  # fix the ordering based on the preferred classifier order (classifierSelection has this correct order)
  pairCompPlotTable$classifierOne = factor(pairCompPlotTable$classifierOne, levels = rev(classifierSelection)) #the y-axis needs to be reversed
  pairCompPlotTable$classifierTwo = factor(pairCompPlotTable$classifierTwo, levels = classifierSelection)
  
  return(list(pairCompPlotTable,wilcoxList))
}