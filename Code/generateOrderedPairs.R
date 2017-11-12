generateOrderedPairs = function(inputTable,classifierSelection,datasetSelection,maxRep)
 {
  # This function creates a data frame with rows of ordered pairs of classifiers and their AUC aggregated over folds per dataset & repetition.
  # An example: Imagine we have 100 repetitions, 9 datasets, 6 classifiers. Data was aggregated over 5 folds (irrelevant here).
  # Per classifier we have 100 reps * 9 sets = 900 AUC values. We have 6 classifiers, so we have 900 * 6 = 5400 AUC values in total. 
  # Each of these AUC values of a classifer is compared to the AUC value of another classifier (including the same classifier)
  # with the same randomization seeds. So we have 5400 AUC values, each compared to 6 other AUC values, so 5400 * 6 = 32400 pairs.

  # initialize data frame
  orderedPairsTable = data.frame(rep = numeric(),
                         dataset = character(),
                         classifierOne = character(),
                         classifierTwo = character(),
                         diffAuc = numeric(),
                         aucOneGreaterAucTwo = numeric()
                         )
  
  for (i_rep in 1:maxRep) # loop through each repetition
  {
    for (i_dataset in datasetSelection) # loop through each dataset
    {
      for (i_classifierOne in classifierSelection) # loop through each classifier (first)
      {
        for (i_classifierTwo in classifierSelection) # loop through each classifier (second)
        {
          # find all indices that concern classifier i_classifierOne in dataset i_dataset in rep i_rep
          curIndOne = (inputTable$rep == i_rep & inputTable$dataset == i_dataset & inputTable$classifier == i_classifierOne)
          # find all indices that concern classifier i_classifierTwo in dataset i_dataset in rep i_rep
          curIndTwo = (inputTable$rep == i_rep & inputTable$dataset == i_dataset & inputTable$classifier == i_classifierTwo)
          # create new row that stores the AUC of each classifier, their difference, whether classifier i_classifierOne has an increased AUC
          newRow = data.frame(rep = i_rep,
                              dataset = i_dataset,
                              classifierOne = i_classifierOne,
                              classifierTwo = i_classifierTwo,
                              meanAucOne = inputTable[curIndOne,'auc'],
                              meanAucTwo = inputTable[curIndTwo,'auc'],
                              diffAuc = inputTable[curIndOne,'auc'] - inputTable[curIndTwo,'auc'],
                              aucOneGreaterAucTwo = inputTable[curIndOne,'auc'] > inputTable[curIndTwo,'auc']
          )
          # add row to table
          orderedPairsTable = rbind(orderedPairsTable,newRow)
        }
      }
    }
  }
  
   return(orderedPairsTable)
 }