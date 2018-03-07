addAucRanks = function(outputTable,classifierSelection,datasetSelection,reps,outerFolds)
{
  # computes ranks for results per fold 
  for (i_rep in reps) # loop through each repetition
  {
    for (i_dataset in datasetSelection) # loop through each dataset
    {
      for (i_kOuter in outerFolds) # loop through each fold
      {
        # compute the indices of all rows that contain output from the same rep, dataset, (sum(curInd) should be equal to classifierSelection)
        curInd = (outputTable$rep == i_rep & outputTable$dataset == i_dataset & outputTable$outerFold == i_kOuter)
        if (sum(curInd) != length(classifierSelection))
        {
          stop('Something is wrong in addAucRanks')
        }
        curAucs = outputTable[curInd,'auc']
        outputTable[curInd,'rankAuc'] = rank(-curAucs, ties.method = 'random')
      }
    }
  }
  return(outputTable)
}
