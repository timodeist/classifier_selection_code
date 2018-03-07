addCvAucRanks = function(aggOutputTable,classifierSelection,datasetSelection,reps)
{
  for (i_rep in reps) # loop through each repetition
  {
    for (i_dataset in datasetSelection) # loop through each dataset
    {
      # compute the indices of all rows that contain output from the same rep, dataset, (sum(curInd) should be equal to classifierSelection)
      curInd = (aggOutputTable$rep == i_rep & aggOutputTable$dataset == i_dataset)
      if (sum(curInd) != length(classifierSelection))
      {
        stop('Likely, computeRanksInTable() does not receive a table which is averaged over outer folds.')
      }
      curAucs = aggOutputTable[curInd,'auc']
      aggOutputTable[curInd,'rankCvAuc'] = rank(-curAucs, ties.method = 'random')
    }
  }
  return(aggOutputTable)
}
