addAucRanks = function(outputTable,classifierSelection,datasetSelection,maxRep,kOuter)
{
  # computes ranks for results per fold  
  for (i_rep in 1:maxRep) # loop through each repetition
  {
    for (i_dataset in datasetSelection) # loop through each dataset
    {
      for (i_kOuter in 1:kOuter) # loop through each fold
      {
        # compute the indices of all rows that contain output from the same rep, dataset, (sum(curInd) should be equal to classifierSelection)
        curInd = (outputTable$rep == i_rep & outputTable$dataset == i_dataset & outputTable$outerFold == i_kOuter)
        if (sum(curInd) != length(classifierSelection))
        {
          stop('Something is wrong.')
        }
        curAucs = outputTable[curInd,'auc']
        outputTable[curInd,'rankAuc'] = rank(-curAucs, ties.method = 'random')
      }
    }
  }
  return(outputTable)
}
