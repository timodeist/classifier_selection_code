loadDataOberijeEtAl = function(pathFromDataFolderToCsv,pathToDataFolder)
{
  library(plyr) # used for revalue
  pathToFile = file.path(pathToDataFolder,pathFromDataFolderToCsv) # construct full file to csv
  data = read.csv(pathToFile,sep = ";", dec = ",") # read data from .csv
  
  # derive outcome (one-year survival)
  yearCutOff = 2
  data[,'survival_yearCutOff'] = NA # add column for the survival at year-cutoff
  nonSurvivors = data$survyear < yearCutOff & data$deadstat
  survivors = data$survyear >= yearCutOff
  data$survival_yearCutOff[nonSurvivors] = 0 # non-survivors at yearCutOff
  data$survival_yearCutOff[survivors] = 1 # survivors at yearCutOff
  
  # remove patients/rows without outcome
  data = data[complete.cases(data[,'survival_yearCutOff']),]
  
  # columns to drop, features
  drop = c('study_id','survmonth','survyear','deadstat')
  data = data[ , !(names(data) %in% drop)]
  
  # convert to factors
  factorCols = c('gender', 'who3g', 'dumsmok2', 't_ct_loc', 'hist4g', 'countpet_all6g', 'countpet_mediast6g',	'tstage',	'nstage',	'stage',	'timing', 'group',	'yearrt', 'survival_yearCutOff') # list all columns that should be factors
  data[factorCols] = lapply(data[factorCols], factor) # convert columns into factor variables
  
  data_class = data$survival_yearCutOff # place outcome in separate variable
  data$survival_yearCutOff = NULL #remove outcome from data
  data_class = revalue(data_class, c('0' = 'nonEvent','1' = 'event')) # relabel outcome as event and nonEvent: 0:dead before 2yr --> nonEvent, 1:alive after 2yr --> event
  return(list(data,data_class)) 
}