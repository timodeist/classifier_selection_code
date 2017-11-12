loadDataOberijeEtAl = function(pathFromDataFolderToCsv,pathToDataFolder)
{
  library(plyr) # used for revalue
  pathToFile = file.path(pathToDataFolder,pathFromDataFolderToCsv) # construct full file to csv
  data = read.csv(pathToFile,sep = ";", dec = ",") # read data from .csv
  
  # columns to drop, features
  drop = c('study_id','survmonth','survyear')
  data = data[ , !(names(data) %in% drop)]
  
  # convert to factors
  factorCols = c('gender', 'who3g', 'dumsmok2', 't_ct_loc', 'hist4g', 'countpet_all6g', 'countpet_mediast6g',	'tstage',	'nstage',	'stage',	'timing', 'group',	'yearrt', 'deadstat') # list all columns that should be factors
  data[factorCols] = lapply(data[factorCols], factor) # convert columns into factor variables
  
  data_class = data$deadstat # place outcome in separate variable
  data$deadstat = NULL #remove outcome from data
  data_class = revalue(data_class, c('1' = 'nonEvent','0' = 'event')) # relabel outcome as event and nonEvent: dead --> nonEvent, alive --> event
  return(list(data,data_class)) 
}