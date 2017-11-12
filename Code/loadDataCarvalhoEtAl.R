loadDataCarvalhoEtAl = function(pathFromDataFolderToCsv,pathToDataFolder)
{
  library(plyr) # used for revalue
  pathToFile = file.path(pathToDataFolder,pathFromDataFolderToCsv) # construct full file to csv
  data = read.csv(pathToFile,sep = ",", dec = ".", strip.white = TRUE, na.strings = '') # read data from .csv
  
  data_class = data$Survival # place outcome in separate variable
  data_class = factor(data_class>= 2)
  data$Survival = NULL #remove outcome from data
  data$ID = NULL #remove from data
  data$Status = NULL #remove from data
  
  data$IL.6 = revalue(data$IL.6, c('<2' = 2)) # recode <2 as 2 in IL.6
  data$IL.8 = revalue(data$IL.8, c('<5' = 5)) # recode <5 as 5 in IL.8
  data$Cyfra.21.1 = revalue(data$Cyfra.21.1, c('<0,1' = 0.1)) # recode <0,1 as 0.1 in Cyfra.21.1
  
  # convert columns from factor to numeric
  data$IL.6 = as.numeric(levels(data$IL.6))[data$IL.6]
  data$IL.8 = as.numeric(levels(data$IL.8))[data$IL.8]
  data$Cyfra.21.1 = as.numeric(levels(data$Cyfra.21.1))[data$Cyfra.21.1]
  
  # convert to factors
  factorCols = c('WHO.PS') # list all columns that should be factors
  data[factorCols] = lapply(data[factorCols], factor) # convert columns into factor variables
  
  data_class = revalue(data_class, c('TRUE' = 'event','FALSE' = 'nonEvent')) # relabel outcome as event and nonEvent
  return(list(data,data_class)) 
}