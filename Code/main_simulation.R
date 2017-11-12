cat("\014") # clear the console
rm(list = ls()) # clear workspace/environment (all variables and functions)
graphics.off() # clear current plots
cat(sprintf('\n\n[%s]  main_simulation.R:\n', format(Sys.time(), '%Y-%m-%d %H:%M:%S')))
startTime_simulation = Sys.time()
timeLabel = format(Sys.time(), '%Y%m%d_%H%M%S')

# set this.dir manually
this.dir = '[absolute path to main directory]/Code'
username = Sys.info()["nodename"]   # computer name used in output folder name
setwd(this.dir)

### load function and set some debugging options
debugSource('runCvForClassifiers.R')
options(warn = 1) # show each warning when it happens
options(nwarnings = 10000) # allow many warnings to be saved
options(show.error.locations = TRUE) # show code line numbers of errors


### startup parameters from ini file and dataset defintion from csv file
startup_inifilename = 'main_simulation_parameters.ini'
# create default ini file if it doesn't exist and stops to allow adjustment of default values
if (!file.exists(startup_inifilename)) {
  fileConn<-file(startup_inifilename)
  inidefaulttext = c( '# default parameters',
                      'pathToDataFolder = \'../Data\'',
                      'pathToOutputFolder = \'../Output\'',
                      'startup_csvfilename = \'main_simulation_datasetDefinitions\'  # csv in code dir with dataset names, foldernames, filenames and load function names',
                      'maxRep = 10		# defining the number of repetitions of the analysis',
                      'kOuter = 5		# defining the number of folds used in the outer CV',
                      'kInner = 5		# defining the number of folds used in the parameter tuning CV',
                      'saveOutput = TRUE	# saves ini/csv/rdata file of the run to output dir',
                      'anonymizeDatasets = FALSE # dataset names in the output rdata file will be anonymized to Set A, B etc.',
                      'mySeed = 1622017	# seed for reproducibility (used before creating outer folds and before each model tuning/training) default = 1622017',
                      'defaultTuning = TRUE	# set to TRUE if you want to use the default tuning grids from the caret package, if FALSE manually determined tuning grids are used (many are not yet implemented)',
                      '',
                      '# select classifiers to use. LOOKUP: c(\'glmnet\',\'rf\',\'nnet\',\'svm\',\'LogitBoost\',\'rpart\')',
                      'classifierSelection = c(\'glmnet\',\'rf\',\'nnet\',\'svm\',\'LogitBoost\',\'rpart\')',
                      '',
                      '# select dataset to use. LOOKUP: see datasetName column in \'main_simulation_datasetDefinitions.csv\'',
                      'datasetSelection = c(\'OberijeEtAl\',\'CarvalhoEtAl\')'
  )
  writeLines(inidefaulttext, fileConn)
  close(fileConn)
  stop(sprintf('missing %s, created default and stopped run. Adjust it and restart \n',startup_inifilename))
}

# read ini, execute line by line and print values
cat(sprintf('Reading ini (%s)...\n',startup_inifilename))
line_string <- readLines(startup_inifilename) # read ini file (note: last line of ini should be \n (EOL) to prevent readlines warning)
for (i_iniline in 1:length(line_string)) {
  eval(parse(text = gsub("\\","/",line_string[i_iniline], fixed=TRUE)))
}
cat(sprintf(paste('  maxRep:                %d\n',
                  '  kOuter:                %d\n',
                  '  kInner:                %d\n',
                  '  saveOutput:            %s\n',
                  '  mySeed:                %d\n',
                  '  classifierSelection:   %s\n',
                  '  datasetSelection:      %s\n',sep=''),
            maxRep,
            kOuter,
            kInner,
            saveOutput,
            mySeed,
            paste(classifierSelection, collapse=', '),
            paste(datasetSelection, collapse=', ')))

# error check number of outer folds
if (kOuter<2) {
  # 1 fold is not allowed, stop
  stop('setting the number of outer folds to 1 is not allowed, stopped run...\n')
}

# error check number of inner folds
if (kInner<2) {
  # 1 fold is not allowed, stop
  stop('setting the number of inner folds to 1 is not allowed, stopped run...\n')
}

# read dataset definitions from csv (names, foldernames, filenames, load function names)
if (!grepl(".csv", startup_csvfilename)) {
  # .csv extension missing, add it
  startup_csvfilename = paste(startup_csvfilename,".csv",sep = "")
}
if (!file.exists(startup_csvfilename)) {
  # csv missing, stop with error
  stop(sprintf('missing %s, stopped run...\n',startup_csvfilename))
}
datasetDefinitions = read.csv(startup_csvfilename, sep = ";")
datasetName <- as.vector(datasetDefinitions$datasetName)
datasetFoldername <- as.vector(datasetDefinitions$datasetFoldername)
datasetFilename <- as.vector(datasetDefinitions$datasetFilename)
datasetLoadFunction <- as.vector(datasetDefinitions$datasetLoadfunction)

# check if output dir exists
if (!dir.exists(pathToOutputFolder)) {
  # output dir missing, stop with error
  stop(sprintf('output dir does not exist, stopped run...  (%s)\n',pathToOutputFolder))
}

# anonymizing data sets
if (anonymizeDatasets) {
  mappingDatasetToPseudonym = paste("Set ", toupper(letters[1:length(datasetSelection)]), sep="")
  names(mappingDatasetToPseudonym) = datasetSelection
  
  # print anonymization (the anonymization mapping file is saved to output subfolder after the simulation is done)
  cat(sprintf('  anonymization:         %s\n', paste(rbind(mappingDatasetToPseudonym,rep('=',2),names(mappingDatasetToPseudonym),rep('  ',2)),collapse = '')))
  
  # apply anonymization
  datasetSelection = revalue(datasetSelection, mappingDatasetToPseudonym)
  datasetName = revalue(datasetName, mappingDatasetToPseudonym)
}

# construct named vectors
datasetFoldername = setNames(datasetFoldername, datasetName)
datasetFilename = setNames(datasetFilename, datasetName)
datasetLoadFunction = setNames(datasetLoadFunction, datasetName)

# add .csv extensions in datasetFilenames if they're missing
for (i_dataset in 1:length(datasetFilename)) {
  if (regexpr('.csv', datasetFilename[i_dataset], fixed=TRUE)==-1) {
    datasetFilename[i_dataset] = paste(datasetFilename[i_dataset], ".csv", sep = "")
  }
}

# remove .R extensions in datasetLoadFunction if they're present
for (i_dataset in 1:length(datasetLoadFunction)) {
  if (regexpr(".R", datasetLoadFunction[i_dataset], fixed=TRUE)>1) {
    datasetLoadFunction[i_dataset] = substr(datasetLoadFunction[i_dataset], 1, regexpr(".R", datasetLoadFunction[i_dataset], fixed=TRUE)-1)
  }
}

# add dataset loadData functions to debugSource (now we need the .R extension)
for (i_dataset in 1:length(datasetSelection)) {
  loadfunctionFilename = paste(datasetLoadFunction[datasetSelection[i_dataset]], ".R", sep = "")
  if (file.exists(loadfunctionFilename)) {
    debugSource(loadfunctionFilename)
  } else {
    stop(sprintf('missing loadfunction file (%s), check input name in ini (%s) of dataset #%d, stopped run...\n', loadfunctionFilename, datasetSelection[i_dataset], i_dataset))
  }
}


### load data

# preallocate
listOfData = list() # initalize list that contains multiple predictor data frames
listOfDataClass = list() # initalize list that contains multiple outcome  class vectors
datasetPatientNumbers = vector(mode = "integer", length = length(datasetSelection))
names(datasetPatientNumbers) = datasetSelection

# loop over datasets, add paths to data files within 'Data' folder, add labels for the datasets and load the data
cat(sprintf('Loading datasets...\n'))
for (i_dataset in 1:length(datasetSelection)) {
  # print current dataset to be loaded
  cat(sprintf('  Loading dataset %d/%d\t %-32s\t', i_dataset, length(datasetSelection), datasetSelection[i_dataset]))
  
  # load current selected dataset (eval() function used to automatically include new datasets in the csv)
  datasetFilePath = file.path(datasetFoldername[datasetSelection[i_dataset]], datasetFilename[datasetSelection[i_dataset]])
  eval(parse(text = paste('loadedData = ', datasetLoadFunction[datasetSelection[i_dataset]], '(datasetFilePath, pathToDataFolder)', sep = "") ))
  
  # save patient numbers of current dataset
  datasetPatientNumbers[i_dataset] = length(loadedData[[2]])
  
  # print info
  cat(sprintf('nfeatures=%i\t prevalence=%g%%\t (%i/%i)\n', ncol(loadedData[[1]]), round(100*sum(loadedData[[2]]=='event')/length(loadedData[[2]])), sum(loadedData[[2]]=='event'), length(loadedData[[2]])))
  
  # store in list
  listOfData[[i_dataset]] = loadedData[[1]] # clumsy way of storing function output in separate variables
  listOfDataClass[[i_dataset]] = loadedData[[2]]
}

if (saveOutput) {
  # build file & folder names for saving models per repetition/dataset
  numDatasets = length(datasetSelection)
  numClassifiers = length(classifierSelection)
  outputFoldername = paste(timeLabel, '_', username, '_', numDatasets, 'datasets_', numClassifiers, 'classifiers_models', sep = '')
  pathToOutputSubFolderModels = file.path(pathToOutputFolder, outputFoldername) # output location for models
  dir.create(pathToOutputSubFolderModels)
}


### run classifiers on all datasets
outputTable = data.frame(rep = numeric(), dataset = character(), outerFold = numeric(), classifier = character(), innerCvAuc = numeric(), auc = numeric(), calibrationIntercept = numeric(), calibrationSlope = numeric(), brierScore = numeric(), HosLemPvalue = numeric(), myAccuracy = numeric(), myKappa = numeric()) # initialize outputTable

# loop over repetitions
cat(sprintf('Running classifiers on datasets...\n'))
for (i_rep in 1:maxRep) {
  # print current classifier rep number
  timeleft = ((Sys.time() - startTime_simulation)/(i_rep-1)) * (maxRep-i_rep+1) # average time per rep * number of reps left
  cat(sprintf('  Repetition %d/%d\t\t\t\t\t\t\t\t\t\t\t\t timeleft~%.1f %s\n', i_rep, maxRep, timeleft, units(timeleft)))
  
  # loop over each dataset
  mySeedPerRep = mySeed + i_rep
  for (i_dataset in 1:length(datasetSelection)) {
    # print current dataset
    cat(sprintf('    Dataset %d/%d\t %s\n', i_dataset, length(datasetSelection), datasetSelection[i_dataset]))
    
    analysisOutput = runCvForClassifiers(listOfData[[i_dataset]],listOfDataClass[[i_dataset]],kOuter,kInner,classifierSelection,defaultTuning,mySeedPerRep) # function that applies all classifiers
    
    newRows = analysisOutput[[1]] # clumsy way of storing function output in separate variables
    newRows['dataset'] = datasetSelection[i_dataset] # add dataset name to new result
    newRows['rep'] = i_rep # add repetition number to new result
    outputTable = rbind(outputTable,newRows) # append new result to outputTable
    currentModelFits =  analysisOutput[[2]] # clumsy way of storing function output in separate variables
    
    # save current modelFits
    if (saveOutput) {
      outputFilename = paste(timeLabel,'_main_simulation_models_rep',toString(i_rep),'_dataset',datasetSelection[i_dataset], sep = '')
      save(list = c('currentModelFits'), file = file.path(pathToOutputSubFolderModels,paste(outputFilename,'.RData',sep = '')))
    }
  }
}

# display running time
endTime = Sys.time()
runningTime = endTime - startTime_simulation
print(runningTime)


### save workspace output
if (saveOutput) {
  # build file & folder names
  numDatasets = length(datasetSelection)
  numClassifiers = length(classifierSelection)
  minSeed = mySeed
  maxSeed = mySeed+maxRep-1
  outputFoldername = paste(timeLabel, '_', username, '_', numDatasets, 'datasets_', numClassifiers, 'classifiers_', maxRep, 'reps', sep = '')
  outputFilename = paste(timeLabel,'_main_simulation_output_seeds',toString(minSeed),'-',toString(maxSeed), sep = '')
  pathToOutputSubFolder = file.path(pathToOutputFolder, outputFoldername) # output location for workspace and plots, datasets live outside the git folder
  dir.create(pathToOutputSubFolder)
  
  # save output (models are saved separately already, datasetDefinitions not saved since its information is in the csv)
  parametersToSave = c('datasetPatientNumbers', 'outputTable', 'analysisOutput', 'classifierSelection', 'datasetSelection', 'defaultTuning', 'kInner', 'kOuter', 'minSeed', 'maxSeed', 'maxRep', 'numDatasets', 'numClassifiers','startTime_simulation','endTime','runningTime','username')
  save(list = parametersToSave, file = file.path(pathToOutputSubFolder,paste(outputFilename,'.RData',sep = '')))
  
  # save anonymization to text file
  if (anonymizeDatasets) {
    write.table(mappingDatasetToPseudonym, col.names = FALSE, file = file.path(pathToOutputSubFolder,paste(timeLabel,'_main_simulation_anonymization_mapping.txt', sep = '')))
  }
  
  # copy current startup ini and dataset csv to output dir and rename with timelabel
  file.copy(c(startup_inifilename, startup_csvfilename), pathToOutputSubFolder)
  file.rename(file.path(pathToOutputSubFolder,startup_inifilename), file.path(pathToOutputSubFolder,paste(timeLabel,'_',startup_inifilename, sep = '')))
  file.rename(file.path(pathToOutputSubFolder,startup_csvfilename), file.path(pathToOutputSubFolder,paste(timeLabel,'_',startup_csvfilename, sep = '')))
}