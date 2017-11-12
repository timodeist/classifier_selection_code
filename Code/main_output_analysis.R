cat("\014") # clear the console
rm(list = ls()) # clear workspace/environment (all variables and functions)
graphics.off() # clear current plots
cat(sprintf('\n\n[%s]  main_output_analysis.R:\n', format(Sys.time(), '%Y-%m-%d %H:%M:%S')))
startTime_dataAnalysis = Sys.time()
timeLabel = format(startTime_dataAnalysis, '%Y%m%d_%H%M%S')

# set this.dir manually
this.dir = '[absolute path to main directory]/Code'
username = Sys.info()["nodename"]   # computer name used in output folder name
setwd(this.dir)

### startup parameters from ini file
startup_inifilename = 'main_output_analysis_parameters.ini'
# create default ini file if it doesn't exist and stops to allow adjustment of default values
if (!file.exists(startup_inifilename)) {
  fileConn = file(startup_inifilename)
  inidefaulttext = c( '# default parameters',
                      'saveOutput = TRUE # saves images/rdata files to output dir',
                      'datasetsToExclude = \'\' # datasets to exclude from the analysis, e.g. c(\'OberijeEtAl\',\'CarvalhoEtAl\')',
                      'anonymizeDatasets = FALSE # anonymizes dataset names to Set A, B etc.',
                      'proportionalDatasetsOn = FALSE  # generates duplicate output where results are weighed based on dataset size',
                      '',
                      '# choose preferred order in which classifiers should appear in figures',
                      'prefClassifierOrder = c(\'glmnet\',\'rf\',\'nnet\',\'svm\',\'LogitBoost\',\'rpart\')',
                      '',
                      '# choose preferred order in which datasets should appear in figures',
                      'prefDatasetOrder = c(\'OberijeEtAl\', \'CarvalhoEtAl\')'
  )
  writeLines(inidefaulttext, fileConn)
  close(fileConn)
  stop(sprintf('missing %s, created default and stopped run. Check it and restart \n', startup_inifilename))
}

# read ini, execute line by line and print values
cat(sprintf('Reading ini (%s)...\n',startup_inifilename))
line_string = readLines(startup_inifilename) # read ini file (note: last line of ini should be \n (EOL) to prevent readlines warning)
for (i_iniline in 1:length(line_string)) {
  eval(parse(text = gsub("\\","/",line_string[i_iniline], fixed=TRUE)))
}
cat(sprintf(paste('  saveOutput:            %s\n',
                  '  datasetsToExclude:     %s\n',
                  '  prefClassifierOrder:   %s\n',
                  '  prefDatasetOrder:      %s\n',sep=''),
            saveOutput,
            paste(datasetsToExclude, collapse=', '),
            paste(prefClassifierOrder, collapse=', '),
            paste(prefDatasetOrder, collapse=', ')))


### load RData file through GUI
library(svDialogs)
pathToOutputFolderDefault = strsplit(this.dir, "/")[[1]]
pathToOutputFolderDefault = paste(pathToOutputFolderDefault[-(c(length(pathToOutputFolderDefault)-1, length(pathToOutputFolderDefault)))], collapse='/')
pathToOutputFolderDefault = paste(pathToOutputFolderDefault, 'Output/*.RDATA', sep='/')
filepath = dlgOpen(pathToOutputFolderDefault,'Select .Rdata file with simulation result', multiple = FALSE, filters = dlgFilters["Rdata"])
filepath = strsplit(filepath$res, "/")[[1]]
pathToOutputFolder = paste(filepath[-(c(length(filepath)-1, length(filepath)))], collapse='/')
outputSubFolder = filepath[-(c(1:(length(filepath)-2), length(filepath)))]
outputFile = filepath[-(1:(length(filepath)-1))]

pathToFile = file.path(pathToOutputFolder, outputSubFolder, outputFile)
load(pathToFile)
rm(list = lsf.str()) # remove all functions in the workspace after loading the rdata file (to get rid of ghost-versions of functions)

### set randomisation seed using the one that was used in the main_simulation run
set.seed(minSeed) # set seed for reproducibility random classifier selection experiment


### load libraries, functions and set some debugging options (after loading the rdata file)
library(ggplot2)
library(plyr)
debugSource('addAucRanks.R')
debugSource('addCvAucRanks.R')
debugSource('aggregateFolds.R')
debugSource('aggregateReps.R')
debugSource('generateTableProportional.R')
debugSource('generateOrderedPairs.R')
debugSource('generatePlotTablePairwiseComparison.R')
debugSource('generatePlotTableAucHeatmap.R')
debugSource('generatePlotTableSelectionSimulation.R')
debugSource('generateAppendixStatsTable.R')
options(warn = 1) # show each warning when it happens
options(nwarnings = 10000) # allow many warnings to be saved
options(show.error.locations = TRUE) # show code line numbers of errors


### preprocess

# drop rows with NAs
#outputTable = outputTable[!is.na(outputTable[,'auc']),]

# exclude datasets (remove for final repository)
outputTable = outputTable[is.na(match(outputTable$dataset ,datasetsToExclude)),]
datasetSelection = datasetSelection[is.na(match(datasetSelection, datasetsToExclude))]
datasetPatientNumbers = datasetPatientNumbers[is.na(match(names(datasetPatientNumbers), datasetsToExclude))]

# order datasetSelection and classifierSelection based on preferred orders (cosmetics for plotting)
classifierSelection = classifierSelection[order(match(classifierSelection,prefClassifierOrder))]
datasetSelection = datasetSelection[order(match(datasetSelection,prefDatasetOrder))]
outputTable = outputTable[order(outputTable$rep,match(outputTable$dataset,datasetSelection),outputTable$outerFold,match(outputTable$classifier,classifierSelection)),]


# anonymizing data sets
if (anonymizeDatasets) 
{
  mappingDatasetToPseudonym = paste("Set ", toupper(letters[1:length(datasetSelection)]), sep="")
  names(mappingDatasetToPseudonym) = datasetSelection
  
  
  # save and print anonymization
  if (saveOutput) 
  {
    write.table(mappingDatasetToPseudonym, col.names = FALSE, file = file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_anonymization_mapping.txt', sep = '')))
  }
  cat(sprintf('  anonymization:         %s\n', paste(rbind(mappingDatasetToPseudonym,rep('=',numDatasets),names(mappingDatasetToPseudonym),rep('  ',numDatasets)),collapse = '')))
  
  # apply anonymization
  datasetSelection = revalue(datasetSelection, mappingDatasetToPseudonym)
  outputTable$dataset = revalue(outputTable$dataset, mappingDatasetToPseudonym)
  names(datasetPatientNumbers) = revalue(names(datasetPatientNumbers), mappingDatasetToPseudonym)
  prefDatasetOrder = revalue(prefDatasetOrder, mappingDatasetToPseudonym)
}


### data analysis/aggregation
cat(sprintf('\nStarting analysis. This may take in the order of ten minutes...\n'))

# computes ranks in the outputTable (before averaging over folds) --> used for the numerical experiment (3)
outputTable = addAucRanks(outputTable,classifierSelection,datasetSelection,maxRep,kOuter)

# create outputTable_aggFolds that contains average values over all kOuter folds
outputTable_aggFolds = aggregateFolds(outputTable,classifierSelection,datasetSelection,kOuter,maxRep)

# computes ranks in the outputTable_aggFolds (after averaging over folds)
outputTable_aggFolds = addCvAucRanks(outputTable_aggFolds,classifierSelection,datasetSelection,maxRep)

# create outputTable_aggRepsFolds that contains average values over all reps and kOuter folds
outputTable_aggRepsFolds = aggregateReps(outputTable_aggFolds,classifierSelection,datasetSelection,kOuter,maxRep)


### generate tables for plots or csv files

# create a table of ordered pairs from k-fold results
orderedPairsTable = generateOrderedPairs(outputTable_aggFolds,classifierSelection,datasetSelection,maxRep)

# create table for pairwise comparison graphs
generatePlotTablePairwiseComparisonOutput = generatePlotTablePairwiseComparison(orderedPairsTable,classifierSelection,datasetSelection,maxRep,TRUE)
pairCompPlotTable = generatePlotTablePairwiseComparisonOutput[[1]]
wilcoxList = generatePlotTablePairwiseComparisonOutput[[2]] # for debugging only

if (proportionalDatasetsOn) 
{
  # create outputTable_aggFolds to patient numbers (for appendix)
  outputTable_aggFolds_proportional = generateTableProportional(outputTable_aggFolds,datasetPatientNumbers)
  
  # create orderedPairsTableProportional to patient numbers (for appendix)
  orderedPairsTable_proportional = generateTableProportional(orderedPairsTable,datasetPatientNumbers)
  
  # create table for pairwise comparison graphs proportional to patient numbers (for appendix)
  generatePlotTablePairwiseComparisonOutput_proportional = generatePlotTablePairwiseComparison(orderedPairsTable_proportional,classifierSelection,datasetSelection,maxRep,FALSE)
  pairCompPlotTable_proportional = generatePlotTablePairwiseComparisonOutput_proportional[[1]]
  wilcoxList_proportional = generatePlotTablePairwiseComparisonOutput_proportional[[2]] # for debugging only
}

# create table for heatmaps of mean AUC (aggregated over all folds and repetitions) and rankCvAuc (aggregated over repetitions)
aucHeatmapPlotTable = generatePlotTableAucHeatmap(outputTable_aggRepsFolds,classifierSelection,datasetSelection)

# compute experiment pre-selection vs random vs inner-CV classifier selection
if (length(datasetSelection)>1) {
  generatePlotTableSelectionSimulationOutput = generatePlotTableSelectionSimulation(outputTable,outputTable_aggFolds,classifierSelection,datasetSelection,kOuter,maxRep,datasetPatientNumbers)
  selectionSimulationPlotTable = generatePlotTableSelectionSimulationOutput[[1]] # not used in tables or figures
  selectionSimulationPlotTable_aggRepsFolds = generatePlotTableSelectionSimulationOutput[[2]] # printed as csv file
  selectionSimulationWilcox = generatePlotTableSelectionSimulationOutput[[3]] # printed as csv file
} else {
  cat(paste('only one dataset present, simulation of classifier selection method not performed.\n'))
}

# create stats tables for appendix
appendixStatsTable = generateAppendixStatsTable(outputTable_aggFolds,classifierSelection)

# small in-text computations for manuscript
aucMeanPerDataset = aggregate(aucHeatmapPlotTable$auc, by = list('dataset' = aucHeatmapPlotTable$dataset), FUN = mean)
cat(paste('\n\nAverage AUC per dataset over all classifiers:\n'))
print(aucMeanPerDataset) # average AUC per dataset over all classifiers
estimatedNumberOfEmptyRparts = sum(outputTable$auc == 0.5 & outputTable$classifier == 'rpart' & is.na(outputTable$calibrationSlope))
cat(paste('\nModel failures information:\n'))
cat(sprintf('#rpart models that are empty:\t %d/%d\n',estimatedNumberOfEmptyRparts,nrow(outputTable[outputTable$classifier=='rpart',])))   # the number of rpart models that are empty (i.e., models return cohort prevalence as prediction for any patient)
failed = nrow(outputTable[is.na(outputTable$auc),])
cat(sprintf('#any model with NAs:\t\t %d/%d\n',failed,nrow(outputTable))) # any fits with NAs
failedglmnet = nrow(outputTable[outputTable$classifier=='glmnet' & is.na(outputTable$auc),])
cat(sprintf('#glmnet models with NAs:\t %d/%d\n\n',failedglmnet,nrow(outputTable[outputTable$classifier=='glmnet',]))) # glmnets with NAs

# save small in-text computations used for manuscript
if (saveOutput) 
{
  consoleoutput_txtfilename1 = file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_consoleoutput_averageaucperdataset.txt', sep = ''))
  fileConn1 = file(consoleoutput_txtfilename1)
  consoleoutputtext1 = 'Average AUC per dataset over all classifiers:'
  writeLines(consoleoutputtext1, fileConn1)
  write.table(aucMeanPerDataset, col.names = FALSE, append = TRUE, file = consoleoutput_txtfilename1, sep = ',')
  close(fileConn1)
  
  consoleoutput_txtfilename2 = file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_consoleoutput_modelfailures.txt', sep = ''))
  fileConn2 = file(consoleoutput_txtfilename2)
  consoleoutputtext2 = c('Model failures information:',
                        sprintf('#rpart models that are empty:\t %d/%d',estimatedNumberOfEmptyRparts,nrow(outputTable[outputTable$classifier=='rpart',])),
                        sprintf('#any model with NAs:\t\t\t %d/%d',failed,nrow(outputTable)),
                        sprintf('#glmnet models with NAs:\t\t %d/%d',failedglmnet,nrow(outputTable[outputTable$classifier=='glmnet',]))
  )
  writeLines(consoleoutputtext2, fileConn2)
  close(fileConn2)
}


### parameters for figure plotting and csv files
figureWidth = 8
figureHeight_boxplot = 6
figureHeight_pairwise = 6
figureHeight_heatmap = 4.5
figureDpi = 600


### write csv files to output subfolder

# save stats table to csv to appendix
if (saveOutput) 
{
  write.csv(appendixStatsTable, file = file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_appendixStats_table.csv', sep = '')))
}

# save selectionSimulationPlotTable to csv
chosenCols = c('dataset',
               'randomRankAuc_mean','randomRankAuc_var',
               'preSelectedClassifier','preSelectedRankAuc_mean','preSelectedRankAuc_var','preSelectedImprovement_mean','preSelectedRankImprovement_mean',
               'setSpecificRankAuc_mean','setSpecificRankAuc_var','setSpecificImprovement_mean','setSpecificRankImprovement_mean'
)

if (saveOutput & length(datasetSelection)>1)
{
  write.csv(selectionSimulationPlotTable_aggRepsFolds[,chosenCols], file = file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_selectionSimulation_table.csv', sep = '')))
  write.csv(selectionSimulationPlotTable_aggRepsFolds, file = file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_selectionSimulation_table_full.csv', sep = '')))
  write.csv(selectionSimulationWilcox, file = file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_selectionSimulation_wilcox_table.csv', sep = '')))
}

### write plot files to output subfolder

# scatter boxplot
ggplot(outputTable_aggFolds, aes(x = classifier,y = rankCvAuc)) +
  geom_boxplot(fill = 'violetred') +
  geom_jitter(width = 0.3, height = 0.15, shape = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.background =element_blank()) +
  scale_y_continuous(breaks = seq(1,7,1)) +
  labs(x = 'Classifier', y = 'AUC rank') + 
  scale_x_discrete(limits = classifierSelection)

if (saveOutput) 
{
  ggsave(file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_scatterboxplot_rankAuc.png', sep = '')), device = 'png', width = figureWidth, height = figureHeight_boxplot, dpi = figureDpi) 
  ggsave(file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_scatterboxplot_rankAuc.eps', sep = '')), device = 'eps', width = figureWidth, height = figureHeight_boxplot, dpi = figureDpi) 
}

if (proportionalDatasetsOn) 
{
  # scatter plot proportional to patient numbers without scatter (for appendix)
  ggplot(outputTable_aggFolds_proportional, aes(x = classifier,y = rankCvAuc)) +
    geom_boxplot(fill = 'violetred') +
    #geom_jitter(width = 0.3, height = 0.15, shape = 20) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.background =element_blank()) +
    scale_y_continuous(breaks = seq(1,7,1)) +
    labs(x = 'Classifier', y = 'AUC rank') + 
    scale_x_discrete(limits = classifierSelection)

  if (saveOutput) 
  {
    ggsave(file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_scatterboxplot_rankAuc_proportional.png', sep = '')), device = 'png', width = figureWidth, height = figureHeight_boxplot, dpi = figureDpi) 
    ggsave(file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_scatterboxplot_rankAuc_proportional.eps', sep = '')), device = 'eps', width = figureWidth, height = figureHeight_boxplot, dpi = figureDpi) 
  }
}

# heatmap pairwise comparisons
colorMapping = c('not tested' = 'grey90','insignificant' = 'red','significant' = 'violetred')

ggplot(pairCompPlotTable, aes( x = classifierTwo, y = classifierOne)) +
  geom_tile(aes(fill = boxColor), color = 'white') +
  scale_fill_manual(values = colorMapping, guide = guide_legend(title = 'Wilcoxon\nsigned-rank\ntest\n(A > B)')) + 
  geom_text(aes(label = displayText)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        plot.background =element_blank()) +
  labs(x = 'Classifier B', y = 'Classifier A')

if (saveOutput) 
{
  ggsave(file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_heatmap_pairwiseComparison.png', sep = '')), device = 'png', width = figureWidth, height = figureHeight_pairwise, dpi = figureDpi) 
  ggsave(file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_heatmap_pairwiseComparison.eps', sep = '')), device = 'eps', width = figureWidth, height = figureHeight_pairwise, dpi = figureDpi) 
}

if (proportionalDatasetsOn) 
{
  # heatmap pairwise comparisons proportional for patient numbers (for appendix)
  ggplot(pairCompPlotTable_proportional, aes( x = classifierTwo, y = classifierOne)) +
    geom_tile(aes(fill = boxColor), color = 'white') +
    scale_fill_manual(values = colorMapping, guide = guide_legend(title = 'Wilcoxon\nsigned-rank\ntest\n(A > B)')) + 
    geom_text(aes(label = displayText)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          plot.background =element_blank()) +
    labs(x = 'Classifier B', y = 'Classifier A')
  
  if (saveOutput) 
  {
    ggsave(file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_heatmap_pairwiseComparison_proportional.png', sep = '')), device = 'png', width = figureWidth, height = figureHeight_pairwise, dpi = figureDpi) 
    ggsave(file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_heatmap_pairwiseComparison_proportional.eps', sep = '')), device = 'eps', width = figureWidth, height = figureHeight_pairwise, dpi = figureDpi) 
  }
}

# heatmap with mean AUC rank over folds and repetitions per dataset and classifier
ggplot(data = aucHeatmapPlotTable, aes(x = dataset, y = classifier)) + 
  geom_tile(aes(fill = rankCvAuc), color = 'white') + 
  theme_bw() +
  scale_fill_gradient(name = 'Mean AUC\nrank', low = 'violetred', high = 'white', guide = 'colorbar') +
  geom_text(aes(label = displayTextRankCvAuc)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        plot.background =element_blank()) +
  labs(x = 'Dataset', y = 'Classifier')
if (saveOutput) 
{
  ggsave(file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_heatmap_rankauc.png', sep = '')), device = 'png', width = figureWidth, height = figureHeight_heatmap, dpi = figureDpi) 
  ggsave(file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_heatmap_rankauc.eps', sep = '')), device = 'eps', width = figureWidth, height = figureHeight_heatmap, dpi = figureDpi) 
}


# heatmap with mean AUC over folds and repetitions per dataset and classifier
ggplot(data = aucHeatmapPlotTable, aes(x = dataset, y = classifier)) + 
  geom_tile(aes(fill = auc), color = 'white') + 
  theme_bw() +
  scale_fill_gradient2(name = 'Mean AUC', low = 'grey', mid = 'white', high = 'violetred', midpoint = 0.5, guide = 'colorbar') +
  geom_text(aes(label = displayTextAuc)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        plot.background =element_blank()) +
  labs(x = 'Dataset', y = 'Classifier')
if (saveOutput) 
{
  ggsave(file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_heatmap_auc.png', sep = '')), device = 'png', width = figureWidth, height = figureHeight_heatmap, dpi = figureDpi) 
  ggsave(file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_main_output_analysis_heatmap_auc.eps', sep = '')), device = 'eps', width = figureWidth, height = figureHeight_heatmap, dpi = figureDpi) 
}

if (saveOutput)
{
  # copy current output analysis ini to output dir and rename with timelabel
  file.copy(startup_inifilename, file.path(pathToOutputFolder,outputSubFolder))
  file.rename(file.path(pathToOutputFolder,outputSubFolder,startup_inifilename), file.path(pathToOutputFolder,outputSubFolder,paste(timeLabel,'_',startup_inifilename, sep = '')))
}

# open output folder in explorer
options(warn = -1) # disable warning
dummy = shell(paste("explorer ", gsub("/","\\",paste(pathToOutputFolder,outputSubFolder,sep='/'), fixed=TRUE), sep=""), intern=TRUE)

# plot times
endTime = Sys.time()
runningTime = endTime - startTime_dataAnalysis
print(runningTime)