library(data.table)

## Creates the report generator
## The returned object encapsulates all the required functionalities and exposes only the minimum necessary for external usage
createReportGen <- function(){

  ## Constants
  ACTIVITY_LABELS_PATH       = "HAR/activity_labels.txt"
  FEATURES_LIST_PATH         = "HAR/features.txt"
  TEST_SUBJECTS_PATH         = "HAR/test/subject_test.txt"
  TEST_ACTIVITY_REPORT_PATH  = "HAR/test/y_test.txt"
  TEST_RESULT_PATH           = "HAR/test/X_test.txt"
  TRAIN_SUBJECTS_PATH        = "HAR/train/subject_train.txt"
  TRAIN_ACTIVITY_REPORT_PATH = "HAR/train/y_train.txt"
  TRAIN_RESULT_PATH          = "HAR/train/X_train.txt"
  
  ## Gets the activity labels
  getActivityLabels <- function (){
    
    labels <- read.table(
      file      = ACTIVITY_LABELS_PATH,
      sep       = " ",
      as.is     = TRUE,
      col.names = c("id", "label"))
    
    labels
    
  }
  
  ## Gets the features
  getFeatures <- function(){
    
    features <- read.table(
      file      = FEATURES_LIST_PATH,
      sep       = " ",
      as.is     = TRUE,
      col.names = c("id", "feature"))
    
    features
    
  }
  
  ## Gets the report desired features
  getDesiredFeatures <- function(){
    
    grep("(mean\\(\\)|std\\(\\))", features$feature)
    
  }
  
  ## Gets the experiment subjects 
  ## path: The file path containing the subjects to be loaded
  getSubjects <- function(path){
    
    test.subjects <- read.table(
      file      = path,
      sep       = " ",
      col.names = c("Subject"))
    
    test.subjects
    
  }
  
  ## Gets the activities
  ## path: The file path containing the activities to be loaded
  getActivities <- function(path){
    
      y <- read.table(
      file = path,
      sep  = " ")
    
    factor(as.numeric(unlist(y)), levels = activityLabels$id, labels = activityLabels$label)
    
  }
  
  ## Gets the Report 
  ## path: The file path containing the records data to be loaded
  ## subjects.path: The file path containing the subjects to be loaded
  ## activities.path: The file path containing the activities to be loaded
  getReport <- function(path, subjects.path, activities.path){
    
    ## Loads test data
      x <- read.table(
      file = path,
      fill = FALSE)
    
    desiredFeatures   <- getDesiredFeatures()
    
    records           <- x[, desiredFeatures]
    colnames(records) <- features$feature[desiredFeatures]
    
    subjects          <- getSubjects(subjects.path)
    activities        <- getActivities(activities.path)
    

    cbind(data.frame(Subject = subjects, Activity = activities), records)
  }
  
  ## Merges two reports
  mergeReports <- function(A,B){
    rbind(A,B)
  }
  
  ## Gets the Test Report 
  getTestReport <- function(){
    getReport(TEST_RESULT_PATH, TEST_SUBJECTS_PATH, TEST_ACTIVITY_REPORT_PATH)
  }
  
  ## Gets the Train Report 
  getTrainReport <- function(){
    getReport(TRAIN_RESULT_PATH, TRAIN_SUBJECTS_PATH, TRAIN_ACTIVITY_REPORT_PATH)
  }
  
  ## Gets the Full Report (Train + Test)
  getFullReport <- function(){
      
    test.report  <- getTestReport()
    train.report <- getTrainReport()
    
    mergeReports(test.report, train.report)
  }
  
  ## Gets the Report Summary with the average of each variable for each activity and each subject
  ## report : The report where the summary will be based on
  getReportSummary <- function(report){
    
    DT = data.table(report)
    DT[, lapply(.SD, mean), by=c("Subject", "Activity")]
    
  }
  
  ## Saves a report to the file system
  ## path : The full path where the file will be saved
  ## report : The report to be saved
  save <- function(path, report){
    write.table(report, path, row.names = FALSE)    
  }
  
  ## Creates private data
  activityLabels       <- getActivityLabels()
  features             <- getFeatures()
  
  ## Returns the object cointaining the required functionalities
  list (getTestReport    = getTestReport,
        getTrainReport   = getTrainReport,
        getFullReport    = getFullReport,
        getReportSummary = getReportSummary,
        save             = save)
}


## Creates the report generator
gen           <- createReportGen()

## Gets the Full Report (Test + Train)
fullReport    <- gen$getFullReport()

## Generates the Report Summary
reportSummary <- gen$getReportSummary(fullReport)

## Saves the Report Summary
gen$save("reportSummary.txt", reportSummary)

