Original Experiment
===============

Source: Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto. 
Smartlab - Non Linear Complex Systems Laboratory 
DITEN - Universit√  degli Studi di Genova, Genoa I-16145, Italy. 
activityrecognition '@' smartlab.ws 
www.smartlab.ws 

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 

Check the url below for further details about the original study:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 



Original Dataset
===============

The original dataset can be obtained via the url below:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

In order to execute the analysis script, the dataset needs to be extracted to a folder named 'HAR' in your working directory.


Analysis Script
===============

The analysis script can be found in the file run_analysis.R, in order to execute the script successfully the original dataset needs to be present in a folder named 'HAR' in your working directory. The script will generate the file reportSummary.txt in your working directory containing the resulting analysis/transformation.


Please see Code Book (CodeBook.md) for more information about the generated dataset.


License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 