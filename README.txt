This is the code and data accompanying the paper: Street Tree Stress Levels Vary with Tree Species, Urban Tolerance, and Urban Conditions in Two U.S. Cities

For any questions, contact Ren Kamakura at: renatakamakura@gmail.com

The files are organized as follows:

01Data: Includes data collected for each city and quality control (QAQC) data
* Chicago: 
	Has raw datasheets and the combined data used in the analysis (NOTE: the raw data include minor spelling errors and other issues corrected in the code). 
	University of Illinois at Chicago (UIC) data (a nearby university) were not used in final analyses but are kept here for reference. 
	Otherwise, files are labeled as to whether they include the tree health data, the growing condition data, or both. 
	Information from Dirr's Encyclopedia of trees and shrubs for Chicago species also included
* Durham:
	Analogous to Chicago, has raw datasheets and combined data use in analysis. Be aware that the raw data can include minor spelling errors corrected in the code
	Duke Campus data (a university within Durham) were not used in final analyses but are kept here for reference.
	Data from a nearby park (West Point on the Eno) were also not used in final analyses but are kept here for reference.
	Files are labeled as to whether they include the tree health data, the growing condition data, or both. 
	Information from Dirr's Encyclopedia of trees and shrubs for Durham species also included
* QAQC
	These are the data used for quality control testing, labeled by the city and year.
	QAQCResults_raw shows the QAQC results (difference between first and second data collection) based on the ordinal categories used in the data collection process. Note that these errors will be larger because some of the groupings will be simplified for analyses
	QAQCResults_mod shows the QAQC results (difference between first and second data collection) but with the ordinal categories first simplified to match those used in the analysis. Since categories are grouped, these errors are smaller. 

02Scripts: The R code and related files used for data analysis
* Chicago_Combine3Years: code to combine data from 2021-2023 in Chicago
* Chicago_MainAnalysis: code used to run multivariate and univariate ordinal models in Chicago
* DurhamCombine2Years: code to combine data from 2022-2023 in Durham
* DurhamMainAnalyses: code used to run multivariate and univariate ordinal models in Durham
* QAQC: code to look at QAQC data (see QAQC folder for results)
* ordReg_1.0.tar: code written to run univariate ordinal models