## crontabR Automation Script for BIOSEND_ACCESSIONING_REPORT
##
## Run the weekly BioSEND Accessioning Report and email it to the coordinators.
library(crontabR)
setCronjobValues('BIOSEND_ACCESSIONING_REPORT', 'Run the weekly BioSEND Accessioning Report and email it to the coordinators.')
cronLog("Script Started.")
##### Do not edit above this line #####

## BioSEND Accessioning Report Email

library(dplyr)
library(oncore2)
library(methods)
library(oncoreCatalogs)
library(crontabR)

if(class(x <- try(emailAccessionReport())) == "try-error") {
  cronLog("BioSEND Accession Report", level = "error")
}


##### Do not edit below this line #####
cronLog("Script Complete.")
