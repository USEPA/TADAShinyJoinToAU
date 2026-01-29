#### R script used to test new EPA TADA functions for integration into R Shiny app
# https://usepa.github.io/EPATADA/articles/ExampleMod2Workflow.html
# Written by Ben Block, Tetra Tech; Ben.Block@tetratech.com
# Date created: 10/29/2025
# Date last updated: 10/29/2025
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R version 4.4.1 (2024-06-14) -- "Race for Your Life"

# Suggested updates by Hillary Marler, EPA; Marler.Hillary@epa.gov
# Date last updated: 12/9/2025

# Libraries needed
library(EPATADA)
library(rExpertQuery)
library(readr)
library(dplyr)

# Declare directories ####
(wd <- getwd())
input.dir <- "/dev"
myDate <- format(Sys.Date(), "%Y%m%d")

# pull in Utah data
# get MT data
df_WQ_Input <- read_csv(file.path(wd, input.dir
                          , "MT_DEQ_test_TRIM_20251029.csv")
                          , na = c("NA",""), trim_ws = TRUE, skip = 0
                          , col_names = TRUE, guess_max = 100000)

df_MLtoAU <- read_csv(file.path(wd, input.dir
                                , "MT_DEQ_User_MLtoAU_test_20251029.csv")
                      , na = c("NA",""), trim_ws = TRUE, skip = 0
                      , col_names = TRUE, guess_max = 100000)

df_UserSupplied_UseAURef <- read_csv(file.path(wd, input.dir
                                     , "MT_DEQ_User_AUtoUse_test_20251029.csv")
                                     , na = c("NA",""), trim_ws = TRUE, skip = 0
                                     , col_names = TRUE, guess_max = 100000)



myOrg <- "MTDEQ" # Need to code this in. See something about expertQuery.

# QC checks ####
#  Required columns
required_cols <- c("ResultIdentifier", "TADA.MonitoringLocationName"
                   , "TADA.LatitudeMeasure", "TADA.LongitudeMeasure"
                   , "HorizontalCoordinateReferenceSystemDatumName"
                   , "TADA.MonitoringLocationIdentifier"
                   , "TADA.CharacteristicName", "ActivityStartDate"
                   , "OrganizationIdentifier")

missing_cols <- setdiff(required_cols, colnames(df_WQ_Input))

if (length(missing_cols) > 0) {
  stop(paste("The following required columns are missing from the
             df_WQ_Input dataframe:",
             paste(missing_cols, collapse = ", ")))
} # End ~ if statement

# Force conversion
df_WQ_Input_v2 <- df_WQ_Input %>%
  # use new internal TADA function for conversion
  EPATADA::TADA_CorrectColType()

class(df_WQ_Input$StateCode)
class(df_WQ_Input_v2$StateCode)

# cleanup
rm(missing_cols, required_cols, df_WQ_Input)

# Run TADA function# Run TADA functiontype
if(exists("df_MLtoAU")){
  AUMLRef <- EPATADA::TADA_CreateAUMLCrosswalk(df_WQ_Input_v2,
                                         au_ref = df_MLtoAU,
                                         org_id = myOrg,
                                         batch_upload = FALSE)

} else {
  AUMLRef <- EPATADA::TADA_CreateAUMLCrosswalk(df_WQ_Input_v2,
                                         au_ref = NULL,
                                         org_id = myOrg,
                                         batch_upload = FALSE)
} # end

# Pull out crosswalk
df_AUMLRef_Xwalk <- AUMLRef$ATTAINS_crosswalk

# get uses
if(exists("df_UserSupplied_UseAURef")){
  df_UseAURef <- EPATADA::TADA_CreateUseAURef(AUMLRef = df_AUMLRef_Xwalk
                                              , useAURef = df_UserSupplied_UseAURef
                                              , org_id = myOrg)
} else {
  df_UseAURef <- EPATADA::TADA_CreateUseAURef(AUMLRef = df_AUMLRef_Xwalk
                                              , useAURef = NULL
                                              , org_id = myOrg)
} # end

# Map
TADA_ViewATTAINS(AUMLRef)

