#### R script used to test new EPA TADA functions for integration into R Shiny app
# https://usepa.github.io/EPATADA/articles/ExampleMod2Workflow.html
# Written by Ben Block, Tetra Tech; Ben.Block@tetratech.com
# Date created: 10/29/2025
# Date last updated: 10/29/2025
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R version 4.4.1 (2024-06-14) -- "Race for Your Life"

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

df_file_types<- read_csv(file.path(wd, input.dir
                                  , "column_info_geospatial.csv")
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

# Field types
# Compare actual types to expected types
df_file_types <- df_file_types %>%
  mutate(column_name = as.character(column_name)
         , column_type = as.character(column_type))

check_types <- tibble::tibble(
  column_name  = df_file_types$column_name,
  column_type  = df_file_types$column_type,
  actual_type  = vapply(df_file_types$column_name, function(col) {
    if (col %in% names(df_WQ_Input)) class(df_WQ_Input[[ col ]])[1] else "missing"
  }, FUN.VALUE = character(1))
) %>%
  dplyr::mutate(matches = actual_type == column_type)

# Show mismatches
check_types %>% filter(!matches)

# Force conversion
df_WQ_Input_v2 <- df_WQ_Input
for (i in seq_len(nrow(df_file_types))) {
  col <- df_file_types$column_name[i]
  type <- df_file_types$column_type[i]
  
  if (col %in% names(df_WQ_Input_v2)) {
    if (type == "character") df_WQ_Input_v2[[col]] <- as.character(df_WQ_Input_v2[[col]])
    if (type == "numeric")   df_WQ_Input_v2[[col]] <- as.numeric(df_WQ_Input_v2[[col]])
    if (type == "integer")   df_WQ_Input_v2[[col]] <- as.integer(df_WQ_Input_v2[[col]])
    if (type == "logical")   df_WQ_Input_v2[[col]] <- as.logical(df_WQ_Input_v2[[col]])
  }
}

class(df_WQ_Input$StateCode)
class(df_WQ_Input_v2$StateCode)

# cleanup
rm(col, i, missing_cols, required_cols, type, check_types, df_file_types
   , df_WQ_Input)

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

