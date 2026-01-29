#### R script used to test new EPA TADA functions for integration into R Shiny app
# https://usepa.github.io/EPATADA/articles/ExampleMod2Workflow.html
# Written by Ben Block, Tetra Tech; Ben.Block@tetratech.com
# Date created: 10/28/2025
# Date last updated: 10/28/2025
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R version 4.4.1 (2024-06-14) -- "Race for Your Life"

# Libraries needed
library(EPATADA)
library(rExpertQuery)

# Declare directories ####
(wd <- getwd())
input.dir <- "/dev"
myDate <- format(Sys.Date(), "%Y%m%d")

# code from workflow
# get MT data
df_WQ_Input <- TADA_DataRetrieval(
  startDate = "2020-01-01",
  endDate = "2022-12-31",
  statecode = "MT",
  characteristicName = c(
    "Escherichia",
    "Escherichia coli",
    "pH"
  ),
  county = "Missoula County",
  ask = FALSE
)

# clean up data set (minimal)
df_WQ_Input_clean <- df_WQ_Input %>%
  TADA_RunKeyFlagFunctions() %>%
  TADA_SimpleCensoredMethods()

write.table(df_WQ_Input_clean, file.path(wd, input.dir
                                        , paste0("MT_DEQ_test_"
                                                 ,myDate, ".csv"))
            , row.names = FALSE, sep = ",", na = "")

# remove intermediate objects
rm(df_WQ_Input)

valid_ATTAINS_orgs <- rExpertQuery::EQ_DomainValues("org_id")
myOrg <- "MTDEQ" # Need to code this in. See something about expertQuery.

if(exists("df_UserSupplied_AUMLRef")){
  df_AUMLRef <- EPATADA::TADA_CreateAUMLCrosswalk(df_WQ_Input_clean,
                                         au_ref = df_UserSuppliedCrosswalk,
                                         org_id = myOrg,
                                         batch_upload = FALSE)
  
} else {
  df_AUMLRef <- EPATADA::TADA_CreateAUMLCrosswalk(df_WQ_Input_clean,
                                         au_ref = NULL,
                                         org_id = myOrg,
                                         batch_upload = FALSE)
} # end

# Pull out crosswalk
df_AUMLRef_Xwalk <- df_AUMLRef$ATTAINS_crosswalk

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
TADA_ViewATTAINS(df_AUMLRef)

