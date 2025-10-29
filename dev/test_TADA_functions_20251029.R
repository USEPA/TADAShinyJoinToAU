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

myOrg <- "MTDEQ" # Need to code this in. See something about expertQuery.

intersect(unique(df_WQ_Input$MonitoringLocationIdentifier)
      , unique(df_MLtoAU$MonitoringLocationIdentifier))


if(exists("df_MLtoAU")){
  df_AUMLRef <- EPATADA::TADA_CreateAUMLCrosswalk(df_WQ_Input,
                                         au_ref = df_MLtoAU,
                                         org_id = myOrg,
                                         batch_upload = FALSE)
  
} else {
  df_AUMLRef <- EPATADA::TADA_CreateAUMLCrosswalk(df_WQ_Input,
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

