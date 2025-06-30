## code to prepare `mltoau_crosswalk` dataset goes here

# TODO
# 1. ask ben to make columns common between tabs
# 2. create a function for the au to use work (from attains)


# load libraries
library(tidyverse)
library(readxl)
library(here)

# load ml to au data
# this file was compiled from ben with feedback from EPA and the states
# mt, co, and sd are still in progress so just select from nd, wy, ut for now

# nd streams
mltoau_nd_streams_raw <- read_xlsx(path = here::here("data-raw", "R8_MLtoAU_Review_All_States_20250602.xlsx"),
                                   sheet = "ND_Streams",
                                   col_names = TRUE)

# nd lakes
mltoau_nd_lakes_raw <- read_xlsx(path = here::here("data-raw", "R8_MLtoAU_Review_All_States_20250602.xlsx"),
                                   sheet = "ND_Lakes",
                                   col_names = TRUE)

# ut streams
mltoau_ut_streams_raw <- read_xlsx(path = here::here("data-raw", "R8_MLtoAU_Review_All_States_20250602.xlsx"),
                                   sheet = "UT_Streams",
                                   col_names = TRUE)

# ut lakes
mltoau_ut_lakes_raw <- read_xlsx(path = here::here("data-raw", "R8_MLtoAU_Review_All_States_20250602.xlsx"),
                                 sheet = "UT_Lakes",
                                 col_names = TRUE)

# wy streams
mltoau_wy_streams_raw <- read_xlsx(path = here::here("data-raw", "R8_MLtoAU_Review_All_States_20250602.xlsx"),
                                   sheet = "WY_Streams",
                                   col_names = TRUE)

# wy lakes
mltoau_wy_lakes_raw <- read_xlsx(path = here::here("data-raw", "R8_MLtoAU_Review_All_States_20250602.xlsx"),
                                 sheet = "WY_Lakes",
                                 col_names = TRUE)

# load au to uses data
# this data was obtained from attains by ben using the code below this section
autouse_raw <- read_csv(file = here::here("data-raw", "R8_ATTAINS_AU_Uses_Long_20250602.csv"),
                        col_names = TRUE)


# ---- ml to au crosswalk for all r8 states ----
# select columns
sel_cols <- c("OrganizationIdentifier", "MonitoringLocationIdentifier", "MonitoringLocationName", "MonitoringLocationTypeName", "TypeSimple", "State", "AU_ID_Final")

# pair down
# nd streams selec
mltoau_nd_streams <- mltoau_nd_streams_raw |>
  select(all_of(sel_cols))

# nd lakes
mltoau_nd_lakes <- mltoau_nd_lakes_raw |>
  select(all_of(sel_cols))

# ut streams
mltoau_ut_streams <- mltoau_ut_streams_raw |>
  select(all_of(sel_cols))

# ut lakes
mltoau_ut_lakes <- mltoau_ut_lakes_raw |>
  select(all_of(sel_cols))

# wy streams
mltoau_wy_streams <- mltoau_wy_streams_raw |>
  select(all_of(sel_cols))

# wy lakes
mltoau_wy_lakes <- mltoau_wy_lakes_raw |>
  select(all_of(sel_cols))


# merge datasets
mltoau_crosswalk_raw <- bind_rows(mltoau_nd_streams, mltoau_nd_lakes,
                           mltoau_ut_streams, mltoau_ut_lakes,
                           mltoau_wy_streams, mltoau_wy_lakes) |>
  mutate(AssessmentUnitIdentifier = case_when(AU_ID_Final == "-999999" ~ NA,
                                 .default = AU_ID_Final)) |>
  select(-AU_ID_Final)

# simplify
mltoau_crosswalk_simple <- mltoau_crosswalk_raw |>
  select(MonitoringLocationIdentifier, AssessmentUnitIdentifier)

# save data to package
# usethis::use_data(mltoau_crosswalk_raw, overwrite = TRUE)
usethis::use_data(mltoau_crosswalk_simple, overwrite = TRUE)

# ---- au to use crosswalk from attains ----
# this code was originally written by ben block

# load libraries
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
# library(rATTAINS)

# create results folder
# boo_Results <- dir.exists(file.path(wd, output.dir, results.dir))
# if(boo_Results==FALSE){
#   dir.create(file.path(wd, output.dir, results.dir))
# }

# cleanup
# rm(boo_Results)

# get data from the api
# EPA guide: https://www.epa.gov/sites/default/files/2020-10/documents/attains_how_to_access_web_services_2020-10-28.pdf

# use rATTAINS to help get values
# domain_vals_lookup <- rATTAINS::domain_values()
# domain_orgname_vals_lookup <- rATTAINS::domain_values(domain_name = "OrgName")

# function to pull data by state
get_state_attains_uses <- function(state_abrv = "ND") {
  
  # define url
  attains_url <- "https://attains.epa.gov/attains-public/api/assessments?state="
  
  # append state abbreviation
  temp_url <- paste0(attains_url, state_abrv)
  
  # make the get request
  temp_response <- httr::GET(temp_url)
  
  # check if the request was successful
  if (status_code(temp_response) == 200) {
    
    # parse the json content
    temp_data <- content(temp_response, as = "text", encoding = "UTF-8")
    
    # flatten
    temp_parsed_data <- fromJSON(temp_data, flatten = TRUE)
    
    # View the structure of the parsed data
    # str(temp_parsed_data)
    
    # convert to a data frame
    temp_df <- as.data.frame(temp_parsed_data$items) # Adjust based on actual structure
    
    # check
    # head(temp_df)
    
    # pull out assessment data
    # unnest the assessments column
    temp_df_unnested <- temp_df |>
      unnest(assessments, names_sep = "_")
    
    temp_df_unnested_v2 <- temp_df_unnested |>
      unnest(assessments_useAttainments, names_sep = "_")
    
    # trim data
    temp_df_final <- temp_df_unnested_v2 |> 
      select(organizationIdentifier,
             organizationName,
             organizationTypeText,
             reportingCycleText,
             assessments_assessmentUnitIdentifier,
             assessments_useAttainments_useName,
             assessments_cycleLastAssessedText,
             assessments_yearLastMonitoredText) |> 
      distinct()
    
    # print
    print("Obtained data successfully.\n")
    
    # return
    return(temp_df_final)

  } else {
    
    # print
    print(paste0("Failed to retrieve data. Status code:", status_code(temp_response), "\n"))
    
    # return null
    return(NULL)
  }
}

# use function to get data state data
df_co <- get_state_attains_uses(state_abrv = "CO")
df_mt <- get_state_attains_uses(state_abrv = "MT")
df_nd <- get_state_attains_uses(state_abrv = "ND")
df_sd <- get_state_attains_uses(state_abrv = "SD")
df_ut <- get_state_attains_uses(state_abrv = "UT")
df_wy <- get_state_attains_uses(state_abrv = "WY")

# function to pull data by tribe
get_tribe_attains_uses <- function(tribe_abrv = "BLCKFEET") {
  
  # define url
  attains_url <- "https://attains.epa.gov/attains-public/api/assessments?organizationId="
  
  # append state abbreviation
  temp_url <- paste0(attains_url, tribe_abrv)
  
  # make the get request
  temp_response <- httr::GET(temp_url)
  
  # check if the request was successful
  if (status_code(temp_response) == 200) {
    
    # parse the json content
    temp_data <- content(temp_response, as = "text", encoding = "UTF-8")
    
    # flatten
    temp_parsed_data <- fromJSON(temp_data, flatten = TRUE)
    
    # View the structure of the parsed data
    # str(temp_parsed_data)
    
    # convert to a data frame
    temp_df <- as.data.frame(temp_parsed_data$items) # Adjust based on actual structure
    
    # check
    # head(temp_df)
    
    # pull out assessment data
    # unnest the assessments column
    temp_df_unnested <- temp_df |>
      unnest(assessments, names_sep = "_")
    
    temp_df_unnested_v2 <- temp_df_unnested |>
      unnest(assessments_useAttainments, names_sep = "_")
    
    # trim data
    temp_df_final <- temp_df_unnested_v2 |> 
      select(organizationIdentifier,
             organizationName,
             organizationTypeText,
             reportingCycleText,
             assessments_assessmentUnitIdentifier,
             assessments_useAttainments_useName,
             assessments_cycleLastAssessedText,
             assessments_yearLastMonitoredText) |> 
      distinct()
    
    # print
    print("Obtained data successfully.\n")
    
    # return
    return(temp_df_final)
    
  } else {
    
    # print
    print(paste0("Failed to retrieve data. Status code:", status_code(temp_response), "\n"))
    
    # return null
    return(NULL)
  }
}

# use function to get data state data
df_bf <- get_tribe_attains_uses(tribe_abrv = "BLCKFEET")
df_umu <- get_tribe_attains_uses(tribe_abrv = "UTEMTN")
# no other tribes in domain_orgname_vals_lookup

# tribes in r8
# Fort Peck
# Blackfeet
# Confederated Salish and Kootenai
# Northern Cheyenne
# Ute Mountain Ute
# Southern Ute


# ---- combine data into one long file ----
# combine data
autouse_crosswalk_raw <- bind_rows(df_co, df_mt, df_nd, df_sd, df_ut, df_wy, df_bf, df_umu) |>
  arrange(organizationIdentifier, assessments_assessmentUnitIdentifier, assessments_useAttainments_useName)

# finalize
autouse_crosswalk_simple <- autouse_crosswalk_raw |>
  select(AssessmentUnitIdentifier = assessments_assessmentUnitIdentifier,
         ATTAINS.UseName = assessments_useAttainments_useName) |>
  arrange(AssessmentUnitIdentifier, ATTAINS.UseName)

# get current date 
current_date <- format(Sys.Date(), "%Y%m%d")

# file name
raw_file_name <- paste0("r8_attains_autouse_long_", current_date , ".csv")
simple_file_name <- paste0("r8_attains_autouse_long_simple_", current_date , ".csv")

# export
# write_csv(x = autouse_crosswalk_raw, path = here::here("data-raw", raw_file_name))
write_csv(x = autouse_crosswalk_simple, path = here::here("data-raw", simple_file_name))

# for now just use ben's file b/c api is down
# autouse_crosswalk_raw <- read_csv(file = here::here("data-raw", "R8_ATTAINS_AU_Uses_Long_20250602.csv"))
# autouse_crosswalk_simple <- autouse_crosswalk_raw |>
#   select(AssessmentUnitIdentifier = assessments_assessmentUnitIdentifier,
#          ATTAINS.UseName = assessments_useAttainments_useName) |>
#   arrange(AssessmentUnitIdentifier, ATTAINS.UseName)
# for hannah
# unique_r8_uses_for_hannah <- data.frame(ATTAINS.UseName = unique(autouse_crosswalk_simple$ATTAINS.UseName))
# write_csv(x = unique_r8_uses_for_hannah, file = "C:/Users/sheila.saia/OneDrive - Tetra Tech, Inc/proj 2025 epa region8/5_Work/Task_5/ATTAINS_AU_Info/Output_Data/ATTAINS_Output_20250602/unique_r8_uses_for_hannah.csv")

# save to package
# usethis::use_data(autouse_crosswalk_raw, overwrite = TRUE)
usethis::use_data(autouse_crosswalk_simple, overwrite = TRUE)


# ---- add documentation ----



# ---- old code: merge all crosswalks into one ----
# mltoautouse_crosswalk <- mltoau_crosswalk |>
#   left_join(autouse_crosswalk, by = "AssessmentUnitIdentifier", relationship = "many-to-many")

# save data to package
# usethis::use_data(mltoautouse_crosswalk, overwrite = TRUE)


# ---- old code: misc. -----
# define url
au_table_url <- "https://github.com/Blocktt/ShinyAppDocuments/raw/main/AUSpatialJoin/MonLoc_to_AU_Crosswalk_20250407.xlsx"

# convert to local temp file
temp_au_table <- tempfile(fileext = ".xlsx")

# pull data from url
httr::GET(au_table_url, httr::write_disk(temp_au_table))

# save to df
mltoau_df <- as.data.frame(readxl::read_excel(temp_au_table))

# final wrangling
mltoau_crosswalk <- mltoau_df |> 
  select(MonitoringLocationIdentifier, AU_ID, AU_NAME)

# save data to package
usethis::use_data(mltoau_crosswalk, overwrite = TRUE)