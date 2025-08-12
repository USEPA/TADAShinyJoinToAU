# ---- header ----
# 
# author: sheila saia
# date created: 2025-05-10
# email: sheila.saia@tetratech.com
#
# script name: scratch_script.R
# 
# 
# script description: 
# 
#
# ---- notes: ----
#
#
#
# ---- to do: ----
#
#
#
# ---- load libraries ----
# library(tidyverse)
# library(here)
# library(fs)
library(golem)
library(usethis)
library(devtools)
library(shiny)
library(htmltools)


# ---- golem project setup ----

# use this very helpful reference/ebook
# https://engineering-shiny.org/index.html

# 1. cloned empty repo from git

# 2. run git init in command line

# 3. added gitignore and scratch folder (ignored scratch folder for now)

# 4. run the code below
proj_path <- fs::path_tidy("C:/Users/sheila.saia/OneDrive - Tetra Tech, Inc/Documents/github/TADAShinyJoinToAU")
golem::create_golem(proj_path, overwrite = TRUE)

# 5. add readme, contributing, and license files
# used command line to do this but you can add them with usethis::use_readme_rmd(open = FALSE)
# there several other usethis functions to make license files, etc.

# 6. set options in description file and other setup files
golem::set_golem_options()
fs::dir_tree()

# 7. fill description file
# see dev > 01_start.R

# 8. add utility functions
golem::use_utils_ui()
golem::use_utils_server()

# 9. walk through lines of code in dev > 01_start.R

# 10. walk through lines of code in dev > 02_dev.R

# 11. to create a module
# golem::add_module(name = "my_first_module")

# 12. to create a function
# golem::add_fct(name = "my_function")

# 13. to initialize a basic test file and open it for editing
# usethis::use_test()

# 14. add cross walk table to data
# create empty raw data (placeholder)
usethis::use_data_raw("mltoau_crosswalk")
# see mltoau_crosswalk.R script in data-raw for subsequent steps
# see https://brouwern.github.io/biodata/articles/x-adding_raw_data.html
# see https://engineering-shiny.org/common-app-caveats.html?q=use_data_raw#reading-data
# revised this to add other datsets...see mltoau_crosswalk.R

# modules: (1) load file, (2) join aus
# 15. create module to load file
golem::add_module(name = "load_file", with_test = TRUE, export = FALSE)

# 16. create join aus module
golem::add_module(name = "join_aus", with_test = TRUE, export = FALSE)

# 17. fill in ui and server in each module

# 18. amend golem config with global variables
golem::amend_golem_config(key = "MB_LIMIT", value = 200)
# use this in script get_golem_config("MB_LIMIT")

# 19. see run_dev.R for code on how to test your app

# 20. more info on reading data
# https://engineering-shiny.org/common-app-caveats.html?q=data#reading-data

# 21. create data download module
golem::add_module(name = "download_result", with_test = TRUE, export = FALSE)

# check in with yu-chen (notes to self)
# 1. sourcing data
# 2. defining namespaces
# 3. versioning...

# 22. 

# ---- reactive list ----
# following "stratégie du petit r" described in:
# https://engineering-shiny.org/structuring-project.html#communication-between-modules
# TADAShiny module 1 uses this approach
# tadat is name of the reactive values list

# app server
# tadat: "default_outfile", "job_id"

# mod_load_file
# inputs: "input_file", "separator"
# outputs: "input_file_display", "df_import_dt"
# tadat: "df_ml_input"

# mod_join_aus
# inputs: "join_calc", "mlid_choice", 
# outputs: "join_map", "df_results_dt"
# tadat: "df_mltoau_for_review", "df_autouse_for_review"

# mod_download_results
# inputs: none
# outputs: "download_results", 
# tadat: "temp_dir"


# ---- code testing ----
library(fs)
library(devtools)
library(tidyverse)

# test_file_path <- fs::path_tidy("C:/Users/sheila.saia/OneDrive - Tetra Tech, Inc/proj 2025 epa region8/5_Work/join2au_app/example_data_from_ben/TADA_Utah_Nutrients_trimsubset2_20250407.csv")
test_file_path <- fs::path_tidy("C:/Users/sheila.saia/OneDrive - Tetra Tech, Inc/proj 2025 epa region8/5_Work/join2au_app/Example_Data_ND/ND_Spirit.csv")
df_ml_data <- utils::read.delim(test_file_path,
                              header = TRUE,
                              sep = ",",
                              stringsAsFactors = FALSE,
                              na.strings = c("", "NA"))
df_ml_input <- df_ml_data

devtools::load_all()

df_mltoau_cw <-TADAShinyJoinToAU::mltoau_crosswalk_simple
df_autouse_cw <- TADAShinyJoinToAU::autouse_crosswalk_simple

# ND spirit lake attains pull for testing
# write_csv(df_ml_unmatched_attains, fs::path_tidy("C:/Users/sheila.saia/OneDrive - Tetra Tech, Inc/Desktop/r8_app_testing/df_ml_unmatched_attains.csv"))
df_ml_unmatched_attains <- read_csv(fs::path_tidy("C:/Users/sheila.saia/OneDrive - Tetra Tech, Inc/Desktop/r8_app_testing/df_ml_unmatched_attains.csv"))
