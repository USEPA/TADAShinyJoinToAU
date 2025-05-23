## code to prepare `mltoau_crosswalk` dataset goes here

# define url
au_table_url <- "https://github.com/Blocktt/ShinyAppDocuments/raw/main/AUSpatialJoin/MonLoc_to_AU_Crosswalk_20250407.xlsx"

# convert to local temp file
temp_au_table <- tempfile(fileext = ".xlsx")

# pull data from url
httr::GET(au_table_url, httr::write_disk(temp_au_table))

# save to df
mltoau_df <- as.data.frame(readxl::read_excel(temp_au_table))

# final wrangling
mltoau_crosswalk <- mltoau_df %>% 
  select(MonitoringLocationIdentifier, AU_ID, AU_NAME)

# save data to package
usethis::use_data(mltoau_crosswalk, overwrite = TRUE)