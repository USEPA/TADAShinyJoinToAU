TADA_GetATTAINSOrgIDsRef <- function() {
  # If there is a cached table available return it
  if (!is.null(ATTAINSOrgIDsRef_Cached)) {
    return(ATTAINSOrgIDsRef_Cached)
  }
  
  # Try to download up-to-date raw data
  
  raw.data <- tryCatch(
    {
      # get data from ATTAINS
      spsUtil::quiet(rExpertQuery::EQ_DomainValues("org_id"))
    },
    error = function(err) {
      NULL
    }
  )
  
  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest ATTAINS Organization Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file(
      "extdata",
      "ATTAINSOrgIDsRef.csv",
      package = "EPATADA"
    )))
  }
  
  ATTAINSOrgIDsRef <- raw.data %>%
    dplyr::distinct()
  
  # Save updated table in cache
  ATTAINSOrgIDsRef_Cached <- ATTAINSOrgIDsRef
  
  ATTAINSOrgIDsRef
}