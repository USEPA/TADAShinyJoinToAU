TADA_GetATTAINSAUMLCrosswalk <- function(org_id = NULL, batch_upload = FALSE) {
  # get reference df of all organization ids
  org.ref <- TADA_GetATTAINSOrgIDsRef()
  
  # check to see if org_id is not NULL
  if (!is.null(org_id)) {
    # check to make sure organization ids supplied by user match those in ATTAINS
    if (all(!org_id %in% org.ref$code)) {
      # remove intermediate objects
      rm(org.ref)
      
      # stop function if organization ids are not in ATTAINS
      stop(paste0(
        "TADA_GetATTAINSAUMLCrosswalk: ",
        "organization identifier(s) entered by user not found in ATTAINS."
      ))
    }
  }
  
  # if org_id is NULL return the AU/ML national extract, otherwise query by org_id
  if (is.null(org_id)) {
    au.info <- spsUtil::quiet(rExpertQuery::EQ_NationalExtract("au_mls"))
  } else {
    au.info <- spsUtil::quiet(rExpertQuery::EQ_AUsMLs(
      org_id = org_id,
      api_key = "lfzVzpwIlKS1O4l1QmbOLUeTzxyql4QdbHVR5Yf5"
    ))
  }
  
  # select, filter and rename crosswalk columns
  au.crosswalk <- au.info %>%
    dplyr::select(
      monitoringLocationId,
      monitoringLocationOrgId,
      assessmentUnitId,
      monitoringLocationDataLink,
      waterType,
      organizationId
    ) %>%
    dplyr::filter(
      !is.na(monitoringLocationId),
      monitoringLocationId != ""
    ) %>%
    dplyr::distinct() %>%
    dplyr::rename(
      ATTAINS.AssessmentUnitIdentifier = assessmentUnitId,
      MonitoringLocationIdentifier = monitoringLocationId,
      OrganizationIdentifier = monitoringLocationOrgId,
      MonitoringDataLinkText = monitoringLocationDataLink,
      ATTAINS.WaterType = waterType,
      ATTAINS.OrganizationIdentifier = organizationId
    ) %>%
    dplyr::rename(
      ATTAINS.MonitoringLocationIdentifier = MonitoringLocationIdentifier,
      ATTAINS.MonitoringDataLinkText = MonitoringDataLinkText
    ) %>%
    dplyr::select(
      OrganizationIdentifier,
      ATTAINS.OrganizationIdentifier,
      ATTAINS.MonitoringLocationIdentifier,
      ATTAINS.AssessmentUnitIdentifier,
      ATTAINS.MonitoringDataLinkText,
      ATTAINS.WaterType
    )
  
  # remove intermediate object
  rm(au.info)
  
  # if org_id is NULL, set to "all organizations" for printed message
  if (is.null(org_id)) {
    org_id <- "all organizations"
  }
  
  # check to see if the crosswalk contains any results
  if (length(au.crosswalk$ATTAINS.MonitoringLocationIdentifier > 0)) {
    # print a message describing the number of results
    print(paste0(
      "TADA_GetATTAINSAUMLCrosswalk: ",
      "There are ",
      nrow(au.crosswalk),
      " monitoring location identifiers associated with assessment units for ",
      org_id,
      " in ATTAINS."
    ))
    
    # if batch_upload is TRUE, create an ATTAINS formatted batch upload df
    if (batch_upload == TRUE) {
      au.crosswalk <- au.crosswalk %>%
        dplyr::select(-ATTAINS.WaterType) %>%
        dplyr::select(-ATTAINS.OrganizationIdentifier) %>%
        dplyr::rename(
          ASSESSMENT_UNIT_ID = ATTAINS.AssessmentUnitIdentifier,
          MS_ORG_ID = ATTAINS.MonitoringLocationIdentifier,
          MS_LOCATION_ID = OrganizationIdentifier,
          MS_DATA_LINK = ATTAINS.MonitoringDataLinkText
        )
    }
    
    # print a message if no crosswalk is found
    if (length(au.crosswalk$ATTAINS.MonitoringLocationIdentifier) == 0) {
      print(paste0(
        "TADA_GetATTAINSAUMLCrosswalk: ",
        "No monitoring location identifiers were recorded in ATTAINS for ",
        org_id,
        " assessment units."
      ))
      
      rm(org.ref)
    }
    
    return(au.crosswalk)
  }
}