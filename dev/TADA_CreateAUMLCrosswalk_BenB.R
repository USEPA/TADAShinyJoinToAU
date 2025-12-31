TADA_CreateAUMLCrosswalk <- function(
    .data,
    au_ref = NULL,
    org_id = NULL,
    fill_ATTAINS_catch = FALSE,
    fill_USGS_catch = FALSE,
    return_nearest = TRUE,
    batch_upload = TRUE
) {
  # create list where all user matches dfs are set to NULL
  user.matches <- list(
    "TADA_with_ATTAINS" = NULL,
    "ATTAINS_catchments" = NULL,
    "ATTAINS_points" = NULL,
    "ATTAINS_lines" = NULL,
    "ATTAINS_polygons" = NULL
  )
  
  # check to see if user supplied ref is NULL
  if (is.null(au_ref)) {
    print(paste0(
      "TADA_CreateAUMLCrosswalk: no au_ref (user-supplied crosswalk ",
      "was provided."
    ))
  }
  
  # check to see if user supplied ref is not NULL
  if (!is.null(au_ref)) {
    # check to see if user supplied ref is not a data frame
    if (!is.data.frame(au_ref)) {
      # stop function with printed message if the user supplied ref is not a data frame
      stop(paste0(
        "TADA_CreateAUMLCrosswalk: The user supplied au_ref must be a data frame ",
        "containing the columns AssessmentUnitIdentifier, MonitoringLocationIdentifier, and ATTAINS.WaterType.",
        "MonitoringLocationIdentifiers must be WQP compatible."
      ))
    }
    
    # check to see if user supplied ref is a data frame
    if (is.data.frame(au_ref)) {
      print(paste0(
        "TADA_CreateAUMLCrosswalk: fetching ATTAINS geospatial data ",
        "for assessment units in the user-supplied crosswalk."
      ))
      
      # list of partial string matches for columns in au_ref
      req.cols <- c(
        "AssessmentUnitIdentifier",
        "MonitoringLocationIdentifier",
        "WaterType"
      )
      
      # get column names by using internal function checkColName (in Utilities.R)
      col.ids <- purrr::map_dfr(req.cols, ~ checkColName(au_ref, .x))
      
      # assign values to col.id variables - might be possible to rewrite with purrr function (HRM 9/8/25)
      # assign assessment unit id
      assign(col.ids$col.id[1], col.ids$select.col[1])
      
      # assign monitoring location identifier
      assign(col.ids$col.id[2], col.ids$select.col[2])
      
      # assign water type
      assign(col.ids$col.id[3], col.ids$select.col[3])
      
      # rename au_ref cols for next function
      au_ref <- au_ref %>%
        dplyr::rename(
          ATTAINS.MonitoringLocationIdentifier = paste0(ml.col),
          ATTAINS.AssessmentUnitIdentifier = paste0(auid.col),
          User.WaterType = paste0(type.col)
        )
      
      rm(col.ids, req.cols, auid.col, ml.col, type.col)
      
      # subset data for au_ref
      au.ref.mls <- .data %>%
        dplyr::filter(
          TADA.MonitoringLocationIdentifier %in%
            au_ref$ATTAINS.MonitoringLocationIdentifier
        ) %>%
        dplyr::mutate(TADA.AURefSource = "User-supplied Ref")
      
      if (dim(au.ref.mls)[1] > 0) {
        # get geospatial data for au_ref monitoring locations
        user.matches <- spsUtil::quiet(
          TADA_GetATTAINSByAUID(
            au.ref.mls,
            au_ref = au_ref,
            fill_ATTAINS_catch = fill_ATTAINS_catch
          )
        )
      }
    }
  }
  
  # ATTAINS supplied ref section
  # get attains crosswalk
  
  attains.matches <- list(
    "TADA_with_ATTAINS" = NULL,
    "ATTAINS_catchments" = NULL,
    "ATTAINS_points" = NULL,
    "ATTAINS_lines" = NULL,
    "ATTAINS_polygons" = NULL
  )
  
  # if no org id is provided, no crosswalk is imported from ATTAINS
  if (is.null(org_id)) {
    print(
      "TADA_CreateAUMLCrosswalk: No org_id provided. No crosswalk will be imported from ATTAINS."
    )
  }
  
  # if an org id is provided, ATTAINS is checked for a crosswalk
  if (!is.null(org_id)) {
    print("TADA_CreateAUMLCrosswalk: checking for crosswalk in ATTAINS.")
    
    attains.cw <- spsUtil::quiet(
      TADA_GetATTAINSAUMLCrosswalk(org_id = org_id)
    )
    
    if (is.null(attains.cw)) {
      print(paste0(
        "TADA_CreateAUMLCrosswalk: There are no MonitoringLocation records ",
        "in ATTAINS for ",
        org_id,
        "."
      ))
    }
  }
  
  if (!is.null(attains.cw)) {
    print("TADA_CreateAUMLCrosswalk: crosswalk from ATTAINS has been imported.")
    
    # we could remove or make this step optional, but it is helpful for making sure
    # monitoring location identifiers are WQP compatible
    attains.cw <- spsUtil::quiet(
      TADA_UpdateATTAINSAUMLCrosswalk(
        crosswalk = attains.cw,
        org_id = org_id,
        attains_replace = TRUE
      )
    )
    
    # if au_ref was provided  by user, remove any records with monitoring locations matching user ref
    if (!is.null(au_ref)) {
      attains.cw.mls <- .data %>%
        dplyr::filter(
          !TADA.MonitoringLocationIdentifier %in%
            au.ref.mls$TADA.MonitoringLocationIdentifier,
          TADA.MonitoringLocationIdentifier %in%
            attains.cw$ATTAINS.MonitoringLocationIdentifier
        )
    }
    
    # if au_ref was not provided  by user, retain all records that match ATTAINS ref
    if (is.null(au_ref)) {
      attains.cw.mls <- .data %>%
        dplyr::filter(
          TADA.MonitoringLocationIdentifier %in%
            attains.cw$ATTAINS.MonitoringLocationIdentifier
        )
    }
    
    # add source column for ATTAINS Crosswalk matched records
    attains.cw.mls <- attains.cw.mls %>%
      dplyr::mutate(TADA.AURefSource = "ATTAINS Crosswalk")
    
    print(paste0(
      "TADA_CreateAUMLCrosswalk: fetching ATTAINS geospatial data ",
      "for assessment units from the ATTAINS crosswalk."
    ))
    # get geospatial data for attains cw monitoring locations
    attains.matches <- spsUtil::quiet(
      TADA_GetATTAINSByAUID(
        attains.cw.mls,
        au_ref = attains.cw,
        fill_ATTAINS_catch = fill_ATTAINS_catch
      )
    )
    
    # remove intermediate objects
    rm(attains.cw)
  }
  
  # TADA_CreateATTAINSAUMLCrosswalk section
  
  print(paste0(
    "TADA_CreateAUMLCrosswalk: checking to see if any unmatched ",
    "monitoring locations remain in the original TADA data frame."
  ))
  
  get.attains.mls <- .data
  
  if (!is.null(attains.matches$TADA_with_ATTAINS)) {
    get.attains.mls <- get.attains.mls %>%
      dplyr::filter(
        !TADA.MonitoringLocationIdentifier %in%
          attains.cw.mls$TADA.MonitoringLocationIdentifier
      )
  }
  
  if (!is.null(user.matches$TADA_with_ATTAINS)) {
    get.attains.mls <- get.attains.mls %>%
      dplyr::filter(
        !TADA.MonitoringLocationIdentifier %in%
          au.ref.mls$TADA.MonitoringLocationIdentifier
      )
  }
  
  # add code here for if there are no remaining mls to match
  if (dim(get.attains.mls)[1] == 0) {
    print(paste0(
      "TADA_CreateAUMLCrosswalk: all monitorintg locations have ",
      "already been matched to an assessment unit by the user or ATTAINS."
    ))
    
    get.attains.matches <- list(
      "TADA_with_ATTAINS" = NULL,
      "ATTAINS_catchments" = NULL,
      "ATTAINS_points" = NULL,
      "ATTAINS_lines" = NULL,
      "ATTAINS_polygons" = NULL
    )
  }
  
  if (dim(get.attains.mls)[1] > 0) {
    print(
      "TADA_CreateAUMLCrosswalk: using TADA_CreateATTAINSAUMLCrosswalk to match remaining monitoring locations to ATTAINS assessment units using a spatial join (EPA snapshot of NHDPlus HR catchments associated with entity submitted assessment unit features). Also returning USGS snapshot of NHDPlus V2 HR for monitoring locations not near any ATTAINS assessment unit."
    )
    
    # add source ref column for TADA_CreateATTAINSAUMLCrosswalk matches
    get.attains.mls <- get.attains.mls %>%
      dplyr::mutate(TADA.AURefSource = "TADA_CreateATTAINSAUMLCrosswalk")
    
    # use get attains for matching remaining monitoring locations
    get.attains.matches <- spsUtil::quiet(
      TADA_CreateATTAINSAUMLCrosswalk(
        get.attains.mls,
        return_nearest = return_nearest,
        fill_USGS_catch = fill_USGS_catch,
        return_sf = TRUE
      )
    )
  }
  
  # remove intermediate objects
  rm(attains.cw.mls, au.ref.mls, get.attains.mls)
  
  # join all the resulting tables within each list to return as one large list
  # TADA_with_ATTAINS
  
  print(
    "TADA_CreateAUMLCrosswalk: joining results to return list of dataframes compatible with TADA_ViewATTAINS."
  )
  
  # internal function to prep output by binding rows from different crosswalk sources
  outputPrep <- function(df.name, user, attains, get.attains) {
    user <- user[[df.name]]
    attains <- attains[[df.name]]
    get.attains <- get.attains[[df.name]]
    
    df <- user %>%
      plyr::rbind.fill(attains) %>%
      plyr::rbind.fill(get.attains) %>%
      dplyr::distinct() %>%
      sf::st_as_sf()
    
    return(df)
  }
  
  # create TADA_with_ATTAINS for output list
  TADA_with_ATTAINS <- outputPrep(
    df.name = "TADA_with_ATTAINS",
    user = user.matches,
    attains = attains.matches,
    get.attains = get.attains.matches
  )
  
  # create ATTAINS_catchments for output list
  ATTAINS_catchments <- outputPrep(
    df.name = "ATTAINS_catchments",
    user = user.matches,
    attains = attains.matches,
    get.attains = get.attains.matches
  )
  
  # create ATTAINS_lines for output list
  ATTAINS_lines <- outputPrep(
    df.name = "ATTAINS_lines",
    user = user.matches,
    attains = attains.matches,
    get.attains = get.attains.matches
  )
  
  # create ATTAINS_points for output list
  ATTAINS_points <- outputPrep(
    df.name = "ATTAINS_points",
    user = user.matches,
    attains = attains.matches,
    get.attains = get.attains.matches
  )
  
  # create ATTAINS_polygons for output list
  ATTAINS_polygons <- outputPrep(
    df.name = "ATTAINS_polygons",
    user = user.matches,
    attains = attains.matches,
    get.attains = get.attains.matches
  )
  
  # create ATTAINS_crosswalk for output list
  ATTAINS_crosswalk <- TADA_with_ATTAINS %>%
    sf::st_drop_geometry() %>%
    dplyr::select(
      TADA.MonitoringLocationIdentifier,
      ATTAINS.AssessmentUnitIdentifier,
      ATTAINS.WaterType,
      TADA.AURefSource
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(ATTAINS.OrganizationIdentifier = org_id) %>%
    dplyr::filter(!is.na(ATTAINS.AssessmentUnitIdentifier))
  
  # create final output list of all dfs
  final_list <- list(
    "TADA_with_ATTAINS" = TADA_with_ATTAINS,
    "ATTAINS_catchments" = ATTAINS_catchments,
    "ATTAINS_points" = ATTAINS_points,
    "ATTAINS_lines" = ATTAINS_lines,
    "ATTAINS_polygons" = ATTAINS_polygons,
    "ATTAINS_crosswalk" = ATTAINS_crosswalk
  )
  
  # add batch upload df to list for output if user has selected this option
  if (batch_upload == TRUE) {
    # create batch upload for ATTAINS df
    ATTAINS_batchupload <- TADA_with_ATTAINS %>%
      sf::st_drop_geometry() %>%
      dplyr::select(
        TADA.MonitoringLocationIdentifier,
        ATTAINS.AssessmentUnitIdentifier,
        OrganizationIdentifier
      ) %>%
      dplyr::distinct() %>%
      dplyr::rename(
        MS_LOCATION_ID = TADA.MonitoringLocationIdentifier,
        ASSESSMENT_UNIT_ID = ATTAINS.AssessmentUnitIdentifier,
        MS_ORG_ID = OrganizationIdentifier
      ) %>%
      dplyr::mutate(MS_DATA_LINK = NA) %>%
      dplyr::select(
        ASSESSMENT_UNIT_ID,
        MS_ORG_ID,
        MS_LOCATION_ID,
        MS_DATA_LINK
      ) %>%
      dplyr::filter(!is.na(ASSESSMENT_UNIT_ID))
    
    # add batch upload df to list for output
    final_list <- c(
      final_list,
      list("ATTAINS_batchupload" = ATTAINS_batchupload)
    )
    
    # remove intermediate objects
    rm(ATTAINS_batchupload)
  }
  
  # remove intermediate objects
  rm(
    TADA_with_ATTAINS,
    ATTAINS_catchments,
    ATTAINS_points,
    ATTAINS_lines,
    ATTAINS_polygons,
    ATTAINS_crosswalk
  )
  
  # add nhd catchments without ATTAINS matches if user has selected this option
  if (fill_USGS_catch == TRUE) {
    # add nhd catchment related dfs to output if required
    final_list <- c(
      final_list,
      list(
        "without_ATTAINS_catchments" = get.attains.matches$without_ATTAINS_catchment
      ),
      list(
        "TADA_without_ATTAINS" = get.attains.matches$TADA_without_ATTAINS
      )
    )
  }
  
  # remove intermediate objects
  rm(attains.matches, user.matches, get.attains.matches)
  
  # return final list of dfs based on user inputs
  return(final_list)
}