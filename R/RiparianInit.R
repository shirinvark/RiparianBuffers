RiparianInit <- function(sim) {
  ## ---------------------------------------------------------
  ## 0) CHECK inputs
  ## ---------------------------------------------------------
  stopifnot(inherits(sim$PlanningGrid, "SpatRaster"))
  stopifnot(inherits(sim$jurisdictionMap, "SpatRaster"))
  stopifnot(is.data.frame(sim$jurisdictionLookup))
  stopifnot(inherits(sim$Hydrology_streams, "SpatVector"))
  stopifnot(inherits(sim$Hydrology_lakes, "SpatVector"))
  stopifnot(is.data.frame(sim$riparianBufferPolicy))
  
  ## ---------------------------------------------------------
  ## 1) Policy
  ## ---------------------------------------------------------
  policy <- sim$riparianBufferPolicy
  
  reqCols <- c(
    "jurisdiction",
    "small_stream",
    "large_stream",
    "small_lake",
    "large_lake"
  )
  stopifnot(all(reqCols %in% names(policy)))
  
  ## ---------------------------------------------------------
  ## 2) Hydro template
  ## ---------------------------------------------------------
  hydro_template <- terra::rast(
    ext        = terra::ext(sim$PlanningGrid),
    resolution = P(sim)$hydroRaster_m,
    crs        = terra::crs(sim$PlanningGrid)
  )
  terra::values(hydro_template) <- NA_real_

 
  ## ---------------------------------------------------------
  ## 3) Classify hydrology
  ## ---------------------------------------------------------
  
  streams <- sim$Hydrology_streams
  
  if (!terra::same.crs(streams, sim$PlanningGrid)) {
    streams <- terra::project(
      streams,
      sim$PlanningGrid
    )
  }

  lakes <- sim$Hydrology_lakes
  
  if (!terra::same.crs(lakes, sim$PlanningGrid)) {
    lakes <- terra::project(
      lakes,
      sim$PlanningGrid
    )
  }
  
  streams$hydro_class <- ifelse(
    streams$ORD_STRA >= 4,
    "large_stream",
    "small_stream"
  )
  
  lakes$hydro_class <- ifelse(
    lakes$Lake_area >= 1,
    "large_lake",
    "small_lake"
  )
  ## ---------------------------------------------------------
  ## 3b) Handle empty hydrology (standalone safety)
  ## ---------------------------------------------------------
  if (nrow(streams) == 0 && nrow(lakes) == 0) {
    

    zero_rast <- sim$PlanningGrid
    terra::values(zero_rast) <- 0
    
    sim$Riparian <- list(
      riparianFraction = zero_rast,
      raster_m         = P(sim)$hydroRaster_m,
      policy           = policy
    )
    
    return(invisible(sim))
  }

  ## ---------------------------------------------------------
  ## 4) Jurisdiction raster
  ## ---------------------------------------------------------
  
  jurisRaster <- terra::resample(
    sim$jurisdictionMap,
    hydro_template,
    method = "near"
  )
  
  ## ---------------------------------------------------------
  ## 5) Build bufferRaster
  ## ---------------------------------------------------------
  
  bufferRaster <- hydro_template
  terra::values(bufferRaster) <- NA_real_
  
  juris_lookup <- sim$jurisdictionLookup
  
  ## Translate full jurisdiction names from jurisdictionLookup
  ## to the codes used in riparianBufferPolicy
  jurisdiction_codes <- c(
    "Ontario"                   = "ON",
    "Quebec"                    = "QC",
    "New Brunswick"             = "NB",
    "Nova Scotia"               = "NS",
    "Prince Edward Island"      = "PE",
    "Newfoundland and Labrador" = "NL"
  )
  
  for (i in seq_len(nrow(juris_lookup))) {
    
    j_id   <- juris_lookup$ID[i]
    j_name <- juris_lookup$jurisdiction[i]
    
    j_code <- unname(jurisdiction_codes[j_name])
    
    if (is.na(j_code)) {
      j_code <- "default"
    }
    
    row <- policy[policy$jurisdiction == j_code, ]
    
    if (nrow(row) == 0) {
      row <- policy[policy$jurisdiction == "default", ]
    }
    
    if (nrow(row) == 0) {
      stop(paste("No riparian policy for jurisdiction:", j_name))
    }
    
    ## small streams
    
    
    
    ## small streams
    # mask <- terra::rasterize(
    # streams[streams$hydro_class == "small_stream", ],
    # hydro_template,
    #touches = TRUE
    #)
    # mask <- terra::rasterize(
    #  streams[streams$hydro_class == "small_stream", ],
    # hydro_template,
    #field = 1,
    #touches = TRUE
    #)
    
    mask <- terra::rasterize(
      terra::buffer(
        streams[streams$hydro_class == "small_stream", ],
        width = 100
      ),
      hydro_template,
      field = 1,
      touches = TRUE
    )
    
  
    bufferRaster[jurisRaster == j_id & mask == 1] <- row$small_stream
   
    ## large streams
    # mask <- terra::rasterize(
    # streams[streams$hydro_class == "large_stream", ],
    # hydro_template,
    # touches = TRUE
    # )
    mask <- terra::rasterize(
      streams[streams$hydro_class == "large_stream", ],
      hydro_template,
      field = 1,
      touches = TRUE
    )
    bufferRaster[jurisRaster == j_id & mask == 1] <- row$large_stream
    
    ## small lakes
    #  mask <- terra::rasterize(
    #   lakes[lakes$hydro_class == "small_lake", ],
    # hydro_template,
    # touches = TRUE
    # )
    ## small lakes
    mask <- terra::rasterize(
      lakes[lakes$hydro_class == "small_lake", ],
      hydro_template,
      field = 1,
      touches = TRUE
    )
    
    bufferRaster[jurisRaster == j_id & mask == 1] <- row$small_lake
    
    ## large lakes
    # mask <- terra::rasterize(
    #  lakes[lakes$hydro_class == "large_lake", ],
    #hydro_template,
    # touches = TRUE
    #)
    ## large lakes
    mask <- terra::rasterize(
      lakes[lakes$hydro_class == "large_lake", ],
      hydro_template,
      field = 1,
      touches = TRUE
    )
    bufferRaster[jurisRaster == j_id & mask == 1] <- row$large_lake
  }
 
  ## ---------------------------------------------------------
  ## 5b) Handle empty bufferRaster (no buffers assigned)
  ## ---------------------------------------------------------
  #if (all(is.na(terra::values(bufferRaster)))) {
  nAssigned <- terra::global(
    !is.na(bufferRaster),
    "sum",
    na.rm = TRUE
  )[1, 1]
  
  if (nAssigned == 0) {

    zero_rast <- sim$PlanningGrid
    terra::values(zero_rast) <- 0
    
    sim$Riparian <- list(
      riparianFraction = zero_rast,
      raster_m         = P(sim)$hydroRaster_m,
      policy           = policy
    )
    
    return(invisible(sim))
  }
  ## --------------------------------------------------------
  ## 6) Riparian fraction
  ## ---------------------------------------------------------
  rip_frac <- buildRiparianFraction(
    PlanningGrid = sim$PlanningGrid,
    streams        = streams,
    lakes          = lakes,
    bufferRaster   = bufferRaster,
    hydroRaster_m  = P(sim)$hydroRaster_m
  )
  
  ## --------------------------------------------------------
  ## 7) Output
  ## -------------------------------------------------------
  sim$Riparian <- list(
    riparianFraction = rip_frac,
    raster_m         = P(sim)$hydroRaster_m,
    policy           = policy
  )
  
  invisible(sim)
}









