RiparianInit <- function(sim) {
  
  ## ---------------------------------------------------------
  ## 0) CHECK inputs
  ## ---------------------------------------------------------
  stopifnot(inherits(sim$PlanningRaster, "SpatRaster"))
  stopifnot(inherits(sim$jurisdiction, "SpatVector"))
  stopifnot(inherits(sim$Hydrology_streams, "SpatVector"))
  stopifnot(inherits(sim$Hydrology_lakes, "SpatVector"))
  stopifnot(is.data.frame(sim$riparianBufferPolicy))
  ## ---------------------------------------------------------
  ## 0b) CHECK jurisdiction attributes
  ## ---------------------------------------------------------
  if (!"jurisdiction" %in% names(sim$jurisdiction)) {
    stop("sim$jurisdiction must contain a 'jurisdiction' attribute")
  }
  
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
    ext        = terra::ext(sim$PlanningRaster),
    resolution = P(sim)$hydroRaster_m,
    crs        = terra::crs(sim$PlanningRaster)
  )
  terra::values(hydro_template) <- NA_real_
  
  ## ---------------------------------------------------------
  ## 3) Classify hydrology
  ## ---------------------------------------------------------
  streams <- sim$Hydrology_streams
  streams$hydro_class <- ifelse(
    streams$ORD_STRA >= 4, "large_stream", "small_stream"
  )
  
  lakes <- sim$Hydrology_lakes
  lakes$hydro_class <- ifelse(
    lakes$Lake_area >= 1, "large_lake", "small_lake"
  )
  
  ## ---------------------------------------------------------
  ## 4) Jurisdiction raster
  ## ---------------------------------------------------------
  jurisRaster <- terra::rasterize(
    sim$jurisdiction,
    hydro_template,
    field = "jurisdiction"
  )
  
  
  ## ---------------------------------------------------------
  ## 5) Build bufferRaster
  ## ---------------------------------------------------------
  bufferRaster <- hydro_template
  terra::values(bufferRaster) <- NA_real_
  
  juris_codes <- unique(na.omit(sim$jurisdiction$jurisdiction))
  juris_codes <- as.character(juris_codes)
  
  for (j in juris_codes) {
    
    row <- policy[policy$jurisdiction == j, ]
    if (nrow(row) == 0) {
      row <- policy[policy$jurisdiction == "default", ]
    }
    if (nrow(row) == 0) {
      stop(paste("No riparian policy for jurisdiction:", j))
    }
    
    ## small streams
    mask <- terra::rasterize(
      streams[streams$hydro_class == "small_stream", ],
      hydro_template,
      touches = TRUE
    )
    bufferRaster[jurisRaster == j & mask == 1] <- row$small_stream
    
    ## large streams
    mask <- terra::rasterize(
      streams[streams$hydro_class == "large_stream", ],
      hydro_template,
      touches = TRUE
    )
    bufferRaster[jurisRaster == j & mask == 1] <- row$large_stream
    
    ## small lakes
    mask <- terra::rasterize(
      lakes[lakes$hydro_class == "small_lake", ],
      hydro_template,
      touches = TRUE
    )
    bufferRaster[jurisRaster == j & mask == 1] <- row$small_lake
    
    ## large lakes
    mask <- terra::rasterize(
      lakes[lakes$hydro_class == "large_lake", ],
      hydro_template,
      touches = TRUE
    )
    bufferRaster[jurisRaster == j & mask == 1] <- row$large_lake
  }
  
  ## ---------------------------------------------------------
  ## 6) Riparian fraction
  ## ---------------------------------------------------------
  rip_frac <- buildRiparianFraction(
    PlanningRaster = sim$PlanningRaster,
    streams        = streams,
    lakes          = lakes,
    bufferRaster   = bufferRaster,
    hydroRaster_m  = P(sim)$hydroRaster_m
  )
  
  ## --------------------------------------------------------
  ## 7) Output
  ## --------------------------------------------------------
  sim$Riparian <- list(
    riparianFraction = rip_frac,
    raster_m         = P(sim)$hydroRaster_m,
    policy           = policy
  )
  
  invisible(sim)
}
