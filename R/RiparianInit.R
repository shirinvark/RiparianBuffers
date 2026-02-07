RiparianInit <- function(sim) {
  
  ## ---------------------------------------------------------
  ## 0) CHECK inputs
  ## ---------------------------------------------------------
  stopifnot(inherits(sim$PlanningRaster, "SpatRaster"))
  stopifnot(inherits(sim$Provinces, "SpatVector"))
  stopifnot(inherits(sim$Hydrology_streams, "SpatVector"))
  stopifnot(inherits(sim$Hydrology_lakes, "SpatVector"))
  stopifnot(is.data.frame(sim$riparianBufferPolicy))
  
  ## ---------------------------------------------------------
  ## 1) Policy
  ## ---------------------------------------------------------
  policy <- sim$riparianBufferPolicy
  
  reqCols <- c(
    "province_code",
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
  sim$Hydrology_streams <- streams
  
  lakes <- sim$Hydrology_lakes
  lakes$hydro_class <- ifelse(
    lakes$Lake_area >= 1, "large_lake", "small_lake"
  )
  sim$Hydrology_lakes <- lakes
  
  ## ---------------------------------------------------------
  ## 4) Province raster
  ## ---------------------------------------------------------
  provRaster <- terra::rasterize(
    sim$Provinces,
    hydro_template,
    field = "province_code"
  )
  
  ## ---------------------------------------------------------
  ## 5) Build bufferRaster
  ## ---------------------------------------------------------
  bufferRaster <- hydro_template
  terra::values(bufferRaster) <- NA_real_
  
  prov_codes <- unique(na.omit(sim$Provinces$province_code))
  prov_codes <- as.character(prov_codes)
  
  for (p in prov_codes) {
    
    row <- policy[policy$province_code == p, ]
    
    if (nrow(row) == 0) {
      row <- policy[policy$province_code == "default", ]
    }
    
    if (nrow(row) == 0) {
      stop(paste("No riparian policy for province:", p))
    }
    
    ## small streams
    mask <- terra::rasterize(
      streams[streams$hydro_class == "small_stream", ],
      hydro_template,
      touches = TRUE
    )
    bufferRaster[provRaster == p & mask == 1] <- row$small_stream
    
    ## large streams
    mask <- terra::rasterize(
      streams[streams$hydro_class == "large_stream", ],
      hydro_template,
      touches = TRUE
    )
    bufferRaster[provRaster == p & mask == 1] <- row$large_stream
    
    ## small lakes
    mask <- terra::rasterize(
      lakes[lakes$hydro_class == "small_lake", ],
      hydro_template,
      touches = TRUE
    )
    bufferRaster[provRaster == p & mask == 1] <- row$small_lake
    
    ## large lakes
    mask <- terra::rasterize(
      lakes[lakes$hydro_class == "large_lake", ],
      hydro_template,
      touches = TRUE
    )
    bufferRaster[provRaster == p & mask == 1] <- row$large_lake
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
  
  ## ---------------------------------------------------------
  ## 7) Output
  ## ---------------------------------------------------------
  sim$Riparian <- list(
    riparianFraction = rip_frac,
    raster_m         = P(sim)$hydroRaster_m,
    policy           = policy
  )
  
  invisible(sim)
}
