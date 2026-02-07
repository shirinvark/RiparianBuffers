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
  ## 1) Policy (from CSV)
  ## ---------------------------------------------------------
  policy <- sim$riparianBufferPolicy
  
  ## ---------------------------------------------------------
  ## 2) Hydro template raster
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
  ## 5) Build bufferRaster (PARTITIONED)
  ## ---------------------------------------------------------
  bufferRaster <- hydro_template
  terra::values(bufferRaster) <- NA_real_
  
  prov_codes <- unique(sim$Provinces$province_code)
  prov_codes <- as.character(prov_codes)
  
  for (p in prov_codes) {   # ✅ این خط اصلاح شد
    
    row <- policy[policy$province_code == p, ]
    if (nrow(row) < 1) {
      row <- policy[policy$province_code == "default", ]
    }
    
    ## small streams
    mask <- terra::rasterize(
      sim$Hydrology_streams[streams$hydro_class == "small_stream", ],
      hydro_template,
      touches = TRUE
    )
    bufferRaster[provRaster == p & mask == 1] <- as.numeric(row$small_stream)
    
    ## large streams
    mask <- terra::rasterize(
      sim$Hydrology_streams[streams$hydro_class == "large_stream", ],
      hydro_template,
      touches = TRUE
    )
    bufferRaster[provRaster == p & mask == 1] <- as.numeric(row$large_stream)
    
    ## small lakes
    mask <- terra::rasterize(
      sim$Hydrology_lakes[lakes$hydro_class == "small_lake", ],
      hydro_template,
      touches = TRUE
    )
    bufferRaster[provRaster == p & mask == 1] <- as.numeric(row$small_lake)
    
    ## large lakes
    mask <- terra::rasterize(
      sim$Hydrology_lakes[lakes$hydro_class == "large_lake", ],
      hydro_template,
      touches = TRUE
    )
    bufferRaster[provRaster == p & mask == 1] <- as.numeric(row$large_lake)
  }
  
  ## ---------------------------------------------------------
  ## 6) Riparian fraction
  ## ---------------------------------------------------------
  rip_frac <- buildRiparianFraction(
    PlanningRaster = sim$PlanningRaster,
    streams        = sim$Hydrology_streams,
    lakes          = sim$Hydrology_lakes,
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
