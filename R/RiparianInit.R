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
  ## 1) Riparian policy (from CSV, already loaded)
  ## ---------------------------------------------------------
  policy <- sim$riparianBufferPolicy
  
  stopifnot(
    all(c(
      "province_code",
      "small_stream",
      "large_stream",
      "small_lake",
      "large_lake"
    ) %in% names(policy))
  )
  
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
  ## 3) Classify hydrology (VECTOR LEVEL)
  ## ---------------------------------------------------------
  
  ## --- streams ---
  streams <- sim$Hydrology_streams
  stopifnot("ORD_STRA" %in% names(streams))
  
  streams$hydro_class <- ifelse(
    streams$ORD_STRA >= 4,
    "large_stream",
    "small_stream"
  )
  sim$Hydrology_streams <- streams
  
  ## --- lakes ---
  lakes <- sim$Hydrology_lakes
  stopifnot("Lake_area" %in% names(lakes))
  
  lakes$hydro_class <- ifelse(
    lakes$Lake_area >= 1,
    "large_lake",
    "small_lake"
  )
  sim$Hydrology_lakes <- lakes
  
  ## ---------------------------------------------------------
  ## 4) Province raster
  ## ---------------------------------------------------------
  stopifnot("province_code" %in% names(sim$Provinces))
  
  provRaster <- terra::rasterize(
    sim$Provinces,
    hydro_template,
    field = "province_code"
  )
  
  ## ---------------------------------------------------------
  ## 5) Build bufferRaster (SIMPLE LOOKUP)
  ## ---------------------------------------------------------
  bufferRaster <- hydro_template
  terra::values(bufferRaster) <- NA_real_
  
  prov_vals <- unique(na.omit(terra::values(provRaster)))
  
  for (p in prov_vals) {
    
    row <- policy[policy$province_code == p, ]
    
    if (nrow(row) == 0) {
      row <- policy[policy$province_code == "default", ]
    }
    
    ## 🔹 فعلاً ساده: فقط large_stream
    ## ---------------------------------------------------------
    ## 5) Build bufferRaster (PARTITIONED by hydro class)
    ## ---------------------------------------------------------
    
    bufferRaster <- hydro_template
    terra::values(bufferRaster) <- NA_real_
    
    prov_vals <- unique(na.omit(terra::values(provRaster)))
    
    for (p in prov_vals) {
      
      row <- policy[policy$province_code == p, ]
      if (nrow(row) == 0) {
        row <- policy[policy$province_code == "default", ]
      }
      
      ## --- small streams ---
      buf <- as.numeric(row$small_stream)
      bufferRaster[
        provRaster == p &
          terra::rasterize(
            sim$Hydrology_streams[sim$Hydrology_streams$hydro_class == "small_stream", ],
            hydro_template,
            touches = TRUE
          ) == 1
      ] <- buf
      
      ## --- large streams ---
      buf <- as.numeric(row$large_stream)
      bufferRaster[
        provRaster == p &
          terra::rasterize(
            sim$Hydrology_streams[sim$Hydrology_streams$hydro_class == "large_stream", ],
            hydro_template,
            touches = TRUE
          ) == 1
      ] <- buf
      
      ## --- small lakes ---
      buf <- as.numeric(row$small_lake)
      bufferRaster[
        provRaster == p &
          terra::rasterize(
            sim$Hydrology_lakes[sim$Hydrology_lakes$hydro_class == "small_lake", ],
            hydro_template,
            touches = TRUE
          ) == 1
      ] <- buf
      
      ## --- large lakes ---
      buf <- as.numeric(row$large_lake)
      bufferRaster[
        provRaster == p &
          terra::rasterize(
            sim$Hydrology_lakes[sim$Hydrology_lakes$hydro_class == "large_lake", ],
            hydro_template,
            touches = TRUE
          ) == 1
      ] <- buf
    }
    
  
  ## ---------------------------------------------------------
  ## 6) Build riparian fraction raster
  ## ---------------------------------------------------------
  rip_frac <- buildRiparianFraction(
    PlanningRaster = sim$PlanningRaster,
    streams        = sim$Hydrology_streams,
    lakes          = sim$Hydrology_lakes,
    bufferRaster   = bufferRaster,
    hydroRaster_m  = P(sim)$hydroRaster_m
  )
  
  ## ---------------------------------------------------------
  ## 7) Save outputs
  ## ---------------------------------------------------------
  sim$Riparian <- list(
    riparianFraction = rip_frac,
    raster_m         = P(sim)$hydroRaster_m,
    policy           = policy
  )
  
  invisible(sim)
}
