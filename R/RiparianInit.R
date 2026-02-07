RiparianInit <- function(sim) {
  
  ## ---------------------------------------------------------
  ## 0) CHECK inputs
  ## ---------------------------------------------------------
  stopifnot(inherits(sim$PlanningRaster, "SpatRaster"))
  stopifnot(inherits(sim$Provinces, "SpatVector"))
  stopifnot(inherits(sim$Hydrology_streams, "SpatVector"))
  stopifnot(inherits(sim$Hydrology_lakes, "SpatVector"))
  
  ## ---------------------------------------------------------
  ## 1) Riparian policy (province-level for now)
  ## ---------------------------------------------------------
  policy <- P(sim)$riparianPolicy
  
  if (is.null(policy)) {
    message(
      "riparianPolicy not supplied; using default boreal riparian buffer (30 m)."
    )
    policy <- data.frame(
      province_code = c("BC","AB","SK","MB","ON","QC","NB","NS","NL","PE"),
      buffer_m = rep(30, 10),
      stringsAsFactors = FALSE
    )
  }
  
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
  streams$stream_class <- ifelse(
    streams$ORD_STRA >= 4,
    "large_stream",
    "small_stream"
  )
  sim$Hydrology_streams <- streams
  
  ## --- lakes ---
  lakes <- sim$Hydrology_lakes
  lakes$lake_class <- ifelse(
    lakes$Lake_area >= 1,
    "large_lake",
    "small_lake"
  )
  sim$Hydrology_lakes <- lakes
  
  ## ---------------------------------------------------------
  ## 4) Province → buffer raster (still simple, by province only)
  ## ---------------------------------------------------------
  prov <- terra::merge(
    sim$Provinces,
    policy,
    by    = "province_code",
    all.x = TRUE
  )
  
  if (any(is.na(prov$buffer_m))) {
    stop("Some provinces have no buffer_m defined in riparianPolicy.")
  }
  
  bufferRaster <- terra::rasterize(
    prov,
    hydro_template,
    field = "buffer_m"
  )
  
  ## ---------------------------------------------------------
  ## 5) Build riparian fraction raster
  ## ---------------------------------------------------------
  rip_frac <- buildRiparianFraction(
    PlanningRaster = sim$PlanningRaster,
    streams        = sim$Hydrology_streams,
    lakes          = sim$Hydrology_lakes,
    bufferRaster   = bufferRaster,
    hydroRaster_m  = P(sim)$hydroRaster_m
  )
  
  ## ---------------------------------------------------------
  ## 6) Save outputs
  ## ---------------------------------------------------------
  sim$Riparian <- list(
    riparianFraction = rip_frac,
    raster_m         = P(sim)$hydroRaster_m,
    policy           = policy
  )
  
  invisible(sim)
}
