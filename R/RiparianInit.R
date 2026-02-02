RiparianInit <- function(sim) {
  
  ## --- CHECK inputs ---
  stopifnot(inherits(sim$PlanningRaster, "SpatRaster"))
  stopifnot(inherits(sim$Provinces, "SpatVector"))
  stopifnot(inherits(sim$Hydrology_streams, "SpatVector"))
  stopifnot(inherits(sim$Hydrology_lakes, "SpatVector"))
  
  ## 1) Riparian policy
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
  
  ## 2) Hydro template
  hydro_template <- terra::rast(
    ext = terra::ext(sim$PlanningRaster),
    resolution = P(sim)$hydroRaster_m,
    crs = terra::crs(sim$PlanningRaster)
  )
  terra::values(hydro_template) <- NA_real_
  
  ## 3) Province → buffer raster
  prov <- terra::merge(sim$Provinces, policy, by = "province_code", all.x = TRUE)
  
  if (any(is.na(prov$buffer_m))) {
    stop("Some provinces have no buffer_m defined in riparianPolicy.")
  }
  
  bufferRaster <- terra::rasterize(
    prov,
    hydro_template,
    field = "buffer_m"
  )
  
  ## 4) Riparian fraction (💎 untouched logic)
  rip_frac <- buildRiparianFraction(
    PlanningRaster = sim$PlanningRaster,
    streams        = sim$Hydrology_streams,
    lakes          = sim$Hydrology_lakes,
    bufferRaster   = bufferRaster,
    hydroRaster_m  = P(sim)$hydroRaster_m
  )
  
  ## 5) Save output
  sim$Riparian <- list(
    riparianFraction = rip_frac,
    raster_m         = P(sim)$hydroRaster_m,
    policy           = policy
  )
  
  invisible(sim)
}
