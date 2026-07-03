RiparianInit <- function(sim) {
  message("DEBUG: ENTERING RIPARIAN INIT")
  ## ---------------------------------------------------------
  ## 0) CHECK inputs
  ## ---------------------------------------------------------
  stopifnot(inherits(sim$PlanningGrid, "SpatRaster"))
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
    ext        = terra::ext(sim$PlanningGrid),
    resolution = P(sim)$hydroRaster_m,
    crs        = terra::crs(sim$PlanningGrid)
  )
  terra::values(hydro_template) <- NA_real_
  message("DEBUG hydro template extent")
  
  print(
    terra::ext(hydro_template)
  )
  ## ---------------------------------------------------------
  ## 3) Classify hydrology
  ## ---------------------------------------------------------
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
  message("DEBUG n streams = ", nrow(streams))
  
  print(terra::ext(streams))
  lakes <- sim$Hydrology_lakes
  
  if (!terra::same.crs(lakes, sim$PlanningGrid)) {
    lakes <- terra::project(
      lakes,
      sim$PlanningGrid
    )
    print(crs(streams))
    print(ext(streams))
  }
  
  streams$hydro_class <- ifelse(
    streams$ORD_STRA >= 4,
    "large_stream",
    "small_stream"
  )
  print(
    table(streams$hydro_class)
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
    
    message("⚠ No hydrology features inside studyArea. Returning zero riparian raster.")
    
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
  jurisRaster <- terra::rasterize(
    sim$jurisdiction,
    hydro_template,
    field = "jurisdiction"
  )
  
  message(
    "DEBUG juris cells = ",
    terra::global(
      !is.na(jurisRaster),
      "sum",
      na.rm = TRUE
    )[1,1]
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
    
    print(mask)
    print(
      streams[streams$hydro_class == "small_stream", ]
    )
    message(
      "DEBUG small stream cells = ",
      terra::global(
        !is.na(mask),
        "sum",
        na.rm = TRUE
      )[1,1]
    )
    bufferRaster[jurisRaster == j & mask == 1] <- row$small_stream
    message(
      "DEBUG assigned cells = ",
      terra::global(
        !is.na(bufferRaster),
        "sum",
        na.rm = TRUE
      )[1,1]
    )
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
    bufferRaster[jurisRaster == j & mask == 1] <- row$large_stream
    
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
    
    bufferRaster[jurisRaster == j & mask == 1] <- row$small_lake
    
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
    print(mask)
    bufferRaster[jurisRaster == j & mask == 1] <- row$large_lake
  }
  message(
    "DEBUG bufferRaster cells after loop = ",
    terra::global(
      !is.na(bufferRaster),
      "sum",
      na.rm = TRUE
    )[1,1]
  )
  
  print(
    terra::freq(bufferRaster)
  )
  ## ---------------------------------------------------------
  ## 5b) Handle empty bufferRaster (no buffers assigned)
  ## ---------------------------------------------------------
  if (all(is.na(terra::values(bufferRaster)))) {
    
    message("⚠ No valid buffer distances assigned. Returning zero riparian raster.")
    
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









