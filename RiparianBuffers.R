## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "RiparianBuffers",
  description = "Computes raster-based riparian influence (fractional) from upstream hydrology inputs.
No data download. No landbase decisions",
  keywords = c("hydrology", "riparian", "buffer"),
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(RiparianBuffers = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "RiparianBuffers.Rmd"),
  reqdPkgs = list(
    "PredictiveEcology/SpaDES.core@development (>= 2.1.8.9001)",
    "terra"
  )
  ,
  parameters = bindrows(
    defineParameter(
      "riparianBuffer_m",
      "numeric",
      30,
      0,
      NA,
      "Uniform riparian buffer distance (m) applied to hydrology streams"
    ),
    defineParameter(
      "riparianPolicy",
      "data.frame",
      NULL,
      NA,
      NA,
      "Province-based riparian buffer policy (columns: province, buffer_m)"
    ),
    
    defineParameter(
      "hydroRaster_m",
      "numeric",
      100,
      0,
      NA,
      "Resolution (m) used to compute proportional riparian fraction"
    )
    
  ),
  inputObjects = bindrows(
    
    expectsInput(
      objectName  = "PlanningRaster",
      objectClass = "SpatRaster",
      desc        = "Coarse-resolution planning raster supplied by upstream module",
      sourceURL  = NA
    ),
    ## Provinces are supplied by EasternCanadaDataPrep
    ## and are used ONLY to spatially apply province-specific
    ## riparian buffer policies (no landbase decisions here).
    expectsInput(
      objectName  = "Provinces",
      objectClass = "SpatVector",
      desc        = "Provincial boundaries with province_code"
    ),
    
    expectsInput(
      objectName  = "Hydrology",
      objectClass = "list",
      desc = "Hydrology inputs prepared upstream (EasternCanadaDataPrep); must contain element `streams` (SpatVector)"
    ),
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName  = "Riparian",
      objectClass = "list",
      desc        = "Proportional riparian fraction raster and metadata"
    )
  )
))
## Main event for RiparianBuffers.
## Translates jurisdiction-specific riparian policy
## into a spatially explicit buffer raster, then
## computes proportional riparian influence.

doEvent.RiparianBuffers<- function(sim, eventTime, eventType) {
  message(">>> NEW RiparianBuffers code <<<")
  switch(
    eventType,
    
    init = {
      
      ## ----------------------------------
      ## 1) Riparian policy
      ## ----------------------------------
      policy <- P(sim)$riparianPolicy
      
      if (is.null(policy)) {
        message("riparianPolicy not supplied; using default conservative 30 m baseline.")
        
        policy <- data.frame(
          province_code = c("BC","AB","SK","MB","ON","QC","NB","NS","NL","PE"),
          buffer_m = rep(300, 10),
          stringsAsFactors = FALSE
        )
      }
      
      ## ----------------------------------
      ## 2) Build hydro template (single source of truth)
      ## ----------------------------------
      hydro_template <- terra::rast(
        ext = terra::ext(sim$PlanningRaster),
        resolution = P(sim)$hydroRaster_m,
        crs = terra::crs(sim$PlanningRaster)
      )
      terra::values(hydro_template) <- NA_real_
      
      ## ----------------------------------
      ## 3) Rasterize provinces directly on hydro grid
      ## ----------------------------------
      # rasterize province_code as factor
      prov_r <- terra::rasterize(
        sim$Provinces,
        hydro_template,
        field = "province_code"
      )
      
      prov_r <- terra::as.factor(prov_r)
      
      # build lookup table
      lut <- data.frame(
        from = policy$province_code,
        to   = policy$buffer_m,
        stringsAsFactors = FALSE
      )
      
      # reclassify province raster → buffer distance raster
      bufferRaster <- terra::classify(
        prov_r,
        lut,
        others = NA_real_
      )
      
      
      ## ----------------------------------
      ## 4) Compute riparian fraction
      ## ----------------------------------
      rip_frac <- buildRiparianFraction(
        PlanningRaster = sim$PlanningRaster,
        streams        = sim$Hydrology$streams,
        bufferRaster   = bufferRaster,
        hydroRaster_m  = P(sim)$hydroRaster_m
      )
      
      ## ----------------------------------
      ## 5) Save output
      ## ----------------------------------
      sim$Riparian <- list(
        riparianFraction = rip_frac,
        raster_m         = P(sim)$hydroRaster_m,
        policy           = policy
      )
    }
  )
  
  invisible(sim)
}

## Compute riparian influence as a fractional raster.
##
## Two mutually exclusive modes are supported:
## 1) Uniform buffer distance applied everywhere (riparianBuffer_m)
## 2) Spatially variable buffer distances supplied as a raster (bufferRaster),
##    typically derived from jurisdiction-specific policy.
## Core riparian influence engine.
## Designed to be policy-agnostic and reusable
## across different regulatory or ecological contexts.

buildRiparianFraction <- function(
    PlanningRaster,
    streams,
    riparianBuffer_m = NULL,   # buffer ثابت (اختیاری)
    bufferRaster     = NULL,   # buffer متغیر (اختیاری)
    hydroRaster_m    = 100
) {
  ## Enforce a single buffering strategy:
  ## either uniform (riparianBuffer_m) OR
  ## spatially variable (bufferRaster), but never both.
  
  # --- sanity check ---
  if (is.null(streams)) {
    stop("Hydrology$streams is missing. Run EasternCanadaDataPrep before RiparianBuffers
.")
  }
  
  if (!inherits(streams, "SpatVector")) {
    stop("Hydrology$streams must be a SpatVector.")
  }
  
  if (is.null(riparianBuffer_m) && is.null(bufferRaster)) {
    stop("Either riparianBuffer_m or bufferRaster must be provided.")
  }
  
  if (!is.null(riparianBuffer_m) && !is.null(bufferRaster)) {
    stop("Provide only one of riparianBuffer_m or bufferRaster, not both.")
  }
  
  # --- CRS consistency ---
  if (!terra::same.crs(streams, PlanningRaster)) {
    streams <- terra::project(streams, PlanningRaster)
  }
  # high-resolution template (shared)
  hydro_template <- terra::rast(
    ext = terra::ext(PlanningRaster),
    resolution = hydroRaster_m,
    crs = terra::crs(PlanningRaster)
  )
  terra::values(hydro_template) <- NA_real_
  
  
  
  ## Internal high-resolution raster used to compute
  ## proportional riparian influence.
  ##
  ## Resolution may differ from PlanningRaster to
  ## better capture narrow hydrological features.
  ## Performance note:
  ## hydroRaster_m controls the trade-off between
  ## spatial accuracy and computational cost.
  ## This is intentionally decoupled from PlanningRaster
  # =========================================================
  # CASE 1: UNIFORM BUFFER (رفتار فعلی – بدون تغییر)
  # =========================================================
  ## Uniform buffer case:
  ## applies a single buffer distance to all streams.
  ## This preserves legacy behaviour and provides
  ## a simple baseline for testing and comparison.
  
  if (!is.null(riparianBuffer_m)) {
    
    streams_buf <- terra::buffer(streams, width = riparianBuffer_m)
    
    riparian_fraction <- terra::rasterize(
      streams_buf,
      hydro_template,
      cover = TRUE,
      background = 0
    )
    
    return(riparian_fraction)
  }
  
  #Case 2 =========================================================
  # aligned high-resolution template
  
  # CASE 2 =========================================================
  
  dist_r <- terra::distance(hydro_template, streams)
  
  rip_hi <- terra::ifel(
    dist_r <= bufferRaster,
    1,
    0
  )
  
  fact <- ceiling(res(PlanningRaster)[1] / hydroRaster_m)
  
  riparian_fraction <- terra::aggregate(
    rip_hi,
    fact = fact,
    fun  = mean,
    na.rm = TRUE
  )
  
  riparian_fraction <- terra::resample(
    riparian_fraction,
    PlanningRaster,
    method = "near"
  )
  
  riparian_fraction[is.na(riparian_fraction)] <- 0
  riparian_fraction <- pmin(pmax(riparian_fraction, 0), 1)
  
  return(riparian_fraction)
}
## This module does not create or download inputs.
## All spatial dependencies are expected to be
## supplied by EasternCanadaDataPrep or the user.
.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
## ------------------------------------------------------------------
## Module philosophy:
## RiparianBuffers translates hydrological geometry
## and jurisdictional policy into a continuous spatial signal,
## without embedding management or landbase assumptions.
## ------------------------------------------------------------------

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}

