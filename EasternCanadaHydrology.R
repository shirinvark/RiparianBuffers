## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "EasternCanadaHydrology",
  description = "Computes raster-based riparian influence (fractional) from upstream hydrology inputs.
No data download. No landbase decisions",
  keywords = c("hydrology", "riparian", "buffer"),
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(EasternCanadaHydrology = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "EasternCanadaHydrology.Rmd"),
  reqdPkgs = list("PredictiveEcology/SpaDES.core@development (>= 2.1.8.9001)"),
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
    
    expectsInput(
      objectName  = "Hydrology",
      objectClass = "list",
      desc        = "Hydrology inputs prepared upstream (must contain `streams`)",
      sourceURL  = NA
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

doEvent.EasternCanadaHydrology <- function(sim, eventTime, eventType) {
  
  switch(
    eventType,
    
    init = {
      sim$Riparian <- list(
        riparianFraction = buildRiparianFraction(
          PlanningRaster   = sim$PlanningRaster,
          streams          = sim$Hydrology$streams,
          riparianBuffer_m = P(sim)$riparianBuffer_m,
          hydroRaster_m    = P(sim)$hydroRaster_m
        ),
        buffer_m = P(sim)$riparianBuffer_m,
        raster_m = P(sim)$hydroRaster_m
      )
    }
  )
  
  invisible(sim)
}


buildRiparianFraction <- function(
    PlanningRaster,
    streams,
    riparianBuffer_m = NULL,   # buffer ثابت (اختیاری)
    bufferRaster     = NULL,   # buffer متغیر (اختیاری)
    hydroRaster_m    = 100
) {
  
  # --- sanity check ---
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
  
  # --- raster template ---
  hydro_template <- terra::rast(
    terra::ext(PlanningRaster),
    resolution = hydroRaster_m,
    crs = terra::crs(PlanningRaster)
  )
  terra::values(hydro_template) <- 0
  
  # =========================================================
  # CASE 1: UNIFORM BUFFER (رفتار فعلی – بدون تغییر)
  # =========================================================
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
  
  # =========================================================
  # CASE 2: VARIABLE BUFFER (برای آینده)
  # =========================================================
  bufferRaster <- terra::resample(bufferRaster, hydro_template, method = "near")
  
  dist_r <- terra::distance(hydro_template, streams)
  
  riparian_fraction <- terra::ifel(
    dist_r <= bufferRaster,
    1,
    0
  )
  
  return(riparian_fraction)
}

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

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}

