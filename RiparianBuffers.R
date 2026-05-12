## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "RiparianBuffers",
  description = "Coarse-resolution PlanningGrid_250m supplied upstream (created internally if missing)",
  keywords = c("hydrology", "riparian", "buffer"),
  authors = structure(list(list(given = c("Shirin", "Middle"), family = "Varkouhi", role = c("aut", "cre"), email = "Shirin.varkuhi@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(RiparianBuffers = "0.1.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "RiparianBuffers.Rmd"),
  reqdPkgs = list(
    "PredictiveEcology/SpaDES.core@development (>= 2.1.8.9001)",
    "terra",
    "sf",
    "rnaturalearth",
    "reproducible"
  )
  ,
  parameters = bindrows(
    defineParameter(
      "hydroRaster_m",
      "numeric",
      30,
      0,
      NA,
      "Resolution (m) used to compute proportional riparian fraction"
    )
  ),
  inputObjects = bindrows(
    
    expectsInput(
      objectName  = "studyArea",
      objectClass = c("SpatVector", "sf"),
      desc        = "Study area polygon used to crop spatial inputs",
      sourceURL   = NA
    ),
    
    expectsInput(
      objectName  = "PlanningGrid_250m",
      objectClass = "SpatRaster",
      desc        = "Coarse-resolution PlanningGrid_250m supplied by upstream module",
      sourceURL   = NA
    ),
    
    expectsInput(
      objectName  = "riparianBufferPolicy",
      objectClass = "data.frame",
      desc        = "Table of jurisdiction buffer width"
    ),
    
    expectsInput(
      objectName  = "Hydrology_streams",
      objectClass = "SpatVector",
      desc        = "Optional upstream hydrology streams (if provided)"
    ),
    
    expectsInput(
      objectName  = "Hydrology_lakes",
      objectClass = "SpatVector",
      desc        = "Optional upstream hydrology lakes (if provided)"
    )
    
  ),
  outputObjects =  bindrows(
    createsOutput(
      objectName  = "jurisdiction",
      objectClass = c("sf", "SpatVector"),
      desc        = "Canadian jurisdictioncial boundaries (ON, QC, NB, NS, PE, NL) cropped to study area"
    ),
    createsOutput(
      objectName  = "Hydrology_streams",
      objectClass = "SpatVector",
      desc        = "Raw stream network from HydroRIVERS"
    ),
    
    createsOutput(
      objectName  = "Hydrology_lakes",
      objectClass = "SpatVector",
      desc        = "Raw lake polygons from HydroLAKES"
    ),
    createsOutput(
      objectName  = "Riparian",
      objectClass = "list",
      desc        = "Riparian outputs (fraction raster + metadata)"
    )
  )
  
  
))

## Main event for RiparianBuffers.
## Translates jurisdiction-specific riparian policy
## into a spatially explicit buffer raster, then
## computes proportional riparian influence

doEvent.RiparianBuffers <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- RiparianInit(sim)
    },
    warning(
      paste(
        "Undefined event type:",
        eventType,
        "in RiparianBuffers"
      )
    )
  )
  invisible(sim)
}


## Spatial dependencies are expected to be supplied upstream
## minimal defaults are created to allow standalone execution.
.inputObjects <- function(sim) {
  
  dPath <- getOption("reproducible.destinationPath")
  if (is.null(dPath)) {
    dPath <- file.path(tempdir(), "inputs")
  }
  
  dir.create(dPath, recursive = TRUE, showWarnings = FALSE)
  ## ---- Ensure studyArea exists ----
  SpaDES.core::checkObject(sim, "studyArea")
  #SpaDES.core::checkObject(sim, "PlanningGrid_250m", "SpatRaster")
  ## ---------------------------------------------------------
  ## PlanningGrid fallback
  ## ---------------------------------------------------------
  if (!SpaDES.core::suppliedElsewhere("PlanningGrid_250m", sim)) {
    
    message("▶ Creating default PlanningGrid_250m from studyArea...")
    
    sim$PlanningGrid_250m <- terra::rast(
      sim$studyArea,
      resolution = 250,
      crs = terra::crs(sim$studyArea)
    )
    
    terra::values(sim$PlanningGrid_250m) <- 1
  }
  ## -------------------------
  ## riparianBufferPolicy
  ## -------------------------
  if (!SpaDES.core::suppliedElsewhere("riparianBufferPolicy", sim)) {    
    policyFile <- file.path(
      modulePath(sim),
      currentModule(sim),
      "data",
      "riparianBufferPolicy.csv"
    )
    
    stopifnot(file.exists(policyFile))
    
    sim$riparianBufferPolicy <- read.csv(
      policyFile,
      stringsAsFactors = FALSE
    )
  }
  
  ## -------------------------
  ##  (jurisdiction)
  ## -------------------------
  if (!SpaDES.core::suppliedElsewhere("jurisdiction", sim)) {
    sim <- buildjurisdiction(sim)
  }
  
  ## -------------------------
  ## Hydrology
  ## -------------------------
  if (!SpaDES.core::suppliedElsewhere("Hydrology_streams", sim)) {    
    message("▶ Downloading HydroRIVERS...")
    
    streams <- Cache(
      prepInputs,
      url = "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip",
      destinationPath = file.path(dPath, "Hydrology"),
      archive = "HydroRIVERS_v10_na_shp.zip",
      targetFile = "HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na.shp",
      fun = terra::vect,
      cropTo = sim$studyArea,
      projectTo = sim$PlanningGrid_250m
    )
    
    sim$Hydrology_streams <- streams
  }
  
  if (!SpaDES.core::suppliedElsewhere("Hydrology_lakes", sim)) {    
    message("▶ Downloading HydroLAKES...")
    
    lakes <- Cache(
      prepInputs,
      url = "https://data.hydrosheds.org/file/hydrolakes/HydroLAKES_polys_v10_shp.zip",
      destinationPath = file.path(dPath, "Hydrology"),
      archive = "HydroLAKES_polys_v10_shp.zip",
      targetFile = "HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp",
      fun = terra::vect,
      cropTo = sim$studyArea,
      projectTo = sim$PlanningGrid_250m
    )
    
    sim$Hydrology_lakes <- lakes
  }
  
  message(
    "✔ Hydrology ready: ",
    nrow(sim$Hydrology_streams), " streams, ",
    nrow(sim$Hydrology_lakes), " lakes."
  )
  
  invisible(sim)
}
