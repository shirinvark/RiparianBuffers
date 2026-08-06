## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "RiparianBuffers",
  description = "Coarse-resolution PlanningGrid supplied upstream (created internally if missing)",
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
      objectName  = "PlanningGrid",
      objectClass = "SpatRaster",
      desc        = "Coarse-resolution PlanningGrid supplied by upstream module",
      sourceURL   = NA
    ),
    expectsInput(
      objectName  = "Jurisdiction",
      objectClass = c("SpatVector", "sf"),
      desc        = paste(
        "Jurisdiction boundaries supplied by the upstream data-preparation module;",
        "downloaded internally from the default source if not supplied."
      ),
      sourceURL   = NA
    ),
    expectsInput(
      objectName  = "jurisdictionMap",
      objectClass = "SpatRaster",
      desc        = "Jurisdiction raster supplied by the upstream data-preparation module."
    ),
    expectsInput(
      objectName  = "jurisdictionLookup",
      objectClass = "data.frame",
      desc        = "Lookup table linking jurisdiction raster IDs to jurisdiction names."
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
  outputObjects = bindrows(
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
  #SpaDES.core::checkObject(sim, "PlanningGrid", "SpatRaster")
 
  # =========================================================
  ## ---------------------------------------------------------
  ## 1) PlanningGrid
  ##
  ## Expected from the upstream data-preparation module.
  ## If not supplied, create a default 240 m PlanningGrid.
  ## ---------------------------------------------------------
  
  if (!SpaDES.core::suppliedElsewhere("PlanningGrid", sim)) {
    
    message("▶ PlanningGrid not supplied upstream; building locally...")
    
    study_v <- if (inherits(sim$studyArea, "SpatVector")) {
      sim$studyArea
    } else {
      terra::vect(sim$studyArea)
    }
    
    sim$PlanningGrid <- terra::rast(
      ext = terra::ext(study_v),
      resolution = 240,
      crs = terra::crs(study_v)
    )
    
    terra::values(sim$PlanningGrid) <- 1
  }
  
  message("✔ PlanningGrid ready.")
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
  ## ---------------------------------------------------------
  ## Jurisdiction
  ##
  ## Expected from the upstream data-preparation module.
  ## If not supplied, download the default jurisdiction layer.
  ## ---------------------------------------------------------
  
  if (!SpaDES.core::suppliedElsewhere("Jurisdiction", sim)) {
    
    message("▶ Jurisdiction not supplied upstream; downloading default layer...")
    
    studyArea_sf <- if (inherits(sim$studyArea, "sf")) {
      sim$studyArea
    } else {
      sf::st_as_sf(sim$studyArea)
    }
    
    sim$Jurisdiction <- Cache(
      prepInputs,
      url = "https://drive.google.com/uc?export=download&id=1rJQCUJXN3m0pGBGo-bmf4qDfiZbCAg1p",
      destinationPath = file.path(dPath, "Jurisdiction"),
      targetFile = file.path(
        "politicalboundaries_shapefile",
        "NA_PoliticalDivisions",
        "data",
        "boundaries_p_2021_v3.shp"
      ),
      fun = terra::vect,
      cropTo = studyArea_sf,
      projectTo = studyArea_sf
    )
  }
  
  message(
    "✔ Jurisdiction ready. Features: ",
    nrow(sim$Jurisdiction)
  )
  ## ---------------------------------------------------------
  ## Jurisdiction raster + lookup
  ##
  ## Expected from the upstream data-preparation module.
  ## If not supplied, build locally from Jurisdiction.
  ## ---------------------------------------------------------
  
  if (
    !SpaDES.core::suppliedElsewhere("jurisdictionMap", sim) ||
    !SpaDES.core::suppliedElsewhere("jurisdictionLookup", sim)
  ) {
    
    message("▶ Jurisdiction products not supplied upstream; building locally...")
    
    if (!terra::same.crs(sim$Jurisdiction, sim$PlanningGrid)) {
      sim$Jurisdiction <- terra::project(
        sim$Jurisdiction,
        terra::crs(sim$PlanningGrid)
      )
    }
    
    ## Add integer ID
    sim$Jurisdiction$ID <- seq_len(nrow(sim$Jurisdiction))
    
    ## Build lookup table
    sim$jurisdictionLookup <- data.frame(
      ID = sim$Jurisdiction$ID,
      jurisdiction = sim$Jurisdiction$NAME_En,
      nation = sim$Jurisdiction$COUNTRY,
      stringsAsFactors = FALSE
    )
    
    ## Rasterize using integer ID
    sim$jurisdictionMap <- terra::rasterize(
      sim$Jurisdiction,
      sim$PlanningGrid,
      field = "ID"
    )
    
    names(sim$jurisdictionMap) <- "jurisdiction"
    
    ## Attach jurisdiction names as raster levels
    levels(sim$jurisdictionMap) <- sim$jurisdictionLookup[
      ,
      c("ID", "jurisdiction")
    ]
  }
  
  message("✔ jurisdictionMap and jurisdictionLookup ready.")
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
      projectTo = sim$PlanningGrid
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
      projectTo = sim$PlanningGrid
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
