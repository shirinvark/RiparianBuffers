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
  authors = structure(list(list(given = c("Shirin", "Middle"), family = "Varkouhi", role = c("aut", "cre"), email = "Shirin.varkuhi@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(RiparianBuffers = "0.1.0.9000"),
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
      "hydroRaster_m",
      "numeric",
      30,
      0,
      NA,
      "Resolution (m) used to compute proportional riparian fraction"
    )
  ),
  inputObjects =  bindrows(
    
    expectsInput(
      objectName  = "PlanningRaster",
      objectClass = "SpatRaster",
      desc        = "Coarse-resolution planning raster supplied by upstream module",
      sourceURL  = NA
    ),
    expectsInput(
      objectName  = "riparianBufferPolicy",
      objectClass = "data.frame",
      desc        = "Table of jurisdiction buffer width"
    ),
    expectsInput(
      objectName  = "Hydrology_lakes",
      objectClass = "SpatVector",
      desc        = "Hydrological lakes and large water bodies supplied upstream"
    ),
    expectsInput(
      objectName  = "Hydrology_streams",
      objectClass = "SpatVector",
      desc = "Hydrological stream network extracted upstream"
    ),
    ## Provinces are supplied by EasternCanadaDataPrep
    ## and are used ONLY to spatially apply province-specific
    ## riparian buffer policies (no landbase decisions here).
    expectsInput(
      objectName  = "jurisdiction",
      objectClass = "SpatVector",
      desc        = "jurisdictional boundaries with jurisdiction"
    )
  ),
  outputObjects =  bindrows(
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
## computes proportional riparian influence.

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
  
  if (!SpaDES.core::suppliedElsewhere("riparianBufferPolicy")) {
    
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
  
  invisible(sim)
}



