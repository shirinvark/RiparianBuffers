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
      objectName  = "Hydrology_lakes",
      objectClass = "SpatVector",
      desc        = "Hydrological lakes and large water bodies supplied upstream"
    ),
   # expectsInput(
     # objectName  = "Hydrology_basins",
      #objectClass = "SpatVector",
      #desc        = "Hydrological basins supplied upstream"
    #),
    expectsInput(
      objectName  = "Hydrology_streams",
      objectClass = "SpatVector",
      desc = "Hydrological stream network extracted upstream"
    ),
    ## Provinces are supplied by EasternCanadaDataPrep
    ## and are used ONLY to spatially apply province-specific
    ## riparian buffer policies (no landbase decisions here).
    expectsInput(
      objectName  = "Provinces",
      objectClass = "SpatVector",
      desc        = "Provincial boundaries with province_code"
    ),
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


## This module does not create or download inputs
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
  ## NOTE:
  ## This module expects all inputObjects to be supplied upstream.
  ## No defaults are created here by design.
  
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") # uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

 
  if (!SpaDES.core::suppliedElsewhere("Policy")){#RiparianBufferPlolicy
    sim$riparianBufferPolicy <- data.frame(
      province_code = c("BC","AB","SK","MB","ON","QC","NB","NS","NL","PE"),
      buffer_m = rep(30, 10),
      stringsAsFactors = FALSE
  }
  return(invisible(sim))
}
## ------------------------------------------------------------------
## Module philosophy:
## RiparianBuffers translates hydrological geometry
## and jurisdictional policy into a continuous spatial signal,
## without embedding management or landbase assumptions.
## ------------------------------------------------------------------


