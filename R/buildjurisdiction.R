buildjurisdiction <- function(sim) {
  
  message("🔵 Building jurisdiction layer...")
  
  prov <- rnaturalearth::ne_states(
    country = "Canada",
    returnclass = "sf"
  )
  
  prov <- prov[prov$name_en %in% c(
    "Ontario",
    "Quebec",
    "New Brunswick",
    "Nova Scotia",
    "Prince Edward Island",
    "Newfoundland and Labrador"
  ), ]
  
  prov$jurisdiction <- ifelse(prov$name_en == "Ontario", "ON",
                              ifelse(prov$name_en == "Quebec", "QC",
                                     ifelse(prov$name_en == "New Brunswick", "NB",
                                            ifelse(prov$name_en == "Nova Scotia", "NS",
                                                   ifelse(prov$name_en == "Prince Edward Island", "PE",
                                                          ifelse(prov$name_en == "Newfoundland and Labrador", "NL", NA))))))
  
  studyArea_sf <- sf::st_as_sf(sim$studyArea)
  
  prov <- sf::st_transform(prov, sf::st_crs(studyArea_sf))
  prov <- sf::st_intersection(prov, studyArea_sf)
  
  prov <- prov[, "jurisdiction", drop = FALSE]
  
  sim$jurisdiction <- terra::vect(prov)
  
  message(
    "✔ jurisdiction ready: ",
    paste(unique(sim$jurisdiction$jurisdiction), collapse = ", ")
  )
  
  invisible(sim)
}