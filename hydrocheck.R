
file.exists("D:/HydroRIVERS_v10_na_shp (3).zip")

dir.create("D:/HydroRIVERS", showWarnings = FALSE)
unzip(
  zipfile = "D:/HydroRIVERS_v10_na_shp (3).zip",
  exdir   = "D:/HydroRIVERS",
  overwrite = TRUE
)
list.files("D:/HydroRIVERS", recursive = TRUE)
library(sf)

hydro <- st_read(
  "D:/HydroRIVERS/HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na.shp"
)
names(hydro)
