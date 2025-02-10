## Code to prepare internal datasets (file "R/sysdata.rda")

fname <- system.file("extdata","B38E0108001_1.csv",package="hydromonitor")
hm3 <- read_dino( fname )

path <- system.file("extdata","Grondwaterstanden_Put",package="hydromonitor")
hm4 <- read_dino_path( path )

fname <- system.file("extdata","Dino_export_18032020.zip",package="hydromonitor")
hm5 <- read_dino_zip( fname )

fname <- "data-raw/polygn.shp"
polygn <- terra::vect(fname)
filtered_on_polygon <- filter_on_poly( hm1, polygn )

obs_periods <- obs_periods( hm1 )

## Create file R/sysdata.rda
usethis::use_data(
  hm3,
  hm4,
  hm5,
  filtered_on_polygon,
  obs_periods,
  internal = TRUE,
  overwrite = TRUE
)
