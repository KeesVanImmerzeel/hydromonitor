## Code to prepare external datasets (visible to user)

fname <- system.file("extdata","export_data_hydromonitor.csv",package="hydromonitor")
hm1 <- read_export_csv( fname )

fname <- system.file("extdata","Topsoil1.csv",package="hydromonitor")
hm2 <- read_export_csv2( fname )

## Code to prepare external "polygn" object: polygon shape to be used in example of function filter_on_poly.
#fname <- "data-raw/polygn.shp"
#polygn <- terra::vect(fname)

usethis::use_data(hm1, hm2, internal=FALSE, overwrite = TRUE)
