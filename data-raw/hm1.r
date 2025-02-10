## Code to prepare external "hm1" example dataset (HydroMonitor Observation Well data).

fname <- system.file("extdata","export_data_hydromonitor.csv",package="hydromonitor")
hm1 <- read_export_csv( fname )

usethis::use_data(hm1, internal=FALSE, overwrite = TRUE)
