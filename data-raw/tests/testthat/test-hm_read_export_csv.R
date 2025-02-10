test_that("Reading csv-file with hm_read_export_csv() results previously created object.", {
  fname <- system.file("extdata","export_data_hydromonitor.csv",package="hydromonitor")
  expect_equal(read_export_csv( fname ), hm1)
})

test_that("Reading csv-file with hm_read_export_csv2() results previously created object.", {
  fname <- fname <- system.file("extdata","Topsoil1.csv",package="hydromonitor")
  expect_equal(read_export_csv2( fname ), hm2)
})

test_that("Reading csv-file with hm_read_export_csv() where no filtersnumbers are specifieds results in an error.", {
  fname <- fname <- system.file("extdata","export_data_hydromonitor(2).csv",package="hydromonitor")
  expect_error(read_export_csv( fname ), "No filter numbers specified in csv-file.")
})
