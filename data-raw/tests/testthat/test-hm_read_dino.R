test_that("Reading dino *_1.csv file with read_dino() results in previously created object.", {
  fname <- system.file("extdata","B38E0108001_1.csv",package="hydromonitor")
  expect_equal(read_dino( fname ), hm3)
})

test_that("Reading all export Dino *_1.csv files with measured heads in specified folder with hm_read_dino_path() results in previously created object.", {
  path <- system.file("extdata","Grondwaterstanden_Put",package="hydromonitor")
  expect_equal(read_dino_path( path ), hm4)
})

test_that("Reading measured heads from zip file.with hm_read_dino_zip() results in previously created object.", {
  fname <- system.file("extdata","Dino_export_18032020.zip",package="hydromonitor")
  expect_equal(read_dino_zip( fname ), hm5)
})
