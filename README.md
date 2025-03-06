
# Package hydromonitor

<!-- badges: start -->
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/KeesVanImmerzeel/hydromonitor?branch=master&svg=true)](https://ci.appveyor.com/project/KeesVanImmerzeel/hydromonitor)

<!-- badges: end -->

The goal of the package hydromonitor is to read / write / manipulate [HydroMonitor](https://www.kwrwater.nl/tools-producten/hydromonitor/) Observationwell data. 

![Rplot](https://user-images.githubusercontent.com/16401251/90610250-71ca0400-e205-11ea-8d81-149542d35b93.png)

## Installation

You can install the released version of hydromonitor from with:

`install_github("KeesVanImmerzeel/hydromonitor")`

Then load the package with:

`library("hydromonitor")` 

## Functions in this package

- `read_export_csv()`: Read export HydroMonitor file with ObservationWell data.
- `read_export_csv2()`: Read export HydroMonitor file with ObservationWell data with missing header.
- `read_dino()`: Read export Dino *_1.csv file with measured heads.
- `read_dino_path`: Read all export Dino *_1.csv files with measured heads in specified folder.
- `read_dino_zip`: Read all export Dino *_1.csv files with measured heads from zip file.
- `read_bro_zip`: Read all BROloket export files with measured heads from zip file.
- `obs_periods`: Create a data frame with observation period for each filter.
- `filter_on_year()`: Filter HydroMonitor ObservationWell data on year.
- `filter_on_pol()`: Filter HydroMonitor ObservationWell data with polygon shape.
- `calc_gxg()`: Calculate GxG's of HydroMonitor ObservationWell data.
- `hm_plot()`: Plot HydroMonitor ObservationWell data.
- `nr_obs_ratio()`: Ratio's (# observations in filter) / (average # of observations in monitoring well)

## Datasets

- `hm1, hm2: HydroMonitor ObservationWell Data. Can be read with `read_export_csv()`.

## Get help

To get help on the functions in this package type a question mark before the function name, like `?read_export_csv()`



