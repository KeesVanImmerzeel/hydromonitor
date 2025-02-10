#' Remove trailing letter of a string.
#'
#' @param x  A character vector where matches are sought, or an object which can be coerced
#'          by as.character to a character vector. Long vectors are supported.
#' @return A character vector of the same length and with the same attributes as x.
#' @examples
#' \dontrun{remove_trailing_letter("13250900B002A")}
remove_trailing_letter <- function(x) {
  gsub("([A-Z]$)|([a-z]$)","",x)
}

#' Read export HydroMonitor file with ObservationWell data.
#'
#' Trailing letters of observation well are removed.
#'
#' @param fname Filename (csv file, character)
#' @return List of 2:
#'
#' * xm Characteristics of the monitoring well (meta data).
#' * xd Measured heads (data.frame).
#'
#' Variables in data.frame xm:
#' * NAME Name of observationwell.  (character)
#' * FILTER Filter number (integer)
#' * X x-coordinate of observationwell (numeric)
#' * Y y-coordinate of observationwell (numeric)
#' * TOP Level of the top of filter (m+REF, numeric)
#' * BOT Level of the bottom of filter (m+REF,numeric)
#' * MV Surface level (m+REF,numeric)
#'
#' Variables in data.frame xd:
#' * NAME Name of observationwell (character)
#' * FILTER Filter number (integer)
#' * DATE Date of observation (POSIXct)
#' * HEAD Observed head (numeric)
#' @examples
#' hm1 <- system.file("extdata","export_hm.csv",package="hydromonitor") |> read_export_csv( )
#' @export
read_export_csv <- function(fname) {
  # Read Header
  # Check if this is not a export HydroMonitor - open data exchange file
  con <- file(fname, "r")
  x <- readLines(con, 1)
  if (!(grepl("HydroMonitor - open data exchange file", x, fixed = TRUE))) {
    close(con)
    stop("This is not a export HydroMonitor - open data exchange file.")
  }
  # Check if this is not a export HydroMonitor - open data exchange file
  i <- 1

  is_obs_well_file <- FALSE
  while (TRUE) {
    x = readLines(con, 1)
    found <- grepl("ObservationWell", x, fixed = TRUE)
    if ((i == 100) || (found)) {
      break
    }
    i <- i + 1
  }
  if (!found) {
    stop("This is not a export HydroMonitor file with ObservationWell data.")
  }
  close(con)

  # Read all lines in the file to determine
  con <- file(fname, "r")
  x <- readLines(con, warn = FALSE)
  close(con)

  # Determine start end end line number of metadata and data.
  i <- which(grepl("^;;", x))

  # Determine column names of metadata and data
  names_xm <- x[i[1] + 1] %>% strsplit(";") %>% unlist()
  names_xd <- x[i[2] + 1] %>% strsplit(";") %>% unlist()

  # Read Metadata
  xm <- read.csv2(
    fname,
    header = FALSE,
    sep = ";",
    quote = "\"",
    fill = TRUE,
    skip = i[1] + 2,
    row.names = NULL
  )
  xm$V19 <- NULL
  names(xm) <- names_xm
  xm$FilterNo <- suppressWarnings(as.integer(xm$FilterNo))
  if (all(is.na(xm$FilterNo)==TRUE)) {
    stop("No filter numbers specified in csv-file.")
  }
  xm <- xm[!is.na(xm$FilterNo),]
  xm$StartDateTime <- lubridate::dmy_hm(xm$StartDateTime)

  # Filter meta data on essential information
  xm <-
    data.frame(
      NAME = xm$Name,
      FILTER = xm$FilterNo,
      X = xm$XCoordinate,
      Y = xm$YCoordinate,
      TOP = xm$FilterTopLevel,
      BOT = xm$FilterBottomLevel,
      MV = xm$SurfaceLevel
    )
  xm$NAME %<>% remove_trailing_letter(.)
  xm %<>% dplyr::arrange(NAME, FILTER)

  # Read Data
  xd <- read.csv2(
    fname,
    header = FALSE,
    sep = ";",
    quote = "\"",
    fill = TRUE,
    skip = i[2] + 2,
    row.names = NULL
  )
  xd <- xd[, c(1, 2, 3, 4)]
  names(xd) <- c("NAME", "FILTER", "DATE", "HEAD")
  xd <- xd[!is.na(xd$FILTER),]
  xd$DATE <- lubridate::dmy_hms(xd$DATE)
  xd$NAME %<>% remove_trailing_letter(.)
  xd <- xd[!is.na(xd$HEAD),] # Remove NA values

  hm <- list()
  hm$xm <- xm
  hm$xd <- xd
  # Remove double filters and observations
  hm %<>% rm_dble_fltrs()
  hm %<>% rm_dble_obs()
  return(hm)
}

#' Read export HydroMonitor file with ObservationWell data with missing header.
#'
#' Trailing letters of observation well are removed.
#'
#' @inherit read_export_csv
#' @examples
#' hm2 <- system.file("extdata","Topsoil1.csv",package="hydromonitor") |> read_export_csv2( )
#' @export
read_export_csv2 <- function(fname) {
  # Lees gegevens van peilbuizen
  xm <-
    read.csv(
      fname,
      header = TRUE,
      skip = 1,
      dec = ",",
      sep = ";",
      stringsAsFactors = FALSE
    )
  colnames(xm) <-
    c("NAME",
      "FILTER",
      "X",
      "Y",
      "MV",
      "TOP",
      "BOT",
      "MEASLEV",
      "SUMPlEN",
      "STARTDATE")
  suppressWarnings( xm$FILTER %<>% as.integer(.) )
  xm <- xm[!is.na(xm$FILTER),]
  suppressWarnings( xm$X %<>% as.numeric(.) )
  xm <- xm[!is.na(xm$X),]
  suppressWarnings( xm$Y %<>% as.numeric(.) )
  xm <- xm[!is.na(xm$Y),]
  xm$STARTDATE %<>% lubridate::dmy_hm(.)

  # Filter meta data on essential information
  xm <- data.frame(NAME=xm$NAME, FILTER=xm$FILTER, X=xm$X, Y=xm$Y, TOP=xm$TOP, BOT=xm$BOT, MV=xm$MV )
  xm$NAME %<>% remove_trailing_letter(.)
  xm %<>% dplyr::arrange(NAME, FILTER)

  # Lees stijghoogte gegevens
  # Bepaal regelnummer van eerste blanco regel
  s <- readLines(fname)
  skip <- which(s == "")[1]
  xd <-
    read.csv(
      fname,
      header = TRUE,
      skip = skip,
      dec = ",",
      sep = ";",
      stringsAsFactors = FALSE
    )
  colnames(xd) <- c("NAME", "FILTER", "DATE", "HEAD")
  xd <- xd[!is.na(xd$FILTER),]
  xd <- xd[!is.na(xd$HEAD),] # Remove NA values
  xd$DATE %<>% lubridate::dmy_hm(.)
  xd$NAME %<>% remove_trailing_letter(.)

  hm <- list()
  hm$xm <- xm
  hm$xd <- xd

  # Remove double filters and observations
  hm %<>% rm_dble_fltrs()
  hm %<>% rm_dble_obs()
  return(hm)
}


