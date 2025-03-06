###################################################################
# Maak data.frame van top/bot van filters o.b.v. GMW_PPO - part of xml file
# i: index of filter (-)
# doc: xml GMW_PPO - part
xml_extract_screen_info <- function(i, doc) {
  x <- doc[[i]]
  s <- as.list(x[["tubeNumber"]]$children$text) # tubeNumber
  tubeNumber <-  unlist(s)[["value"]] |> as.numeric()
  s <- as.list(x[["screen"]])
  screenTopPosition <- unlist(s[["screenTopPosition"]]$children$text)[["value"]] |> as.numeric()
  screenBottomPosition <- unlist(s[["screenBottomPosition"]]$children$text)[["value"]] |> as.numeric()
  data.frame(FILTER = tubeNumber,
             TOP = screenTopPosition,
             BOT = screenBottomPosition)
}

# Lees NAAM o.b.v. GMW*.xml file.
#
# fname: naam van "GMW*.xml" file.
# id: code om te gebruiken als NAME in het data.frame.
# opties:   "broId", "nitgCode", "wellCode", "brold"
xml_extract_NAME <- function(fname, id = "nitgCode") {
  doc <- XML::xmlParse(fname)
  root_node <- XML::xmlRoot(doc)
  s <- paste0("root_node[[\"dispatchDocument\"]][[\"GMW_PPO\"]][[\"",
              id,
              "\"]]")
  node <- eval(parse(file = "", text = s))
  if (is.null(node)) {
    xml_extract_NAME(fname, id = "broId")
  } else {
    node |> XML::getChildrenStrings(addNames = FALSE)
  }
}

# Maak data.frame van xm-part of hydromonitor object o.b.v. GMW*.xml file.
#
# Return: data.frame met velden (NAME, FILTER, X, Y, TOP, BOT, MV)
# fname: naam van "GMW*.xml" file.
# id: code om te gebruiken als NAME in het data.frame.
# opties:   "broId", "nitgCode", "wellCode", "brold"
read_BRO_xm <- function(fname, id = "nitgCode") {
  NAME <- xml_extract_NAME(fname, id)

  doc <- XML::xmlTreeParse(fname)
  root_node <- XML::xmlRoot(doc)
  doc <- root_node[["dispatchDocument"]][["GMW_PPO"]]

  res <- doc[["deliveredLocation"]][["location"]][["pos"]] |> XML::xmlValue()
  if (!is.na(res)) {
    res <- res |> strsplit(" ") |> unlist() |> as.numeric()
    X <- res[1]
    Y <- res[2]
  } else {
    X <- NA
    Y <- NA
  }

  res <- doc[["deliveredVerticalPosition"]][["groundLevelPosition"]] |> XML::xmlValue()
  if (!is.na(res)) {
    MV <- res |> as.numeric()
  } else {
    MV <- NA
  }

  df1 <- data.frame(NAME, X, Y, MV)

  i <- which(names(doc) == "monitoringTube")
  df2 <- apply(as.array(i),
               FUN = xml_extract_screen_info,
               MARGIN = 1,
               doc = doc) |> dplyr::bind_rows()
  n <- nrow(df2)

  if (n > 1) {
    df1 %<>% rbind(df1[rep(1, n - 1), ])
  }

  df <- dplyr::bind_cols(df1, df2)
  df %<>% dplyr::select(NAME, FILTER, X, Y, TOP, BOT, MV)
  rownames(df) <- NULL
  return(df)
}

# Maak data.frame van xd-part of hydromonitor object o.b.v. GLD*.xml file.
#
# fname: naam van GLD*.xml file.
# NAME: Peilbuis naam.
#
# Return data.frame met velden NAME, FILTER, DATE, HEAD.
xml_extract_observation_info <- function(fname, NAME) {
  cat("Reading", fname, "\n")
  xml_file <- XML::xmlParse(fname)
  x <- XML::getNodeSet(xml_file, "//gldcommon:GroundwaterMonitoringTube")
  FILTER <- as.numeric(XML::xmlValue(x[[1]][["tubeNumber"]]))

  times <- XML::xpathSApply(xml_file, "//waterml:time", XML::xmlValue)
  DATE <- as.Date(substr(times, 1, 10))
  if (length(DATE)==0) {
    return(NULL)
  }
  HEAD <- XML::xpathSApply(xml_file, "//waterml:value", XML::xmlValue) |> as.numeric()
  df <- data.frame(DATE, HEAD)
  df %<>% dplyr::group_by(DATE) |>
    dplyr::summarise(HEAD = mean(HEAD, na.rm = TRUE)) |>
    dplyr::ungroup() |> dplyr::arrange_at("DATE")
  df <- data.frame(NAME, FILTER, DATE=df$DATE, HEAD=df$HEAD)
  return(df)
}

# Maak data.frame van xd-part of hydromonitor object.
#
# Return: data.frame met velden (NAME, FILTER, DATE, HEAD)
# fname: naam van "GMW*.xml" file.
# id: code om te gebruiken als NAME in het data.frame.
# opties voor id:   "broId", "nitgCode", "wellCode", "brold"
read_BRO_xd <- function(fname, id = "nitgCode") {
  NAME <- xml_extract_NAME(fname, id)
  fls <- fname |> dirname() |> list.files(glob2rx("GLD*.xml"),
                                          recursive = TRUE,
                                          full.names = TRUE)
  if (length(fls) == 0)
    (return(NULL))
  df <- Map(xml_extract_observation_info, fls, NAME = NAME) |> dplyr::bind_rows()
}
###################################################################

#' Read all BROloket export files with measured heads from zip file.
#'
#' @inherit read_export_csv
#' @param id Code to use in the column 'NAME' of the resulting data.frame (character).
#' @details id is one of "broId", "nitgCode", "wellCode", "brold".
#' @examples
#' hm1 <- system.file("extdata","bro_export.zip",package="hydromonitor") |> read_bro_zip( )
#' @export
read_bro_zip <-  function(fname, id = "nitgCode") {
  if (!file.exists(fname)) {
    stop('Specified path does not exist.')
  }
  extdir <- paste0(path.expand("~"), "/tmp_read_bro_zip") # Temporary folder

  cat("Unzip file", fname, "to folder", extdir, "\n")
  if (file.exists(extdir)) {
    unlink(extdir,
           recursive = TRUE,
           force = TRUE,
           expand = FALSE)
  }
  utils::unzip(fname,
             overwrite = TRUE,
             junkpaths = FALSE,
             exdir = extdir)


  GMW_fls <- extdir |> list.files(glob2rx("GMW*.xml"),
                                  recursive = TRUE,
                                  full.names = TRUE)
  if (length(GMW_fls) == 0) {
    cat("No HydroMonitor ObservationWell Data found in folder [",
        extdir,
        "].\n")
    return(NULL)
  }

  print("Create xm-part of HydroMonitor ObservationWell Data.")
  xm <- Map(read_BRO_xm, GMW_fls, id = id) |> dplyr::bind_rows()

  print("Create xd-part of HydroMonitor ObservationWell Data.")
  xd <- Map(read_BRO_xd, GMW_fls, id = id) |> dplyr::bind_rows()
  res <- list(xm, xd)
  names(res) <- c("xm", "xd")
  return(res)
}
