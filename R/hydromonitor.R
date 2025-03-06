#' hydromonitor: Read / write / manipulate \href{https://www.kwrwater.nl/tools-producten/hydromonitor/}{HydroMonitor} Observationwell data.
#'
#' Hydromonitor data is converted to an intern format: HydroMonitor ObservationWell Data (HMOWD).
#'
#' Structure of HMOWD: list of two data.frames:
#'
#' - data.frame `hm` with fields:
#' NAME: chr
#' FILTER: int
#' X     : int
#' Y     : int
#' TOP   : num
#' BOT   : num
#' MV    : num
#'
#' - data.frame `xm`with fields:
#' NAME  : chr
#' FILTER: int
#' DATE  : POSIXct
#' HEAD  : num
#'
#' Functions:
#'
#' * \code{\link{read_export_csv}}
#' * \code{\link{read_export_csv2}}
#' * \code{\link{read_bro_zip}}
#' * \code{\link{read_dino}}
#' * \code{\link{read_dino_path}}
#' * \code{\link{read_dino_zip}}
#' * \code{\link{obs_periods}}
#' * \code{\link{filter_on_year}}
#' * \code{\link{filter_on_extent}}
#' * \code{\link{filter_on_poly}}
#' * \code{\link{rm_fltrs_with_no_obs}}
#' * \code{\link{hm_rbind}}
#' * \code{\link{rm_dble_fltrs}}
#' * \code{\link{rm_dble_obs}}
#' * \code{\link{hm_plot}}
#' * \code{\link{calc_gxg}}
#' * \code{\link{create_shp}}
#' * \code{\link{nr_obs_ratio}}
#'
#' Datasets:
#' - hm1, hm2: HydroMonitor ObservationWell Data. Can be read with \code{\link{read_export_csv}}.
#'
#' @name hydromonitor
#'
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#'
#' @importFrom utils read.csv
#' @importFrom utils read.csv2
#' @importFrom utils glob2rx
#' @importFrom utils unzip
#'
#' @importFrom lubridate dmy_hm
#' @importFrom lubridate dmy_hms
#' @importFrom lubridate year
#' @importFrom lubridate month
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr semi_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom dplyr do
#' @importFrom dplyr n
#' @importFrom dplyr mutate
#' @importFrom dplyr slice
#' @importFrom dplyr full_join
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ggsave
#'
#' @importFrom stats quantile
#'
#' @importFrom terra vect
#' @importFrom terra writeVector
#' @importFrom terra project
#' @importFrom terra extract
#' @importFrom terra ext
#'
#' @importFrom XML xmlRoot
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
#' @importFrom XML xpathSApply
#'
NULL
