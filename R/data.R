#' Indonesian Population (SP2020)
#'
#' Tabulation of Indonesia's population based on the results of the 2020 population census by regency/city and gender
#'
#' @format ## `pop_dt`
#' A data frame with 514 rows and 8 columns:
#' \describe{
#'   \item{idkab}{region id}
#'   \item{kdprov}{province code}
#'   \item{kdkab}{regency/city code}
#'   \item{nmprov}{province name}
#'   \item{nmkab}{regency/city name}
#'   \item{Laki-laki}{Male Population}
#'   \item{Perempuan}{Female Population}
#'   \item{Total}{Total Population}
#'   ...
#' }
#' @source <https://sensus.bps.go.id/main/index/sp2020>
"pop_dt"

#' Example of Allocation Data
#'
#' Example of Allocation Data for Sampling Purposes
#'
#' @format ## `alokasi_dt`
#' A data frame with 34 rows and 3 columns:
#' \describe{
#'   \item{kdprov}{province code}
#'   \item{jml_kabkota}{Population or number of regencies/cities}
#'   \item{n_primary}{Sample Allocation}
#'   ...
#' }
"alokasi_dt"
