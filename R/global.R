#' @import dplyr
#' @importFrom utils tail
#' @importFrom magrittr %>%
#' @importFrom stats runif
#' @importFrom data.table rbindlist
#' @importFrom rlang caller_env
#' @importFrom sampling inclusionprobabilities

utils::globalVariables(
  c("VMIN", "VMAX", "INDEX","certainty","npop","flags","tanggal",
    "ncertainty","k","cert_now","nsam_tot","sisa","nsam","tmp_strata",
    "alokasi_n","fsqrt","sfsqrt","alok0","alok","alok_p","n_primary","n_secondary","jml","cumsumpi",
    "n_selected","n_deficit","is_secondary_tmp")
)
