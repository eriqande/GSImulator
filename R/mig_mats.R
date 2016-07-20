


#' return the migration matrix for a simple hierarchical island model
#'
#' This provides the migration matrix for one of the simplest possible
#' island models with migration rates within and between groups.
#' @param ru a vector giving the numbers of collections within each
#' group.  For example, if there are 10 collections total and the first
#' two are in one group, the next three are in another and the final five
#' are in a third, then you would want \code{ru = c(2, 3, 5)}.
#' @param Min the M = 4Nem value desired within groups.  This will get
#' scaled by the number of collections within the group (Min / (N - 1))
#' where N is the number of collections in the group.
#' @param Mbt the M = 4Nem value desired between collections that are in
#' different groups. This will not be scaled by the number of collections not in
#' the group.
#' @return Returns that matrix in text format to pass into ms
#' @export
sh_island_mig_mat <- function(ru, Min, Mbt) {
  tots <- sum(ru)

  # start off setting everything to the background "between" rate
  M <- matrix(Mbt, nrow = tots, ncol = tots)

  # get the indices of the groups in a list
  starts <- cumsum(ru) - ru + 1
  ends <- cumsum(ru)

  for(i in 1:length(ru)) {
    idx <- seq(starts[i], ends[i])
    M[idx, idx] <- Min / (ru[i] - 1)
  }
  diag(M) <- NA

  paste(as.character(M), collapse = " ")
}
