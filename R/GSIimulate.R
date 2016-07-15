


#' Simulate baseline and mixture files in to the current working directory
#'
#' This is a wrapper around ms2geno that should make it much easier to use
#' @param refsizes integer vector of sample sizes for the reference samples from each
#' collection (population)
#' @param mixsizes integer vector of samples sizes of individuals that go into the
#' mixture data set.
#' @param num_loci the number of loci to simulate
#' @param variability_string  the command to pass to ms to influence the number of segregating sites.  Either something
#' like "-s 20" for fixed number of segsites, or "-t 8" to set the value of theta to 8.
#' @param marker_pars a string that gives all the command line arguments needed for either microsatellites or SNPs
#' (but not both!) to be simulated.  See the documentation for ms2geno.
#' @param M_num a number for 4Nem to be passed to ms to make a symmetrical island model.  Gets overridden by
#' M_matrix if M_matrix is present (i.e. non-NULL).
#' @param M_matrix  a full migration matrix if you want to pass one in.  Will override M_num if present.
#' By default it will be NULL.
#' @param repunits optional vector that must be the same length as refsizes and which gives the
#' reporting units to which each collection belongs (NOT IMPLEMENTED)
#' @param num_sets desired number of data sets.
#' @param ploiy the ploidy of the organisms to be simulated.
#' @export
#' @examples
#' # microsatellites
#' GSImulate(
#'    refsizes = c(200, 200, 150, 100),
#'    mixsizes = c(25, 25, 10, 30),
#'    num_loci = 20,
#'    num_sets = 5,
#'    variability_string = "-t 2.4",
#'    marker_pars = "-u .15 3",
#'    M_num = 5)
#'
#' # SNPs ascertained from 8 individuals from each population
#' # and taken if all three genotypes are seen
#' GSImulate(
#'    refsizes = c(200, 200, 150, 100),
#'    mixsizes = c(25, 25, 10, 30),
#'    num_loci = 20,
#'    num_sets = 5,
#'    variability_string = "-t 2.4",
#'    marker_pars = "-s 1 8 -2 -s 2 8 -2 -s 3 8 -2 -s 4 8 -2 --all-pops-geno-asc",
#'    M_num = 5)

GSImulate <- function(refsizes,
                      mixsizes,
                      num_loci,
                      num_sets,
                      variability_string,
                      marker_pars,
                      M_num = NULL,
                      M_matrix = NULL,
                      repunits = character(0),
                      ploidy = 2) {

  if(length(refsizes) != length(mixsizes)) stop("Gotta have the same number of populations in refsizes and mixsizes")

  # make some variables to hold number of gene copies in each population, etc, for ms
  gcr <- refsizes * ploidy
  gcm <- mixsizes * ploidy

  gcboth <- gcr + gcm

  totgc <- sum(gcboth)  # this is the number of tips on the coalescent tree to simulate

  ntrees <- (num_sets + 1) * num_loci - 1  # we leave ourselves almost enough loci for another data set to try to account for invariant loci that get dropped

  # get the first part of the ms system call
  ms_main <- paste(ms_binary(), totgc, ntrees, variability_string)

  # now, deal with the migration rate stuff.
  ms_mig_start <- paste("-I", length(refsizes), paste(gcboth, collapse = " ") )

  # now set the migration rates
  if(all(is.null(M_matrix), is.null(M_num))) stop("One of M_num or M_matrix must be provided");

  if(is.null(M_matrix)) {
    ms_mig_end <- M_num
  } else {
    rc <- dim(M_matrix)
    ms_mig_end <- paste("-ma ", paste(t(M_matrix), collapse = " " ))
  }

  # here is our full ms call
  mscall <- paste(ms_main, ms_mig_start, ms_mig_end)


  # now we need to compile the call to ms2geno
  ms2geno_call <- paste(ms2geno_binary(),
                        "--ploidy", ploidy,
                        "-l", num_loci,
                        "-b", paste(refsizes, collapse = " "),
                        "-m", paste(mixsizes, collapse = " "),
                        marker_pars
                        )
  full_call <- paste(mscall, "|", ms2geno_call)

  message("Giving the command ", full_call)

  system(full_call)
}
