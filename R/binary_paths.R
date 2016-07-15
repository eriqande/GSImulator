


######### ms2geno funcs  ##########
#' return the path where ms2geno should be in the R system paths
#'
ms2geno_binary_path <- function() {
  bin_name <- paste("ms2geno", Sys.info()["sysname"], sep = "-")
  if(Sys.info()["sysname"] == "Windows") {
    bin_name <- paste(bin_name, ".exe", sep = "")
  }
  file.path(system.file(package = "GSImulator"), "bin", bin_name)
}

#' return TRUE if ms2geno exists where it should be
#'
ms2geno_exists <- function() {
  file.exists(ms2geno_binary_path())
}


#' return TRUE if ms2geno is executable
#'
ms2geno_is_executable <- function() {
  NULL #incomplete
}


#' file path to be used in a call to ms2geno
#'
#' This version checks to make sure it is there and throws an
#' error with a suggestion of how to get it if it is not there.
#' @export
ms2geno_binary <- function() {
  if(!ms2geno_exists()) stop("Can't find the ms2geno executable where it was expected
                             at ", ms2geno_binary_path(), ".")

  # then I should check to make sure it is executable

  # if so, return the path
  ms2geno_binary_path()

}

######### ms funcs  ##########
#' return the path where ms should be in the R system paths
#'
ms_binary_path <- function() {
  bin_name <- paste("ms", Sys.info()["sysname"], sep = "-")
  if(Sys.info()["sysname"] == "Windows") {
    bin_name <- paste(bin_name, ".exe", sep = "")
  }
  file.path(system.file(package = "GSImulator"), "bin", bin_name)
}

#' return TRUE if ms exists where it should be
#'
ms_exists <- function() {
  file.exists(ms_binary_path())
}


#' return TRUE if ms is executable
#'
ms_is_executable <- function() {
  NULL #incomplete
}


#' file path to be used in a call to ms
#'
#' This version checks to make sure it is there and throws an
#' error with a suggestion of how to get it if it is not there.
#' @export
ms_binary <- function() {
  if(!ms_exists()) stop("Can't find the ms executable where it was expected
                        at ", ms_binary_path(), ".")

  # then I should check to make sure it is executable

  # if so, return the path
  ms_binary_path()

}

