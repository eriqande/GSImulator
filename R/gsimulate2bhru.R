


#' convert a BaseFile and MixFile output to long(-ish) format usable by BHRU
#'
#' More later
#' @param Num the number of the data set to read in.  Assumes that things are
#' named like BaseFile_X.txt and MixFile_X.txt.  Num should be what you want
#' passed in for X there...
#' @param coll2RU a data frame with columns "repunit" and "collection".
#' Each collection is included once, and the "repunit" of the same row is the
#' reporting unit to which it belongs
#' @export
gsim2bhru <- function(Num, coll2RU = NULL) {
  if(length(Num) != 1) stop("sorry, Num has to be of length 1")

  # get the file names
  base <- paste("BaseFile_", Num, ".txt", sep = "")
  if(!file.exists(base)) stop("Can't find the file ", base)

  mix <- paste("MixFile_", Num, ".txt", sep = "")
  if(!file.exists(mix)) stop("Can't find the file ", mix)

  df <- list(reference = ms2geno2bhru(base),
       mixture =  ms2geno2bhru(mix, isMixture = TRUE)) %>%
    dplyr::bind_rows(.id = "sample_type")
  if(!is.null(coll2RU)) {
    repunit <- coll2RU$repunit[match(df$collection, coll2RU$collection)]
    df <- cbind(repunit, df) %>%
      dplyr::select(sample_type, repunit, everything())
  }
  dplyr::tbl_df(df)
}


#' Compute pairwise Fst from a BaseFile using gsi_sim and return as a data frame
#'
#' More later
#' @param Num the number of the data set to read in.  Assumes that things are
#' named like BaseFile_X.txt.  Num should be what you want
#' passed in for X there...
#' @param coll2RU a data frame with columns "repunit" and "collection".
#' Each collection is included once, and the "repunit" of the same row is the
#' reporting unit to which it belongs
#' @export
basefile2fst <- function(Num) {
  if(length(Num) != 1) stop("sorry, Num has to be of length 1")

  # get the file names
  base <- paste("BaseFile_", Num, ".txt", sep = "")
  if(!file.exists(base)) stop("Can't find the file ", base)

  system(paste(gsi_sim_binary(), "-b", base, "> gsi_sim_dumpola.txt"))
  x <- readr::read_lines("gsi_sim_dumpola.txt")
  x2 <- x[stringr::str_detect(x, "^PAIRWISE")]
  ret <- as.data.frame(stringr::str_split_fixed(x2, "  *", 7)[, c(3,5,7)], stringsAsFactors = FALSE)
  names(ret) <- c("pop1", "pop2", "fst")
  ret$fst <- as.numeric(ret$fst)
  dplyr::tbl_df(ret)
}




# this is an unexported function for converting a gsi_sim like file to bhru type format
ms2geno2bhru <- function(x, ploidy = 2, isMixture = FALSE) {
  d <- readr::read_lines(x)
  locs <- d[stringr::str_detect(d, "^Locus_")]
  locheader <- paste(rep(locs, each = ploidy), c("", paste(".", 1:(ploidy - 1), sep = "")), sep = "")

  # now get just the indivs
  inds <- d[stringr::str_detect(d, "Pop_")]

  # now get these into a data frame
  tmp <- inds %>%
    stringr::str_split_fixed(., "  *", length(locheader) + 1) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    setNames(c("indiv", locheader))

  tmp[, -1] <- lapply(tmp[, -1], function(x) as.integer(x)) %>%
    as.data.frame()

  # and finally, get the collection and attach the reporting units
  ret <- tmp %>%
    dplyr::mutate(collection = stringr::str_extract(tmp$indiv, "Pop_[0-9][0-9]*")) %>%
    dplyr::select(collection, everything())

  if(isMixture) {
    ret$collection = "mixture"
  }
  ret
}
