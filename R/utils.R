#' Your classic read into list function
#'
#' Take a dir path and read all tsv in it and store in name list
#'
#' @param string take a directory path
#'
#' @return a list of data frame
#' @examplesIf interactive()
#' list_samples <- read_sample("data/data_sample/") 
#' list2env(list_samples)

read_sample <- function(path_dir) {
  list_tsv <- paste0(path_dir,
                     list.files(path_dir,
                                pattern = ".tsv"))

  list_samples <- sapply(list_tsv, read.csv, sep = "\t")
  names(list_samples) <- basename(names(list_samples))
  return(list_samples)
}

#' Wrapper around fread for this use case
#'
#' Take a dir path and read all tsv in it and store in name list
#'
#' @param string take a file path
#'
#' @return a data frame
#' 


