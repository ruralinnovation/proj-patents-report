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
#' @param string take a file path
#'
#' @return a data frame

fread_tsv <- function(f_path, ...) {
  data.table::fread(f_path, sep = "\t", ...)
}

#' My opinionated version of DT with all options that I like
#'
#' Take a dir path and read all tsv in it and store in name list
#'
#' @param string a table to render
#'

table_with_options <- function(x) {
  DT::datatable(x,
               rownames = FALSE,
                extensions = "Buttons",
                options = list(
                          dom = "Blfrtip",
                          buttons = list("copy",
                                         "print",
                                         list(extend = "collection",
                                              buttons = c("csv", "excel"),
                                              text = "Download"))))
}

#' Quick and dirty way to download stuff 
#'
#' Put it in an opinionated place (should be data/raw)
#'
#' @param string name of a file
#' @examplesIf interactive()
#' dl_me_raw_stuff("g_location_disambiguated.tsv")

dl_me_raw_stuff <- function(file, path = "data/data_raw/unzipped") {
  url_path <- paste0("https://s3.amazonaws.com/data.patentsview.org/download/",
                     file,
                     ".zip")
  my_dest <- paste0(path, file)
  file <- basename(url_path)
  download.file(url_path,
                destfile = my_dest)
  message(sprintf("%s was downloaded", file))
  unzip(my_dest, exdir = path)
}


#' count unique not NA value of x
#''
#' @param x avector 

my_unique <- function(x) {
  length(unique(na.omit(x)))
}