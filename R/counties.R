#' wrangle location for us
#'
#' Location was not in the US and even in tehb US we have some NA location
#'
#' @param string take a file path
#'
#' @return a data frame

get_me_us_location <- function(dat) {
  slim_location <- dat[dat$disambig_country == "US",
                       c("location_id", "state_fips", "county_fips")]
  slim_location <- slim_location[!is.na(slim_location$county_fips), ]
  slim_location$geoid_co <- paste0(sprintf("%02d", slim_location$state_fips),
                                   sprintf("%03d", slim_location$county_fips))
  return(slim_location)
}

#' Make a big data frame before the need to summarize it
#'
#' Location was not in the US and even in the US we have some NA locations
#'
#' @param data.frame patent data
#' @param data.frame assignee data
#' @param data.frame location data
#'
#' @return a data frame


get_me_patent_assignee_location <- function(patent, assignee, location) {

  patent$year <- format(as.Date(patent$patent_date, format = "%Y/%m/%d"), "%Y")

  slim_assignee <- assignee[, c("patent_id", "assignee_id",
                                "location_id",
                                "disambig_assignee_organization")]

  patent_w_assignee <- merge(patent, slim_assignee,
                             by.x = "patent_id", by.y = "patent_id",
                             all.x  = TRUE, all.y = TRUE)
  # a patent can have multiple assignees
  # in that case I will count them more than once in location

  patent_w_assignee_location <- merge(patent_w_assignee, location,
                                        by.x = "location_id", by.y = "location_id",
                                        all.x  = TRUE, all.y = TRUE)

  # obv you have assignee with multiple location
  return(patent_w_assignee_location)
}

dat <-  tar_read(patent_assignee_location)


#' Summarized by county and year number of patents
#'
#' kept when year is NA for overal summary
#'
#' @param dat big data with patent / assignee and location
#' 
#' @return a data frame
get_me_county_year_patent <- function(dat) {
  # I kept NA in year but removed them from geoid_Co
  dat <- dat[!is.na(dat$geoid_co), ]

  summarized <- dplyr::summarize(dat,
                                 cnt_patent = n(),
                                 .by = c(geoid_co, year))
  return(summarized)
}
