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