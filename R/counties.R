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


get_me_patent_assignee_loc <- function(patent, assignee, location, inventor) {

  patent$year <- format(as.Date(patent$patent_date, format = "%Y/%m/%d"), "%Y")

  slim_assignee <- assignee[, c("patent_id", "assignee_id",
                                "location_id",
                                "disambig_assignee_organization")]

  patent_w_assignee <- merge(patent, slim_assignee,
                             by.x = "patent_id", by.y = "patent_id",
                             all.x  = TRUE, all.y = TRUE)

  slim_inventor <- inventor[, c("patent_id", "inventor_id")]

  # a patent can have multiple assignees
  # in that case I will count them more than once in location


  patent_w_assignee_location <- merge(patent_w_assignee, location,
                                      by.x = "location_id",
                                      by.y = "location_id",
                                      all.x  = TRUE, all.y = TRUE)

  full_table <- merge(patent_w_assignee_location, slim_inventor,
                      by.x = "patent_id",
                      by.y = "patent_id",
                      all.x  = TRUE, all.y = TRUE)

  # obv you have assignee with multiple location
  return(full_table)
}

#' get inventor by county
#'
#' Location was not in the US and even in the US we have some NA locations
#'
#' @param data.frame inventor data
#' @param data.frame location data
#'
#' @return a data frame

get_me_inv_cty <- function(inventor, location) {

  slim_inventor <- inventor[, c("patent_id", "inventor_id", "location_id")]

  inv_location <- merge(slim_inventor, location,
                        by.x = "location_id",
                        by.y = "location_id",
                        all.x  = TRUE, all.y = TRUE)

  dat <- inv_location[!is.na(inv_location$geoid_co), ]

  summarized <- dplyr::summarize(dat,
                                 cnt_inv = my_unique(patent_id),
                                 .by = geoid_co)
  return(summarized)
}

#' Summarized by county and year number of patents
#'
#' kept when year is NA for overal summary
#'
#' @param dat big data with patent / assignee and location
#'
#' @return a data frame
get_me_county_year_patent <- function(dat) {
  # not happy on hwo to deal with missing year county
  # kind of relying that I have all cty and year
  tidy_base <- expand.grid(unique(na.omit(dat$geoid_co)),
                           unique(na.omit(dat$year)),
                           stringsAsFactors = FALSE)
  names(tidy_base) <- c("geoid_co", "year")

  dat2 <-  dplyr::bind_rows(tidy_base, dat)

  dat2$geoid_st <- substr(dat2$geoid_co, 1, 2)

  summarized_co <- dat2 |>
    dplyr::filter(!is.na(geoid_co)) |>
    dplyr::summarize(
                     cnt_patents = my_unique(patent_id),
                     cnt_patent_inventors = my_unique(inventor_id),
                     cnt_pantent_owners = my_unique(assignee_id),
                     .by = c(geoid_co, year)) |>
    dplyr::rename(geoid = geoid_co) |>
    dplyr::filter(!is.na(year))

  stopifnot(nrow(summarized_co) == 155722)


  summarized_st <- dat2 |>
    dplyr::filter(!is.na(geoid_st)) |>
    dplyr::summarize(
                     cnt_patents = my_unique(patent_id),
                     cnt_patent_inventors = my_unique(inventor_id),
                     cnt_pantent_owners = my_unique(assignee_id),
                     .by = c(geoid_st, year)) |>
    dplyr::rename(geoid = geoid_st) |>
    dplyr::filter(!is.na(year))

  stopifnot(nrow(summarized_st) == 2744)

  summarized_natl <-
    dplyr::summarize(dat2,
                     cnt_patents = my_unique(patent_id),
                     cnt_patent_inventors = my_unique(inventor_id),
                     cnt_pantent_owners = my_unique(assignee_id),
                     .by =  year) |>
    dplyr::filter(!is.na(year))
    
  summarized_natl$geoid <- "00"

  summarized <- dplyr::bind_rows(summarized_co, summarized_st, summarized_natl)

  return(summarized)
}