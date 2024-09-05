# remoinder since I used data.table a lot of function like
# merge are dispatched to it and using teh DT version

# TODO create a function that return just year and patent_id from patent
# replace repeating code with it


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


# Use PG to get counties

get_me_us_counties2010 <- function() {
  con <- cori.db::connect_to_db("sch_census_tiger")
  on.exit(DBI::dbDisconnect(con))
  q <- 'select "GEOID" from sch_census_tiger.source_cb_2019_county'
  DBI::dbGetQuery(con, q)
}

get_me_patent_assignee_loc <- function(patent, assignee, location, inventor) {

  patent$year <- format(as.Date(patent$patent_date, format = "%Y/%m/%d"), "%Y")

  slim_assignee <- assignee[, c("patent_id", "assignee_id",
                                "location_id",
                                "disambig_assignee_organization")]

  patent_w_assignee <- merge(patent, slim_assignee,
                             by.x = "patent_id", by.y = "patent_id",
                             all.x  = TRUE, all.y = FALSE)

  patent_w_assignee_location <- merge(patent_w_assignee, location,
                                      by.x = "location_id",
                                      by.y = "location_id",
                                      all.x  = TRUE, all.y = FALSE)

  slim_inventor <- inventor[, c("patent_id", "inventor_id")]

  # a patent can have multiple assignees
  # in that case I will count them more than once in location

  full_table <- merge(patent_w_assignee_location, slim_inventor,
                      by.x = "patent_id",
                      by.y = "patent_id",
                      all.x  = TRUE, all.y = FALSE)

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

get_me_patent_inventor <- function(patent_raw, inventor, location) {

  patent_raw$year <- format(as.Date(patent_raw$patent_date,
                                    format = "%Y/%m/%d"), "%Y")

  patent_slim <- patent_raw[, c("patent_id", "year")]

  slim_inventor <- inventor[, c("patent_id", "inventor_id", "location_id")]

  inventor_clean <- inventor[!duplicated(slim_inventor),
                             c("patent_id", "inventor_id",
                               "location_id", "gender_code")]
  # you have a gender_code "" that need to be NA
  inventor_clean$gender_code[inventor_clean$gender_code == ""] <- NA_character_

  inventor_impr <- merge(inventor_clean, patent_slim, by = "patent_id",
                         all.x = TRUE)

  # september 2024 you have 1226 duplicated row patent_id / inventor_id / location

  location_slim <- location[, c("location_id", "geoid_co")]

  inventor_loc <- merge(inventor_impr, location_slim,
                        by = "location_id")
  # it looks like some stuff in location us are not related to patent
  # if by.y = TRUE we have close to 600 location w/o inventor

  # we can be unique on patent_id / inventor_id / location_id
  # but not with geoid ie
  # on inventor can move inside a geoid_co
  # I will filter out those cases

  inventor_loc_no_dup <- inventor_loc[
    !duplicated(inventor_loc[, c("patent_id", "inventor_id",
                                 "year", "geoid_co")]),
                                 c("patent_id", "inventor_id",
                                   "year", "geoid_co", "gender_code")]

  stopifnot(sum(is.na(inventor_loc$geoid_co)) == 0)

  return(inventor_loc_no_dup)
}

# TODO new table patent_assignee
# assignee / geoid_co /
# inquiry can an assignee have multiple location ?
# year / county / assignee_id 
# add assignee_type

get_patent_assignee <- function(patent_raw, assignee_raw, location_us) {
  slim_assignee <- assignee_raw[, c("patent_id", "assignee_id",
                                    "assignee_type", "location_id")]

  patent_raw$year <- format(as.Date(patent_raw$patent_date,
                                    format = "%Y/%m/%d"), "%Y")

  slim_patent <- patent_raw[, c("patent_id", "year")]
  # unique on patent it
  # and setdiff(slim_assignee$patent_id, slim_patent$patent_id)

  assignee_year <- merge(slim_assignee, slim_patent, by = "patent_id")

  slim_location <- location_us[, c("location_id", "geoid_co")]

  assignee_us <- merge(assignee_year, slim_location, by = "location_id")
  # filtering out the duplicated on patent_id / location_id 
  # weird cases
   assignee_us_no_dup_co <- assignee_us[
    !duplicated(assignee_us[, c("assignee_id", "patent_id", "geoid_co")]), ]
  # 305 removed if pick location_id and 
  # 308 if used geoid_co I went with geoid_co

  return(assignee_us_no_dup_co[, c("patent_id", "assignee_id",
                                   "assignee_type", "year", "geoid_co")])
}


#' Return a combo of all years and county in the data set
#'
#' should get me a PK on geoid_co / year useful for join
#'
#' @param dat big data with patent / assignee and location
#'
#' @return a data frame
#'
get_rel_table_co_year <- function(dat, geoid_co_2010) {
  tidy_base <- expand.grid(geoid_co_2010$GEOID,
                           unique(na.omit(dat$year)),
                           stringsAsFactors = FALSE)
  names(tidy_base) <- c("geoid_co", "year")
  tidy_base[order(tidy_base$geoid_co, tidy_base$year), ]
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


  dat <- dat[!is.na(dat$geoid_co), ]

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

  summarized_natl2 <- dat2 |>
    dplyr::filter(!is.na(dat2$year)) |>
    dplyr::summarize(
                     cnt_patents = my_unique(patent_id),
                     cnt_patent_inventors = my_unique(inventor_id),
                     cnt_pantent_owners = my_unique(assignee_id),
                     .by =  year)


  summarized_natl$geoid <- "00"

  summarized <- dplyr::bind_rows(summarized_co, summarized_st, summarized_natl)

  return(summarized)
}

slim_cpc <- function(cpc_raw) {
  # not great
  cpc_raw <- as.data.frame(cpc_raw)
  cpc <- cpc_raw[c("patent_id", "cpc_class")]
  cpc$cpc_subsection <- substr(cpc$cpc_class, 1, 2)
  cpc$temp_id <- paste(cpc$patent_id, cpc$cpc_subsection, sep = "-")
  cpc_pk <- cpc[!duplicated(cpc$temp_id),
                c("patent_id", "cpc_subsection")]
  return(cpc_pk)
}


get_patent_counts_wide <- function(patent_raw, cpc, assignee, location, cpc_codes) {
  # good news sis that patent id from cpc is a correct fk
  # no patent_id here that are not in patent_raw
  patent_raw$year <- format(as.Date(patent_raw$patent_date,
                                    format = "%Y/%m/%d"), "%Y")
  patent <- patent_raw[, list(patent_id, year)]
  cpc$patent_id <- as.character(cpc$patent_id)
  patent_cpc <- merge(patent, cpc,
                      by.x = "patent_id", by.y = "patent_id",
                      all.x = TRUE, all.y = TRUE)
  patent_cpc_get2014 <- patent_cpc[patent_cpc$year >= 2014 & patent_cpc$year < 2024,]
  # remove quite a lot, need to keep assignee ID or not anymore a PK
  # for some reason we have case where we have multiple of same assignee per patent
  slim_assignee <- assignee[, list(patent_id, location_id, assignee_id)]
  slim_assigne_no_dup <- slim_assignee[!duplicated(slim_assignee), ]
  slim_location <- location[, list(location_id, geoid_co)]
  assignee_us <- merge(slim_assigne_no_dup, slim_location,
                       by = "location_id")
  tidy_patents <- merge(patent_cpc_get2014, assignee_us, by = "patent_id")
  tidy_cori_patents <- merge(tidy_patents, cpc_codes, by = "cpc_subsection")
  # if not adding assignee id it is not anymore a PK hence we will have dup
  slim_tidy_cori_patents <- tidy_cori_patents[, list(patent_id, year,
                                                     cpc_subsection,
                                                     geoid_co)]

  slim_tidy_cori_patents_oli <- slim_tidy_cori_patents[!duplicated(slim_tidy_cori_patents),]

  patent_counts_wide <- slim_tidy_cori_patents_oli |>
              as.data.frame() |>
              dplyr::mutate(value = 1) |>
              tidyr::pivot_wider(names_from = cpc_subsection,
                                 values_from = value)
}

write_to_proj_erc <- function(table_name, data, schema = "proj_erc") {
  con <- cori.db::connect_to_db(schema)
	message(schema)
	on.exit(DBI::dbDisconnect(con))
  (DBI::dbWriteTable(conn = con,
                     name = table_name,
                     value = data,
                     overwrite = TRUE))
}