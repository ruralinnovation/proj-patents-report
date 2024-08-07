
library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("tibble", "data.table", "dplyr"),
  seed = 42
)

tar_source()

list(
  tar_target(
             patent_file,
             "data/data_raw/unziped/g_patent.tsv",
             format = "file"),
  tar_target(
             assignee_file,
             "data/data_raw/unziped/g_assignee_disambiguated.tsv",
             format = "file"),
  tar_target(
             inventor_file,
             "data/data_raw/unziped/g_inventor_disambiguated.tsv",
             format = "file"),
  tar_target(
             location_file,
             "data/data_raw/unziped/g_location_disambiguated.tsv",
             format = "file"),
  tar_target(patent_raw, fread_tsv(patent_file)),
  tar_target(assignee_raw, fread_tsv(assignee_file)),
  tar_target(inventor_raw, fread_tsv(inventor_file)),
  tar_target(location_raw, fread_tsv(location_file)),
  tar_target(location_us, get_me_us_location(location_raw)),
  tar_target(patent_assignee_location,
             get_me_patent_assignee_loc(patent_raw,
                                        assignee_raw,
                                        location_us,
                                        inventor_raw)),
  tar_target(cnty_patent, get_me_county_year_patent(patent_assignee_location)),
  tar_target(cnty_inv, get_me_inv_cty(inventor_raw, location_us)),
  tar_quarto(website, quiet = FALSE),
  tar_target(write_county, write.csv(cnty_patent,
                                     "data/county_patent.csv",
                                     row.names = FALSE)),
  tar_target(write_inv, write.csv(cnty_inv,
                                  "data/county_inv.csv",
                                  row.names = FALSE))
)