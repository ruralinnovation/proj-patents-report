
library(targets)

tar_option_set(
  packages = c("tibble", "data.table")
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
  tar_target(location_us, get_me_us_location(location_raw))
)

