
library(targets)

tar_option_set(
  packages = c("tibble", "data.table")
)

tar_source()

list(
  tar_target(
             name = patent_file,
             "data/data_raw/unziped/g_patent.tsv",
             format = "file"),
  tar_target(
             name = assignee_file,
             "data/data_raw/unziped/g_assignee_disambiguated.tsv",
             format = "file"),
  tar_target(
             name = inventor_file,
             "data/data_raw/unziped/g_inventor_disambiguated.tsv",
             format = "file"),
  tar_target(
             name = location_file,
             "data/data_raw/unziped/g_location_disambiguated.tsv",
             format = "file")
)
