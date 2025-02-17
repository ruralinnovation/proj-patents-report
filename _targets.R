
library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("tibble", "data.table", "dplyr", "cori.db", "DBI", "tidyr")
)

tar_source()

list(
  tar_target(
             patent_file,
             "data/data_raw/g_patent.tsv",
             format = "file"),
  tar_target(
             assignee_file,
             "data/data_raw/g_assignee_disambiguated.tsv",
             format = "file"),
  tar_target(
             inventor_file,
             "data/data_raw/g_inventor_disambiguated.tsv",
             format = "file"),
  tar_target(
             location_file,
             "data/data_raw/g_location_disambiguated.tsv",
             format = "file"),
  tar_target(g_cpc_raw,
             "data/data_raw/g_cpc_current.tsv",
             format = "file"),
  tar_target(cpc_codes,
             "data/CPC_patent_codes.csv",
             format = "file"),
  tar_target(patent_raw, fread_tsv(patent_file)),
  tar_target(assignee_raw, fread_tsv(assignee_file)),
  tar_target(inventor_raw, fread_tsv(inventor_file)),
  tar_target(location_raw, fread_tsv(location_file)),
  tar_target(cpc_raw, fread_tsv(g_cpc_raw)),
  tar_target(cpc, slim_cpc(cpc_raw)),
  tar_target(cpc_codes_mem, read.csv(cpc_codes)),
  tar_target(location_us, get_me_us_location(location_raw)),
  tar_target(patent_counts_wide, get_patent_counts_wide(
    patent_raw, cpc, assignee_raw, location_us, cpc_codes_mem
  )),
  # tar_target(patent_db, write_to_DB("patent_counts_wide",
  #                                   patent_counts_wide, "staging")),
  tar_target(patent_inventor, get_me_patent_inventor(
    patent_raw,
    inventor_raw,
    location_us
  )),
  # tar_target(patent_inventor_db, write_to_DB("patent_inventor",
  #                                            patent_inventor, "staging")),
  tar_target(patent_assignee, get_patent_assignee(
    patent_raw,
    assignee_raw,
    location_us
)),
# tar_target(patent_assignee_db, write_to_DB("patent_assignee",
#                                            patent_assignee, "staging"))
  tar_target(patent_assignee_location,
             get_me_patent_assignee_loc(patent_raw,
                                        assignee_raw,
                                        location_us,
                                        inventor_raw)),
  tar_target(tidy_base, get_rel_table_co_year(patent_assignee_location,
                                                   geoid_co_2010)),
  tar_target(geoid_co_2010, get_me_us_counties2010()),
  tar_target(cnty_patent, get_me_county_year_patent(patent_assignee_location, tidy_base)),
  tar_target(cnty_patent_db, write_to_DB("cnty_patents", cnty_patent, "staging"))
  # tar_quarto(website, quiet = FALSE),
  # patent_raw, cpc, assignee, location, cpc_codes
  # tar_target(write_county, write.csv(cnty_patent,
  #                                    "data/county_patent.csv",
  #                                    row.names = FALSE)),
  # tar_target(write_inv, write.csv(cnty_inv,
  #                                 "data/county_inv.csv",
  #                                 row.names = FALSE))
)