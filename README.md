# proj-patents-report

The repo include files to generate a quarto website and a target pipeline. 

Right now the targets pipeline need to be run locally.

You need to clone that repository. 

To run it you need to have all the dependencies: 

- R with:
    - targets
    - tibble, 
    - data.table, 
    - dplyr,
    - cori.db,
    - DBI, 
    - tidyr
    - purrr

Then you will need to run:

```r
source("R/utils.R")
system("mkdir -p data/data_raw/zipped/")

tsv <- c("g_assignee_disambiguated.tsv",
            "g_inventor_disambiguated.tsv", 
            "g_location_disambiguated.tsv", 
            "g_patent.tsv",
            "g_cpc_current.tsv")

purrr::walk(tsv, dl_me_raw_stuff)
```

TODO: include this part in _targets.R

After that you need to run the target pipeline, it wis better to run that process in a background as it takes a bit of time. 

```bash
Rscript -e "targets::tar_make()"
```

The name of the table and schema to write the result is specified in `_targets.R` (it will overwrite it). 

