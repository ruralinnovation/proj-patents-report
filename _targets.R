
library(targets)

tar_option_set(
  packages = c("tibble")
)

tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(
    name = data,
    command = tibble(x = rnorm(100), y = rnorm(100))),
  tar_target(
    name = model,
    command = coefficients(lm(y ~ x, data = data))
  )
)
