
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shoe3d

<!-- badges: start -->

<!-- badges: end -->

## Data Source

The shoe data is located at
`\\iastate.edu\lss\research\csafe-shoeprints`. For users on VPN or using
personal devices, the \\iastate.edupath may not work. In that case,
replace it with \\las-dfs-01.las.iastate.edu(for example Shoe data would
become \\las-dfs-01.las.iastate.edu-shoeprints).

To start with, I copied `005*` and `006*` stl files into `data-raw/`

``` r
usethis::use_data_raw() # set up directory
lss_location <- file.path("/lss", "research", "csafe-shoeprints", "ShoeImagingPermanent") # this is true on bigfoot, won't work on windows. 
file.copy(from = list.files(lss_location, "00[56]\\d{3}[LR].*\\.stl"), to = "data-raw/", overwrite = F)
writeLines("*.stl$", con = "data-raw/.gitignore")
```

These files can’t be added to github - they’re too big.

## Other Details:

Please note that the ‘shoe3d’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
