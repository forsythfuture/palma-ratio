# Deriving Palma ratios from Public Use Microdata Areas

The Palma ratio is a measure of income inequality and represents the ratio of incomes shares of the top 10% to the bottom 40%. It is derived by summing all incomes in the top 10% and bottom 40%, and then dividing the the summed income of the top 10% by the bottom 40%.

The income data used to calculate Palma ratios comes from the US Census's [Public Use Microdata Areas (PUMA)](https://www.census.gov/programs-surveys/acs/data/pums.html). Estimated federal and state income taxes, and FICA (payroll) taxes were removed from income. These taxes were estimated using the NBER's [tax simulation (TAXSIM) model](https://users.nber.org/~taxsim/taxsim27/). [This GitHub repo](https://github.com/forsythfuture/puma-to-taxsim) contains the scripts used to convert PUMA data into a format that can be read in the TAXSIM model.

We also adjusted post-tax household incomes based on household size. For the adjustments, we used the US Census's [equivalency scale](https://www.census.gov/topics/income-poverty/income-inequality/about/metrics/equivalence.html).

Finally, check out our accompanying [shiny app](https://forsythfutures.shinyapps.io/nc_palma_ratio/) based on North Carolina Palma ratios.
