library(tidyverse)
library(DBI)
library(data.table)

source('palma_functions.R')
source('puma_functions.R')

# connect to PUMS database (too large for GitHub)
con <- dbConnect(RSQLite::SQLite(), "pums_db.db")

# initialize list to store palams; with each list element a different geography
palma_geo <- list()

# iterate through each geographic level, calculating Palma for all years
for (area in c('puma', 'county', 'state')) {

  palma_geo[[area]] <- palma_complete_years(con = con, 
                                            years = seq(2006, 2017), 
                                            level = area, 
                                            state = 37, 
                                            area_code = NA)
  
  # file name to write out
  write_file <- paste0('data/palma_', area, '.csv')
  
  write_csv(palma_geo[[area]], write_file)

}



### Create dataset to be used in analysis; place data in format to be used with shiny app

# import three palma files (state, county, puma) and combine into one
palma_files <- list.files('data', full.names = TRUE)

# import and combine all three
palmas <- lapply(palma_files, read_csv, col_types = 'ccinn') %>%
  bind_rows() %>%
  mutate(moe = se * 1.645,
         cv = (se / palma) * 100) %>%
  rename(geo_description = geography, type = level, estimate = palma) %>%
  # remove PUMAs prior to 2012 since boundaries changed
  filter(!(type == 'puma' & year < 2012))

# import puma code names
codes <- read_csv('puma_counties.csv') %>%
  # only keep puma12 name and number
  select(puma12, cntyname, PUMA12name) %>%
  # convert codes to character, so it can be merged with plama dataset
  mutate(puma12 = as.character(puma12)) %>%
  # delete duplicates
  unique()

# add PUMA names to rows that are for PUMA (not counties)
palmas_full <- left_join(palmas, codes, by = c('geo_description' = 'puma12')) %>%
  # if row is for a PUMA (not county) then use puma name for geography
  mutate(geo_description = ifelse(.$type == 'puma',
                                  .$PUMA12name, .$geo_description),
         # make the county name the geo_description for county areas
         cntyname = ifelse(.$type == 'county',
                            .$geo_description, .$cntyname)) %>%
  # make cntyname for state palmas 'North Carolina'
  mutate(cntyname = ifelse(.$type == 'state',
                           'North Carolina', .$cntyname),
         # make geo_description for the state 'North Carolina
         geo_description = ifelse(.$type == 'state',
                                  'North Carolina', .$geo_description),
         # pumas have ' NC' after the county name; remove this
         cntyname = str_replace_all(cntyname, ' NC', '')) %>%
  select(-PUMA12name) %>%
  rename(subtype = cntyname) %>%
  # order by geographic level, year,and county names
  arrange(type, year, subtype)

# write out full palmas
#write_csv(palmas_full, 'i_social_justice/data/palmas_full.csv')

# create dataset with only Forsyth, Guilford, Durham, and NC
geo_areas <- c('Forsyth', 'Guilford', 'Durham', 'North Carolina')

palmas_comparison <- palmas_full %>%
  filter(subtype %in% geo_areas)

# write out comparison dataset
#write_csv(palmas_comparison, 'i_social_justice/data/palmas_comparison.csv')