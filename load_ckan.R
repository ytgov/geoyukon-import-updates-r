library(tidyverse)
library(fs)
library(readxl)
library(rmarkdown)
library(janitor)
library(ckanr)

run_start_time <- now()
paste("Start time:", run_start_time)

if(file_exists(".env")) {
  readRenviron(".env")
  
  ckan_url <- Sys.getenv("ckan_url")
  ckan_api_token <- Sys.getenv("ckan_api_token")
  
  geoyukon_import_tag <- Sys.getenv("geoyukon_import_tag")
  
} else {
  stop("No .env file found, create it before running this script.")
}

ckanr_setup(
  url = ckan_url, 
  key = ckan_api_token
)

ckan_organizations <- organization_list(
  as = "table"
)

geomatics_id <- ckan_organizations |> 
  filter(display_name == "Geomatics Yukon") |> 
  pull(id)

geomatics_organization <- organization_show(
  id = geomatics_id,
  # Only inclues 10 datasets:
  include_datasets = FALSE,
  as = "table"
)

# TODO: update this to also include other tags
# e.g. geoyukon-import-20260309 or others
# Or retrieve all packages across the Geomatics Yukon organization.

# geoyukon-import
# tag ID: b7c79d13-8d21-45b4-bb1a-a364a3db8c48
# Limited to 1000 results via tag_show

geoyukon_import_datasets_raw <- tag_show(
  id = geoyukon_import_tag,
  include_datasets = TRUE,
  as = "table"
)

# Rename IDs to avoid conflicts
# Hoist up the packages
# Compare to the DCAT results in another process

geoyukon_import_datasets <- geoyukon_import_datasets_raw |> 
  within(rm(vocabulary_id)) |> 
  as_tibble()

geoyukon_import_datasets <- geoyukon_import_datasets |> 
  select(packages) |> 
  unnest()

geoyukon_import_dataset_resources <- geoyukon_import_datasets |> 
  select(
    id,
    name,
    title,
    notes,
    owner_org,
    resources
  ) |> 
  rename(
    dataset_ckan_id = "id",
    dataset_name = "name"
  ) |> 
  unnest(
    resources
  )

