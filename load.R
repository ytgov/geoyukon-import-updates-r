library(tidyverse)
library(fs)
library(readxl)
library(rmarkdown)
library(janitor)

# This file is adapted from the DKAN to CKAN conversion script here,
# https://github.com/ytgov/dkan-to-ckan-csv-migration-r/blob/main/load.R

# Source files ========================

source_geoyukon_dataset_file <- "input/20260102/datasets.csv"
source_geoyukon_resources_file <- "input/20260102/resources.csv"

# Helper functions ========================

# Note: DKAN currently expects CRLF line endings (not LF) and so those have been specified in the project-specific R settings.
# TODO: Confirm what line endings are needed by Link Digital's CKAN infrastructure.

# This script exports CSV files that don't have BOMs (which Excel adds, and that DKAN can't process).

# Writes a CSV file to the "out/" directory, and returns the data if necessary for future piped functions.
write_out_csv <- function(df, filename, na = "") {
  
  df %>%
    write_csv(
      str_c(filename, ".csv"), 
      na = na,
      eol = "\r\n",
    )
  
  df
  
}

filter_is_published <- function(df) {
  
  df |> 
    filter(is_published == 1)
  
}

mutate_languages <- function(df) {
  
  df |> 
    mutate(
      language = case_when(
        languages == "en-CA" ~ "english",
        languages == "en" ~ "english",
        languages == "en-CA,fr-CA" ~ "multiple_languages",
        languages == "fr-CA" ~ "french",
        .default = ""
      )
    ) |> 
    relocate(
      language, .before = "languages"
    )
  
}

mutate_license_id <- function(df) {
  
  df |> 
    mutate(
      license_id	 = "OGL-Yukon-2.0"
    ) |> 
    relocate(
      license_id, .before = "licence"
    )
  
}

str_datetime_to_time <- function(str) {
  
  str_sub(str, 0L, 10L)
  
}

str_fme_datetime_to_datetime <- function(str) {
  # From 2025-06-24T10:08:15.914Z
  # to   2025-06-24 19:46:34
  
  str_replace_all(str_sub(str, 0L, 19L), "T", " ")
  
}

# Processing FME dataset output ========================


geoyukon_datasets <- read_csv(source_geoyukon_dataset_file) |> 
  clean_names()

# Used by rename(), where new = "old"
field_mapping <- c(
  node_id = "id",
  publishers_groups = "publisher",
  homepage_url = "homepage",
  custodian = "author_name",
  last_revised = "modified_date"
  
)

geoyukon_datasets <- geoyukon_datasets |> 
  rename(all_of(field_mapping))

geoyukon_datasets <- geoyukon_datasets |> 
  mutate(
    tags = str_c(tags, ",geoyukon-import,geoyukon-import-20250714"),
    content_type = "dataset",
    schema_type = "data",
    node_id = node_id + geoyukon_node_starting_id,
    last_revised = str_fme_datetime_to_datetime(last_revised),
    released_date = str_fme_datetime_to_datetime(released_date),
    # topics = str_to_sentence(topics)
  )

# TODO - additional cleanup here
# [done] fix mailto: emails
# [done] move tag cleanup afterwards
# [done] move frequency cleanup afterwards
# [done] remove data dictionary
# [done] check that dates created and modified look good.
# [done] fix topics
# [done] fix frequency
# [done] add created date to original FME output

# Remove single template(?) entry with a description of {{description}}
geoyukon_datasets <- geoyukon_datasets |> 
  filter(description != "{{description}}")

# Fix a single entry with a topic of "Nature and evnvironment"
# Should be "Nature and environment"
geoyukon_datasets <- geoyukon_datasets |> 
  mutate(
    topics = str_replace_all(topics, "Nature and evnvironment", "Nature and environment")
  )

geoyukon_datasets <- geoyukon_datasets |> 
  mutate(
    contact_email = "Geomatics.Help@yukon.ca",
    data_dictionary = NA_character_,
    authored = released_date
  )


# Processing FME dataset resources output =====

# SC03. Import GeoYukon resources and associate them together.
geoyukon_resources <- read_csv(source_geoyukon_resources_file) |> 
  clean_names()

geoyukon_resources <- geoyukon_resources |> 
  filter(title != "{{name}}")

geoyukon_resources <- geoyukon_resources |> 
  mutate(
    dataset_id = dataset_id + geoyukon_node_starting_id,
    # schema_type = "data",
    format_raw = str_to_upper(format)
  )

geoyukon_resources <- geoyukon_resources |> 
  rename(
    url = "link",
    dataset_node_id = dataset_id
  )

geoyukon_dataset_resource_parents <- geoyukon_datasets |> 
  select(schema_type, node_id, title, publishers_groups, last_revised, authored) |> 
  rename(
    dataset_node_id = "node_id",
    dataset_title = "title",
    organization_title = "publishers_groups"
  )

geoyukon_resources <- geoyukon_resources |> 
  left_join(geoyukon_dataset_resource_parents, by = "dataset_node_id")

# Update resource titles to be more descriptive
geoyukon_resources <- geoyukon_resources |> 
  mutate(
    title = case_when(
      format == "html" ~ "ArcGIS Online layers",
      format == "ftp" ~ "Shape files",
      format == "xml" ~ "XML metadata"
    )
  )

# Fix for &amp; URL errors from ArcGIS Online
geoyukon_resources <- geoyukon_resources |> 
  mutate(
    url = str_replace_all(url, "&amp;", "&")
  )