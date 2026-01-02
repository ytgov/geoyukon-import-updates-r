library(tidyverse)
library(jsonlite)
library(fs)
library(readxl)
library(rmarkdown)
library(janitor)

# Source files ================

geoyukon_dcat_url <- "https://metadata-yukon.hub.arcgis.com/api/feed/dcat-us/1.1.json"

json_file_path <- str_c("input/", today(), "/1.1.json")

# Download and process JSON ===========

if(! file_exists(json_file_path)) {
  
  cat("Downloading JSON file")
  
  dir_create(str_c("input/", today()))
  
  # Two-stage download
  download.file(
    url = geoyukon_dcat_url, 
    destfile = json_file_path,
    mode = "wb")
  
  
  
} else {
  cat("JSON file already downloaded")
}

json_data <- read_json(json_file_path)

# datasets <- json_data[["dataset"]]

raw_datasets <- tibble(datasets = json_data[["dataset"]])

datasets <- raw_datasets |> 
  hoist(
    datasets,
    title = "title",
    description = "description",
    arcgis_id = "agol",
    issued = "issued",
    modified = "modified",
    license = "license"
        )

distribution_resources <- raw_datasets |> 
  hoist(
    datasets,
    dataset_title = "title",
    arcgis_id = "agol",
    distribution = "distribution"
  ) |> 
  select(! datasets) |> 
  unnest_longer(distribution) |> 
  unnest_wider(distribution)

links_resources <- raw_datasets |> 
  hoist(
    datasets,
    dataset_title = "title",
    arcgis_id = "agol",
    links = "links"
  )

# Thanks to
# https://stackoverflow.com/a/53230530
links_resources <- links_resources |> 
  mutate(
    links = map(links, as.list)
  ) |> 
  select(! datasets) |> 
  unnest_longer(links)

# Second round of unnesting, due to JSON structure
links_resources <- links_resources |> 
  mutate(
    links = map(links, as.list)
  ) |> 
  select(! links_id) |> 
  unnest_longer(links)

# Third round of unnesting
links_resources <- links_resources |> 
  mutate(
    links = map(links, as.list)
  ) |> 
  select(! links_id) |> 
  unnest_longer(links)

# Remove entries without links
links_resources <- links_resources |> 
  filter(links_id == "linkage") |> 
  select(! links_id)

# Add labels to links ===========================

resources_output <- links_resources |> 
  rename(
    resource_url = "links"
  ) |> 
  mutate(
    resource_title = case_when(
      str_detect(resource_url, "map-data.service.yukon.ca") ~ "Shape files",
      str_detect(resource_url, "yukon.maps.arcgis.com/home") ~ "ArcGIS Online layers",
      str_detect(resource_url, "mapservices.gov.yk.ca/imagery") ~ "Image server",
      .default = "(Resource title)"
    )
  )


# Cleanup

datasets <- datasets |> 
  filter(description != "") |> 
  filter(description != "{{description}}")


