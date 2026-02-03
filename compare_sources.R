library(tidyverse)

source("load_dcat.R")

source("load_ckan.R")

# Todo:
# Net net datasets (not currently in CKAN)


# Determine which datasets need updates -----------------------------------

compare_dcat_datasets <- dcat_datasets |> 
  select(
    title,
    modified
  ) |> 
  rename(
    dcat_modified = "modified"
  ) |> 
  mutate(
    # Convert "2022-05-03T20:36:08.000Z" to "2022-05-03T20:36:08"
    dcat_modified = str_sub(dcat_modified, 0L, 19L)
  )

compare_ckan_datasets <- geoyukon_import_datasets |> 
  select(
    title,
    metadata_modified
  ) |> 
  rename(
    ckan_modified = "metadata_modified"
  ) |> 
  mutate(
    # Convert "2025-12-02T21:54:21.502826" to "2025-12-02T21:54:21"
    ckan_modified = str_sub(ckan_modified, 0L, 19L)
  )

compare_datasets <- compare_dcat_datasets |> 
  left_join(
    compare_ckan_datasets,
    by = "title"
  )

# Remove datasets that haven't been updated on DCAT since the CKAN updates
compare_datasets <- compare_datasets |> 
  filter(
    dcat_modified > ckan_modified
  )

datasets_to_update <- compare_datasets |> 
  pull(title)

# Compare resources -------------------------------------------------------

# Todo: add in the metadata URLs from the DCAT feed dataset

compare_resources_dcat <- dcat_resources |> 
  select(
    dataset_title,
    resource_url,
    resource_title
  ) |> 
  rename(
    dcat_resource_url = "resource_url",
    resource_title = "resource_title"
  ) |> 
  filter(
    dataset_title %in% datasets_to_update
  )

compare_resources_ckan <- geoyukon_import_dataset_resources |> 
  select(
    title,
    url,
    name
  ) |> 
  rename(
    dataset_title = "title",
    ckan_resource_url = "url",
    resource_title = "name"
  ) |> 
  filter(
    dataset_title %in% datasets_to_update
  )

compare_resources <- compare_resources_dcat |> 
  left_join(
    compare_resources_ckan,
    by = c("dataset_title", "resource_title")
  )

compare_resources <- compare_resources |> 
  mutate(
    dcat_url_is_updated = case_when(
      dcat_resource_url != ckan_resource_url ~ "updated",
      is.na(ckan_resource_url) ~ "new",
      .default = "unchanged"
    )
  )

resources_to_update <- compare_resources |> 
  filter(dcat_url_is_updated == "updated") |> 
  select(
    dataset_title,
    resource_title,
    dcat_resource_url
  )
