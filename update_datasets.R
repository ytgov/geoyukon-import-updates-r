source("compare_sources.R")

# Back to geoyukon CKAN resources

# CKAN resources: geoyukon_import_dataset_resources
# Resources to update: resources_to_update

ckan_resources_to_update <- geoyukon_import_dataset_resources |> 
  select(
    title,
    id,
    name,
    url
  ) |> 
  rename(
    dataset_title = "title",
    resource_id = "id",
    resource_title = "name"
  )

ckan_resources_to_update <- resources_to_update |> 
  left_join(
    ckan_resources_to_update,
    by = c("dataset_title", "resource_title")
  ) |> 
  select(! url)
  
ckan_resources_to_update_parameters <- ckan_resources_to_update |> 
  select(
    resource_id,
    dcat_resource_url
  ) |> 
  rename(
    url = dcat_resource_url
  ) |> 
  distinct()


# Loop through and update resources ---------------------------------------

for (i in seq_along(ckan_resources_to_update_parameters$resource_id)) { 
  cat("Updating", ckan_resources_to_update_parameters$resource_id[i], "to", ckan_resources_to_update_parameters$url[i], "\n")
  
  # resource_update(
  #   id = ckan_resources_to_update_parameters$resource_id[i],
  #   rcurl = ckan_resources_to_update_parameters$url[i]
  # )
  # resource_patch(
  #   
  # )
  
  break;
}
