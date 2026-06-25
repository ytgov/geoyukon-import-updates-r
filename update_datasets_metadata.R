source("compare_sources.R")


# Manually filter to specific datasets to update --------------------------

# dataset_descriptions_to_update <- geoyukon_import_datasets %>% 
#   as_tibble() %>% 
#   filter(
#     str_detect(title, "(Legacy)")
#   ) %>% 
#   select(name) %>% 
#   pull(name)

dataset_descriptions_to_update <- c(
  "dawson-land-management-units-250k"
)

# Compare dataset descriptions ==============================

compare_dataset_descriptions <- geoyukon_import_datasets |> 
  select(
    name,
    title,
    notes
  )

dcat_dataset_descriptions <- dcat_datasets |> 
  select(
    title,
    description,
    keyword
  ) |> 
  rename(
    dcat_raw_description = "description"
  )

compare_dataset_descriptions <- compare_dataset_descriptions |> 
  left_join(dcat_dataset_descriptions, by = "title")

compare_dataset_descriptions <- compare_dataset_descriptions |> 
  mutate(
    dcat_clean_description = clean_dcat_description_text(dcat_raw_description)
  ) |> 
  mutate(
    description_is_up_to_date = case_when(
      notes == dcat_clean_description ~ TRUE,
      .default = FALSE
    )
  )

# compare_dataset_descriptions |> 
#   select(
#     name, title, notes, dcat_clean_description
#   ) |> 
#   write_csv("output/temp-dataset-description-comparison.csv")



# Update dataset descriptions! --------------------------------------------

descriptions_to_update <- compare_dataset_descriptions |> 
  filter(name %in% dataset_descriptions_to_update)

for (i in seq_along(descriptions_to_update$name)) { 
  
  package_id = descriptions_to_update$name[i]
  dataset_description = descriptions_to_update$dcat_clean_description[i]
  tags = convert_dcat_keywords_to_tags(descriptions_to_update$keyword[[i]])
  
  cat("Updating description for ", package_id)
  
  package_patch(
    list(
      id = package_id,
      notes = dataset_description
      # tags = tags
    )
  )
  
}
