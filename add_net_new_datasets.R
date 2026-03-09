source("compare_sources.R")

# Add net new datasets as new publications in CKAN


# net_new_dataset_titles <- net_new_datasets_dcat_export |> 
#   pull(title)
# 
# net_new_datasets

clean_dcat_description_text <- function(description_text) {
  
  # Pre-emptively add some line breaks.
  # Sometimes these are <p> and sometimes they are <p style="..."
  description_text = str_replace_all(description_text, "<p", "\n\n<p")
  
  # Sometimes they are capitalized! TODO: make this a better regex combined with the above
  description_text = str_replace_all(description_text, "<P", "\n\n<p")
  
  description_text = str_replace_all(description_text, "<br", "\n\n<br")
  description_text = str_replace_all(description_text, "<BR", "\n\n<br")
  
  # Remove all HTML entities.
  # Thanks to https://regex101.com/library/qS0gE2?orderBy=RELEVANCE&search=html
  description_text = str_remove_all(description_text, "<([^>]+)>")
  # description_text = str_replace_all(description_text, "<([^>]+)>", " ")
  
  # Remove multiple spaces between words
  
  # description_text = str_replace_all(description_text, "\\s{2,}", " ")
  
  # Remove many linebreaks to just 2
  description_text = str_replace_all(description_text, "\\n{2,}", "\n\n")
  
  # Remove &nbsp;-linebreak combinations
  # 
  description_text = str_replace_all(description_text, "\\n\\n&nbsp;\\n\\n", "\n\n")
  
  # Remove leading linebreaks at the start of the description
  description_text = str_replace_all(description_text, "^\\n{2,}", "")
  
  # Add Markdown links for the GeoYukon links.
  description_text = str_replace_all(description_text, "Distributed from GeoYukon by the Government of Yukon.", "Distributed from [GeoYukon](https://yukon.ca/geoyukon) by the [Government of Yukon](https://yukon.ca/maps).")
  
  # Add Geomatics emails
  description_text = str_replace_all(description_text, "geomatics.help@yukon.ca", "[geomatics.help@yukon.ca](mailto:geomatics.help@yukon.ca)")
  description_text = str_replace_all(description_text, "geomatics.help@gov.yk.ca", "[geomatics.help@yukon.ca](mailto:geomatics.help@yukon.ca)")
  description_text = str_replace_all(description_text, "Geomatics.Help@gov.yk.ca", "[geomatics.help@yukon.ca](mailto:geomatics.help@yukon.ca)")
  
  # Add Elections Yukon emails
  # TODO: standardize this into a generic email regex
  description_text = str_replace_all(description_text, "info@electionsyukon.ca", "[info@electionsyukon.ca](mailto:info@electionsyukon.ca)")
  
  description_text
  
}

# Thanks, Google
slugify <- function(x) {
  x %>%
    str_to_lower() %>%                        # Convert to lowercase
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% # Convert accented characters to ASCII before removing non-ASCII below
    str_replace_all("[^a-z0-9\\s-]", "") %>%  # Remove non-alphanumeric (except spaces/hyphens)
    str_squish() %>%                          # Remove extra whitespace
    str_replace_all("\\s+", "-") %>%          # Replace spaces with hyphens
    str_replace_all("-{2,}", "-")             # Replace multiple hyphens with singular ones
}

get_name_from_title <- function(title) {
  slugify(title)
}

get_date_stamp <- function() {
  
  date_stamp <- as.character(now())
  
  date_stamp |> 
    str_sub(0L, 10L) |> 
    str_replace_all("-", "")
    
  
}

convert_dcat_keywords_to_tags <- function(keywords, filter_short_tags = TRUE, add_geoyukon_datestamp = TRUE) {
  
  # tags <- tibble(name = net_new_datasets$keyword[[1]])
  tags <- tibble(raw_name = keywords)
  
  tags <- tags |> 
    mutate(
      name = get_name_from_title(raw_name)
    ) |> 
    select(name) |> 
    distinct()
  
  if(filter_short_tags == TRUE) {
    
    tags <- tags |> 
      filter(
        str_length(name) > 2
      )
    
  }
  
  if(add_geoyukon_datestamp == TRUE) {
    
    new_row = tibble_row(
      name = str_c("geoyukon-import-", get_date_stamp())
    )
    
    tags <- tags |>
      bind_rows(
        new_row
      ) |> 
      arrange(name)
    
  }
  
  tags
  
}

add_geospatial_layer_ckan_package <- function(title, description, homepage_url, keywords = list()) {
  
  cat("Adding ", title, "to CKAN.\n")
  
  tags <- convert_dcat_keywords_to_tags(keywords)
  
  newly_created_package <- package_create(
    name = get_name_from_title(title),
    title = title,
    notes = description,
    license_id = "OGL-Yukon-2.0",
    # Geomatics Yukon
    owner_org = "f8301d90-0290-4456-ad98-df79d33b1bd6",
    
    extras = list(
      internal_contact_email = "Geomatics.Help@yukon.ca",
      internal_contact_name = "Geomatics Yukon",
      homepage_url = homepage_url
    ),
    
    # REVIEW: Includes the first entry but not subsequent ones??
    # groups = data.frame(
    #   # Nature and environment
    #   id = "46be34d1-443f-4188-9639-692f2bda0e14",
    #   # Science and technology
    #   id = "a8dc4319-ae14-4568-b49e-57334fd3fa31"
    # ),
    # 
    groups = data.frame(
      id = c(
        # Nature and environment
        "46be34d1-443f-4188-9639-692f2bda0e14",
        # Science and technology
        "a8dc4319-ae14-4568-b49e-57334fd3fa31"
      )
    ),
    
    tags = tags
    
    
    
  )
  
  resources_to_add <- dcat_resources |> 
    filter(dataset_title == title)
  
  for (i in seq_along(resources_to_add$resource_url)) { 
    
    cat("Adding resource ", resources_to_add$resource_title[i], "\n")
    
    resource_create(
      package_id = newly_created_package$id,
      rcurl = resources_to_add$resource_url[i],
      name = resources_to_add$resource_title[i],
      format = "HTML"
    )
    
    Sys.sleep(0.1)
    
  }
  
  Sys.sleep(0.3)
  
}

net_new_datasets <- net_new_datasets_dcat |> 
  mutate(
    description = clean_dcat_description_text(description)
  )

net_new_datasets$description


for (i in seq_along(net_new_datasets$title)) { 
  
  add_geospatial_layer_ckan_package(
    net_new_datasets$title[i],
    net_new_datasets$description[i],
    net_new_datasets$arcgis_url[i],
    # Note syntax for list contents for keyword here
    net_new_datasets$keyword[[i]]
  )
  
  # For testing, just run one item:
  # break;
  
}
