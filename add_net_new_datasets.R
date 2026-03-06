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
  
  description_text = str_replace_all(description_text, "<br", "\n\n<br")
  
  # Remove all HTML entities.
  # Thanks to https://regex101.com/library/qS0gE2?orderBy=RELEVANCE&search=html
  description_text = str_remove_all(description_text, "<([^>]+)>")
  # description_text = str_replace_all(description_text, "<([^>]+)>", " ")
  
  # Remove multiple spaces between words
  
  # description_text = str_replace_all(description_text, "\\s{2,}", " ")
  
  # Remove many linebreaks to just 2
  description_text = str_replace_all(description_text, "\\n{2,}", "\n\n")
  
  # Remove leading linebreaks at the start of the description
  description_text = str_replace_all(description_text, "^\\n{2,}", "")
  
  # Add Markdown links for the GeoYukon links.
  description_text = str_replace_all(description_text, "Distributed from GeoYukon by the Government of Yukon.", "Distributed from [GeoYukon](https://yukon.ca/geoyukon) by the [Government of Yukon](https://yukon.ca/maps).")
  
  # Add Geomatics emails
  description_text = str_replace_all(description_text, "geomatics.help@yukon.ca", "[geomatics.help@yukon.ca](mailto:geomatics.help@yukon.ca)")
  description_text = str_replace_all(description_text, "geomatics.help@gov.yk.ca", "[geomatics.help@yukon.ca](mailto:geomatics.help@yukon.ca)")
  description_text = str_replace_all(description_text, "Geomatics.Help@gov.yk.ca", "[geomatics.help@yukon.ca](mailto:geomatics.help@yukon.ca)")
  
  description_text
  
}

# Thanks, Google
slugify <- function(x) {
  x %>%
    str_to_lower() %>%                        # Convert to lowercase
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% # Convert accented characters to ASCII before removing non-ASCII below
    str_replace_all("[^a-z0-9\\s-]", "") %>%  # Remove non-alphanumeric (except spaces/hyphens)
    str_squish() %>%                          # Remove extra whitespace
    str_replace_all("\\s+", "-")              # Replace spaces with hyphens
}

get_name_from_title <- function(title) {
  slugify(title)
}

add_geospatial_layer_ckan_package <- function(title, description, homepage_url, tags) {
  
  cat("Adding ", title, "to CKAN.\n")
  
  package_create(
    name = get_name_from_title(title),
    title = title,
    notes = description,
    license_id = "OGL-Yukon-2.0",
    # Geomatics Yukon
    owner_org = "f8301d90-0290-4456-ad98-df79d33b1bd6",
    
    extras = list(
      internal_contact_email = "Geomatics.Help@yukon.ca",
      internal_contact_name = "Geomatics Yukon",
    ),
    
    groups = data.frame(
      # Nature and environment
      id = "46be34d1-443f-4188-9639-692f2bda0e14",
      # Science and technology
      id = "a8dc4319-ae14-4568-b49e-57334fd3fa31"
    )
    
  )
  
  # TODO: add keywords/tags
  # TODO: add resource links to the newly-created resource
  
}

net_new_datasets <- net_new_datasets_dcat |> 
  mutate(
    description = clean_dcat_description_text(description)
  )

net_new_datasets$description
