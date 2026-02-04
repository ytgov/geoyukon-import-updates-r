library(tidyverse)
library(fs)
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

# SSL proxying to monitor with Charles
# crul::set_verbose()
# crul::curl_verbose(data_out = TRUE, data_in = TRUE, info = TRUE, ssl = TRUE)
# crul::set_proxy(crul::proxy(url = "https://127.0.0.1:8888"))

ckan_organizations <- organization_list(
  as = "table",
  verbose = TRUE,
  ssl_verifyhost = FALSE,
  ssl_verifypeer = FALSE
)

resource_id <- "39410ff5-5515-3a52-9c37-19bd75c3b6c9"
updated_resource_url <- "https://yukon.maps.arcgis.com/home/item.html?id=4c7259f4d84f4f6fb7e490d309b53449"

# This does not work:
resource_update(
  id = resource_id,
  extras = list(url = updated_resource_url),
  # rcurl = "https://yukon.maps.arcgis.com/home/item.html?id=4c7259f4d84f4f6fb7e490d309b53449",
  verbose = TRUE,
  ssl_verifyhost = FALSE,
  ssl_verifypeer = FALSE
)

# x <- list(url = "https://yukon.maps.arcgis.com/home/item.html?id=4c7259f4d84f4f6fb7e490d309b53449")

# This works!
ckan_action(
  "resource_update",
  body = list(
    id = resource_id,
    url = updated_resource_url
  ),
  verbose = TRUE,
  ssl_verifyhost = FALSE,
  ssl_verifypeer = FALSE
)
