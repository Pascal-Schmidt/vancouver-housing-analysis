library(arrow)
library(tidyverse)
library(googleway)
library(janitor)
library(config)

config <- config::get()

# get website names from folder
websites <- here::here("data/housing_data") %>%
  list.files()
path_to_website <- here::here(stringr::str_glue("data/housing_data/{websites}"))

schemas <- list()
# create schemas for every raw scraped website
for (i in seq_along(path_to_website)) {
  print(i)
  parquet_files <- list.files(
    path_to_website[i]
  ) %>% .[stringr::str_detect(., "parquet")]

  schemas_parquet <- list()
  for (j in seq_along(parquet_files)) {
    schemas_parquet[[j]] <- arrow::open_dataset(
      here::here(stringr::str_glue("data/housing_data/{websites[i]}/{parquet_files[j]}"))
    )$schema
  }
  schemas[[i]] <- do.call("unify_schemas", schemas_parquet)
}


create_lat_lng_dfs <- function(website, path_to_website_arg, schemas_arg) {
  exclude_dates <- paste0(path_to_website_arg, "/maps=Yes") %>%
    list.files() %>%
    stringr::str_remove("date=")

  raw_df <- dplyr::tibble()
  # read in raw data
  if (website == "craigslist") {
    # clean some data and filter out dates where we already ran the google maps api
    raw_df <- arrow::open_dataset(
      here::here("data/housing_data/craigslist"),
      schema = schemas_arg
    ) %>%
      dplyr::arrange(date) %>%
      dplyr::filter(!(as.character(date) %in% exclude_dates)) %>%
      dplyr::collect() %>%
      janitor::clean_names() %>%
      dplyr::arrange(as.integer(id)) %>%
      dplyr::rename(
        beds = number_of_bedrooms,
        baths = number_of_bathrooms_total,
      ) %>%
      dplyr::mutate(address = paste(address_street_address, address_address_locality, address_postal_code)) %>%
      dplyr::distinct(price, type, address, beds, baths, .keep_all = TRUE) %>%
      dplyr::filter(
        nchar(address_street_address) > 4
      )
  } else if (website == "rew") {
    raw_df <- arrow::open_dataset(
      here::here("data/housing_data/rew"),
      schema = schemas_arg
    ) %>%
      dplyr::arrange(date) %>%
      dplyr::filter(!(as.character(date) %in% exclude_dates)) %>%
      dplyr::collect() %>%
      dplyr::rename(
        price = `List Price`,
        beds = Bedrooms,
        baths = Bathrooms,
        type = `Property Type`
      ) %>%
      dplyr::distinct(price, type, address, beds, baths, .keep_all = TRUE) %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        lot_size = ifelse(
          stringr::str_detect(
            lot_size, "\\)"
          ),
          stringr::str_extract(lot_size, "\\(.*\\)"),
          lot_size
        ),
        acre_yes_no = ifelse(
          stringr::str_detect(
            lot_size, "acre"
          ),
          "acre",
          "no acre"
        ),
        lot_size = readr::parse_number(lot_size),
        lot_size = ifelse(
          acre_yes_no == "acre",
          lot_size * 43560,
          lot_size
        )
      )
  } else if (website == "zumper") {
    raw_df <- arrow::open_dataset(
      here::here("data/housing_data/zumper"),
      schema = schemas_arg
    ) %>%
      dplyr::collect() %>%
      dplyr::filter(!(as.character(date) %in% exclude_dates)) %>%
      dplyr::filter_at(vars(dplyr::contains("_ft")), any_vars(!is.na(.))) %>%
      tidyr::unite(
        col = ft22, dplyr::contains("_ft2"), sep = ", ",
        remove = TRUE
      ) %>%
      dplyr::mutate(
        ft22 = gsub("NA,", "", ft22),
        ft2 = gsub(",", "", ft22) %>%
          stringr::str_squish(),
        pets_allowed = dplyr::case_when(
          !is.na(no_pets) ~ "No",
          !is.na(dogs_cats) ~ "Yes",
          !is.na(dogs) ~ "Yes",
          !is.na(cats) ~ "Yes",
          TRUE ~ "NA"
        )
      ) %>%
      dplyr::select(-c(dplyr::contains("ago"), "no_pets", "dogs_cats", "ft22")) %>%
      tidyr::unite(
        col = bath2, dplyr::matches("(bath|bathroom)"), sep = ", ",
        remove = FALSE
      ) %>%
      dplyr::mutate(
        bath2 = gsub("NA,", "", bath2),
        bath2 = gsub(",", "", bath2) %>%
          stringr::str_squish(),
        bath2 = ifelse(
          stringr::str_detect(bath2, "Half Bath"),
          stringr::str_replace(bath2, "[0-9]+ Half Bath", ".5"),
          bath2
        ),
        bath2 = stringr::str_replace(bath2, " Full ", ""),
        beds = ifelse(is.na(beds), studio, beds)
      ) %>%
      dplyr::mutate_at(vars(beds, bath2, ft2, price), ~ readr::parse_number(.)) %>%
      dplyr::select(-c(header, dplyr::contains("_bath"), dplyr::contains("_baths"), dogs, bed, studio, cats, bathroom, bathrooms)) %>%
      dplyr::mutate(
        type = dplyr::case_when(
          stringr::str_detect(description, "apartment") ~ "apartment",
          stringr::str_detect(description, "balcony") ~ "apartment",
          stringr::str_detect(description, "condo") ~ "condo",
          stringr::str_detect(description, "townhouse") ~ "townhouse",
          stringr::str_detect(description, "house") ~ "house",
          stringr::str_detect(description, "garage") ~ "house",
          stringr::str_detect(description, "basement") ~ "basement",
          TRUE ~ "Apartment"
        )
      ) %>%
      dplyr::select(
        id = ID, address = location,
        price, date, beds, baths = bath2,
        square_feet = ft2, type
      ) %>%
      dplyr::distinct(price, address, date, beds, baths, .keep_all = TRUE)
  } else if (website == "point2home") {
    raw_df <- arrow::open_dataset(
      here::here("data/housing_data/point2home"),
      schema = schemas_arg
    ) %>%
      dplyr::collect() %>%
      dplyr::filter(!(as.character(date) %in% exclude_dates)) %>%
      dplyr::select(beds:Title, date)
  } else if (website == "remax") {
    raw_df <- arrow::open_dataset(
      here::here("data/housing_data/remax"),
      schema = schemas_arg
    ) %>%
      dplyr::collect() %>%
      dplyr::filter(!(as.character(date) %in% exclude_dates))
  } else if (website == "livrent") {
    raw_df <- arrow::open_dataset(
      here::here("data/housing_data/livrent"),
      schema = schemas_arg
    ) %>%
      dplyr::collect() %>%
      janitor::clean_names() %>%
      dplyr::mutate(date = lubridate::ymd("2023-01-01")) %>%
      dplyr::select(id, type, price, address = location, beds = bedrooms, baths = bathrooms, square_feet = ft2, date) %>%
      dplyr::filter_all(~ !is.na(.)) %>%
      dplyr::mutate_at(vars(price, beds:square_feet), ~ readr::parse_number(.)) %>%
      dplyr::mutate(
        type = stringr::str_remove(type, "Entire ")
      ) %>%
      dplyr::filter(!(as.character(date) %in% exclude_dates))
  } else if (website == "zolo") {
    print("zolo")
    raw_df <- arrow::open_dataset(
      here::here("data/housing_data/zolo"),
      schema = schemas_arg
    ) %>%
      dplyr::collect() %>%
      janitor::clean_names() %>%
      dplyr::select(
        id, type, price, year_built, lot_size,
        address = location, beds = bed, baths = bath, square_feet = size, date
      ) %>%
      dplyr::filter_all(~ !is.na(.)) %>%
      dplyr::mutate(
        price = stringr::str_remove_all(price, "est.*") %>%
          readr::parse_number()
      ) %>%
      dplyr::mutate_at(vars(beds:square_feet, lot_size), ~ readr::parse_number(.)) %>%
      dplyr::filter(!(as.character(date) %in% exclude_dates))
  } else {
    return()
  }

  if (nrow(raw_df) > 0) {
    print(website)
    # bind columns to data frame and write out folder with lat and lng data
    insert_lat_lng_maps_api(
      raw_df  = raw_df,
      api_key = config$maps_api_key,
      website = website
    )
  } else {
    return()
  }
}

for (i in seq_along(websites)) {
  create_lat_lng_dfs(
    website = websites[[i]],
    path_to_website_arg = path_to_website[[i]],
    schemas_arg = schemas[[i]]
  )
}
