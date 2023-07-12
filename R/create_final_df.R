library(arrow)
library(tidyverse)
library(googleway)
library(janitor)
library(config)
library(data.table)
library(sf)
library(geosphere)

# arrow::read_parquet(here::here("data/housing_data/zolo/2023-03-08.parquet")) %>% View()

# main function to fill out distances
source(here::here("R/add_distance_var_to_df.R"))

# get data sets for placec
source(here::here("R/data_augmentation_data.R"))

# clean data
lookup <- c(square_feet = "sqft", beds = "bed", baths = "bath", baths = "bahs")
df <- list.files("data/housing_data", recursive = TRUE) %>%
  .[stringr::str_detect(., "maps")] %>%
  here::here("data/housing_data", .) %>%
  purrr::map2_dfr(
    .x = .,
    .y = stringr::str_extract(., paste0(list.files("data/housing_data"), collapse = "|")),
    ~ arrow::read_parquet(.) %>%
      dplyr::mutate(
        website = .y,
        rent_buy = ifelse(website %in% c("craigslist", "livrent", "zumper"), "rent", "buy")
      ) %>%
      rename(any_of(lookup)) %>%
      dplyr::mutate_at(
        vars(any_of(c("square_feet", "price", "beds", "baths", "lot_size"))),
        ~ readr::parse_number(as.character(.))
      )
  ) %>%
  dplyr::select(
    location_type, date_posted, square_feet, price, beds, baths, rent_buy,
    lat, lng, type, formatted_google_address, address_components, website, lot_size
  )


df <- df %>%
  dplyr::filter(
    stringr::str_detect(formatted_google_address, "Vancouver"),
    location_type %in% c("ROOFTOP", "APPROXIMATE")
  ) %>%
  dplyr::filter(!(website == "remax" & (type == "House"))) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    lot_size = ifelse(
      website == "point2home" & stringr::str_detect(type, "ac Lot Size"),
      type,
      lot_size
    ),
    type = ifelse(
      website == "point2home" & stringr::str_detect(type, "ac Lot Size"),
      "House",
      type
    ),
    type = dplyr::case_when(
      type %in% c("basement", "Apartment", "Condo", "Condominium", "apartment", "Apt/Condo", "condo") ~ "Apartment",
      type %in% c("House/Single Family", "House", "Single Family", "Residential", "house") ~ "House",
      type %in% c("1/2 Duplex", "Duplex") ~ "Duplex",
      type %in% c("townhouse", "Townhouse") ~ "Townhouse",
      TRUE ~ type
    ),
    beds = ifelse(is.na(beds), 0, beds),
    lot_size = ifelse(is.na(lot_size), 0, lot_size)
  ) %>%
  dplyr::filter(
    !is.na(square_feet),
    !is.na(beds),
    !is.na(baths),
    !is.na(lat),
    !is.na(lng),
    !is.na(type)
  ) %>%
  dplyr::mutate(
    lot_size = readr::parse_number(lot_size),
    lot_size = ifelse(
      website == "point2home",
      lot_size * 43560,
      lot_size
    )
  )

# vancouver map boundaries, shape file
boundaries <- read_sf("data/augmentation_data/local-area-boundary.shp")

# prepare for joining
sf_point_df <- sf::st_as_sf(
  df,
  coords = c("lng", "lat"),
  crs = st_crs(boundaries)
)

# get intersection of point and polygon to get neighborhood
df_augmented <- sf::st_join(sf_point_df, boundaries, join = st_intersects) %>%
  dplyr::filter(!is.na(name))

# get intersection of point and polygon to get neighborhood for crimes
sf_point_df <- sf::st_as_sf(
  crime,
  coords = c("lng", "lat"),
  crs = st_crs(boundaries)
)

crime_neighborhoods <- sf::st_join(sf_point_df, boundaries, join = st_intersects) %>%
  dplyr::filter(!is.na(name)) %>%
  dplyr::as_tibble() %>%
  dplyr::select(-c(geometry, mapid, formatted_google_address, neighbourhood)) %>%
  dplyr::group_by(name, type) %>%
  dplyr::summarise(
    n = dplyr::n()
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = type, values_from = n) %>%
  janitor::clean_names()

df_augmented <- df_augmented %>%
  dplyr::inner_join(
    crime_neighborhoods,
    by = "name"
  )

# extract postal codes from google results
df_augmented <- df_augmented %>%
  dplyr::as_tibble() %>%
  dplyr::rename(neighborhood = name) %>%
  dplyr::mutate(
    postal_code = purrr::map_chr(
      address_components,
      ~ purrr::pluck(., 1) %>%
        unnest(types) %>%
        dplyr::filter(types %in% c("country", "postal_code")) %>%
        dplyr::arrange(desc(types)) %>%
        dplyr::pull(long_name) %>%
        .[1]
    )
  ) %>%
  dplyr::mutate(
    lng = unlist(purrr::map(.$geometry, 1)),
    lat = unlist(purrr::map(.$geometry, 2))
  )

# get distances for places and sum of near by places
dfs_places <- list(
  skytain_stations,
  bus_stops,
  parks,
  schools,
  commerecial_services,
  food_beverages,
  convenience_stores
)
variables <- c(
  "sky_train", "bus_stop", "parks", "schools",
  "commercial_services", "food_beverages", "convenience_stores"
)
radius <- c(rep(0, 4), rep(500, 3))
method <- c(rep("min_dist", 4), rep("sum", 3))

# new_var <- variables[[i]]
# radius <- radius[[i]]
# method <- method[[i]]
# homes_df <- df_augmented
# places_df <- dfs_places[[i]]

homes_df <- df_augmented
startTime <- Sys.time()
purrr::pwalk(
  list(dfs_places, variables, method, radius),
  .f = function(a, b, c, d) {
    add_distance_var_to_df(
      new_var = b,
      homes_df = df_augmented,
      places_df = a,
      method = c,
      radius = d
    )
  }
)
endTime <- Sys.time()
# readr::write_csv(homes_df, here::here("data/analysis/df34.csv"))
# View(homes_df)

homes_df %>%
  readr::write_csv(here::here("data/analysis/df.csv"))
