skytain_stations <- here::here("data/augmentation_data/stops_translink.txt") %>%
  data.table::fread() %>%
  dplyr::filter(location_type == 1) %>%
  dplyr::rename(lng = stop_lon, lat = stop_lat) %>%
  dplyr::as_tibble()
bus_stops <- here::here("data/augmentation_data/stops_translink.txt") %>%
  data.table::fread() %>%
  dplyr::filter(location_type == 0) %>%
  dplyr::rename(lng = stop_lon, lat = stop_lat) %>%
  dplyr::as_tibble()
parks <- here::here("data/augmentation_data/parks_vancouver.csv") %>%
  data.table::fread() %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(
    Geom = stringr::str_remove_all(Geom, ".*\\[|\\].*"),
    lng = stringr::str_extract(Geom, ".*, ") %>%
      stringr::str_remove("\\,") %>%
      stringi::stri_trim_both() %>%
      as.numeric(),
    lat = stringr::str_extract(Geom, ", .*") %>%
      stringr::str_remove("\\,") %>%
      stringi::stri_trim_both() %>%
      as.numeric()
  ) %>%
  dplyr::select(-Geom)
schools <- here::here("data/augmentation_data/schools_vancouver.csv") %>%
  data.table::fread() %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(
    Geom = stringr::str_remove_all(Geom, ".*\\[|\\].*"),
    lng = stringr::str_extract(Geom, ".*, ") %>%
      stringr::str_remove("\\,") %>%
      stringi::stri_trim_both() %>%
      as.numeric(),
    lat = stringr::str_extract(Geom, ", .*") %>%
      stringr::str_remove("\\,") %>%
      stringi::stri_trim_both() %>%
      as.numeric()
  ) %>%
  dplyr::select(-Geom)

convenience_stores <- data.table::fread(
  here::here("data/augmentation_data/storefronts_inventory.csv")
) %>%
  janitor::clean_names() %>%
  dplyr::filter(
    !(retail_category %in% c("Vacant", "Vacant UC", "Unknown"))
  ) %>%
  dplyr::filter(
    retail_category == "Convenience Goods",
    stringr::str_detect(business_name, "Pharma", negate = TRUE)
  ) %>%
  dplyr::slice(1:20) %>%
  dplyr::pull(business_name)


convenience_stores <- data.table::fread(
  here::here("data/augmentation_data/storefronts_inventory.csv")
) %>%
  janitor::clean_names() %>%
  dplyr::filter(business_name %in% convenience_stores) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(
    lat = stringr::str_extract(geo_point_2d, ".*, ") %>%
      stringr::str_remove("\\,") %>%
      stringi::stri_trim_both() %>%
      as.numeric(),
    lng = stringr::str_extract(geo_point_2d, ", .*") %>%
      stringr::str_remove("\\,") %>%
      stringi::stri_trim_both() %>%
      as.numeric()
  )

food_beverages <- data.table::fread(
  here::here("data/augmentation_data/storefronts_inventory.csv")
) %>%
  janitor::clean_names() %>%
  dplyr::filter(
    retail_category %in% c("Food & Beverage")
  ) %>%
  dplyr::tibble() %>%
  dplyr::mutate(
    lat = stringr::str_extract(geo_point_2d, ".*, ") %>%
      stringr::str_remove("\\,") %>%
      stringi::stri_trim_both() %>%
      as.numeric(),
    lng = stringr::str_extract(geo_point_2d, ", .*") %>%
      stringr::str_remove("\\,") %>%
      stringi::stri_trim_both() %>%
      as.numeric()
  )

commerecial_services <- data.table::fread(
  here::here("data/augmentation_data/storefronts_inventory.csv")
) %>%
  janitor::clean_names() %>%
  dplyr::filter(
    retail_category %in% c("Service Commercial")
  ) %>%
  dplyr::tibble() %>%
  dplyr::mutate(
    lat = stringr::str_extract(geo_point_2d, ".*, ") %>%
      stringr::str_remove("\\,") %>%
      stringi::stri_trim_both() %>%
      as.numeric(),
    lng = stringr::str_extract(geo_point_2d, ", .*") %>%
      stringr::str_remove("\\,") %>%
      stringi::stri_trim_both() %>%
      as.numeric()
  )

crime <- data.table::fread(
  here::here("data/augmentation_data/crime2.csv")
) %>%
  janitor::clean_names() %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(
    type = ifelse(
      type == "Vehicle Collision or Pedestrian Struck (with Fatality)",
      "Vehicle Collision or Pedestrian Struck (with Injury)",
      type
    )
  )


# crime <- data.table::fread(
#   here::here("data/augmentation_data/crime.csv")
# ) %>% janitor::clean_names() %>%
#   dplyr::filter(hundred_block != "OFFSET TO PROTECT PRIVACY") %>%
#   dplyr::mutate(
#     hundred_block2 = hundred_block,
#     hundred_block = stringr::str_remove(hundred_block, ".*[0-9]+X.+? ")
#   ) %>%
#   dplyr::as_tibble()
#   dplyr::group_by(hundred_block, neighbourhood) %>%
#   dplyr::summarise(
#     n = dplyr::n()
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(address = paste(hundred_block, neighbourhood, ", Vancouver, BC")) %>%
#   dplyr::arrange(desc(n))
#
# library(arrow)
# library(tidyverse)
# library(googleway)
# library(janitor)
# library(config)
#
# config <- config::get()
# temp_df <- dplyr::tibble()
# for (i in 1:nrow(crime)) {
#   df <- googleway::google_geocode(
#     address = crime$address[i],
#     key = config$maps_api_key,
#     simplify = TRUE
#   )
#
#   temp <- df %>% .[["results"]]
#   temp_check <- temp %>% as_tibble()
#   if(nrow(temp_check) > 1) {
#     print("Bigger than 1")
#     temp <- temp %>% as_tibble()
#   }
#
#   formatted_google_address <- temp$formatted_address
#   location_type <- temp$geometry$location_type
#   address_components <- temp$address_components
#   lat <- temp$geometry$location[1]
#   lng <- temp$geometry$location[2]
#
#   maps_res <- dplyr::tibble(
#     formatted_google_address = formatted_google_address,
#     location_type = location_type,
#     address_components = list(address_components),
#     lat = lat$lat,
#     lng = lng$lng,
#     id = crime$hundred_block[i],
#   )
#   print(i)
#   temp_df <- temp_df %>%
#     dplyr::bind_rows(maps_res)
#
# }
#
# temp_df %>%
#   dplyr::distinct(id, lat, lng, .keep_all = TRUE) %>%
#   dplyr::right_join(
#     crime, by = c("id" = "hundred_block")
#   ) %>%
#   dplyr::distinct(type, year, month, day, hour, minute, x, y, .keep_all = TRUE) %>%
#   dplyr::select(
#     formatted_google_address, lat, lng, type, neighbourhood
#   ) %>%
#   readr::write_csv(here::here("data/augmentation_data/crime2.csv"))
