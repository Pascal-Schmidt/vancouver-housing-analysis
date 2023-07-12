library(googleway)
library(arrow)
library(tidyverse)

list.files(here::here("data/housing_data")) %>%
  purrr::map(
    ~ here::here("data/housing_data", .)
  )

schema_remax_raw <- arrow::open_dataset(
  "/Users/pascal/Desktop/personal/vancouver-housing/data/housing_data/remax/2023-01-24.parquet"
)$schema

remax_df <- arrow::open_dataset(
  "/Users/pascal/Desktop/personal/vancouver-housing/data/housing_data/remax",
  schema = schema_remax_raw
) %>%
  dplyr::filter(!is.na(address), !is.na(sqft), !is.na(type), !is.na(bed), !is.na(bath), !is.na(price)) %>%
  dplyr::collect()

schema_remax_gmaps <- schema(
  maps = string(),
  price = string(),
  sqft = string(),
  type = string(),
  bed = string(),
  bath = string(),
  date = date32(),
  formatted_google_address = string(),
  lat = float64(),
  lng = float64(),
  location_type = string()
)



schema_rew_raw <- arrow::open_dataset(
  "/Users/pascal/Desktop/personal/vancouver-housing/data/housing_data/rew/2023-01-25.parquet"
  )$schema

rew_df <- arrow::open_dataset(
  "/Users/pascal/Desktop/personal/vancouver-housing/data/housing_data/rew/2023-01-25.parquet",
  schema = schema_rew_raw
) %>%
  dplyr::collect()

temp_res <- dplyr::tibble()
for (i in 1:nrow(rew_df)) {

  description <- tryCatch({
    df <- googleway::google_geocode(
      address = remax_data$address[i],
      key = "AIzaSyALos4dDgQ02Lk7uAyqKWNuyv_HloKykZc",
      simplify = TRUE
    )

    temp <- df %>% .[["results"]]
    formatted_google_address <- temp$formatted_address
    location_type <- temp$geometry$location_type
    address_components <- temp$address_components
    lat <- temp$geometry$location[1]
    lng <- temp$geometry$location[2]

    maps_res <- dplyr::tibble(
      formatted_google_address = formatted_google_address,
      location_type = location_type,
      address_components = list(address_components),
      lat = lat$lat,
      lng = lng$lng,
      id = remax_data$id[i],
      address = remax_data$address[i]
    )

    temp_res <- temp_res %>%
      dplyr::bind_rows(
        maps_res
      )

    print(tail(temp_res, 3))

  }, error = function(e) {cat("ERROR :","No description", "\n")})

}

remax_data[1:5000, ] %>%
  dplyr::bind_cols(
    temp_res %>% View()
      dplyr::distinct(formatted_google_address, .keep_all = TRUE)
  ) %>%
  dplyr::select(address, formatted_google_address) %>%
  View()

temp_res %>%
  dplyr::mutate(
    diff = c(NA, diff(id))
  ) -> p

remax_data %>%
  dplyr::mutate(
    diff = c(NA, diff(id))
  ) -> p

delete <- which(p$diff == 0)
temp_res2 <- temp_res[-delete, ]

ff <- remax_data %>%
  dplyr::filter(address != "#1702-565 SMITHE STREETVancouver, BC, V6B 0E4") %>%
  dplyr::slice(1:3328) %>%
  dplyr::bind_cols(
    temp_res2[1:3328, ]
  )

remax_data %>%
  dplyr::filter(address != "#1702-565 SMITHE STREETVancouver, BC, V6B 0E4") %>%
  dplyr::slice(3328:5000) %>%
  dplyr::bind_cols(
    temp_res2[3328:5000, ]
  ) %>%
  dplyr::select(address, formatted_google_address) %>%
  dplyr::mutate(
    address1 = stringr::str_extract(address, "BC,.*") %>%
      stringr::str_remove("BC, "),
    formatted_google_address1 = stringr::str_extract(formatted_google_address, "BC.*") %>%
      stringr::str_remove("BC ") %>%
      stringr::str_remove(", Canada"),
  ) %>%
  tidyr::pivot_longer(-c(address, formatted_google_address)) %>%
  dplyr::group_split(name) -> j

j[[2]] %>%
  dplyr::select(formatted_google_address, name, value) %>%
  dplyr::inner_join(j[[1]] %>% dplyr::select(address, name, value), by = "value") %>%
  dplyr::distinct(address, .keep_all = TRUE) %>%
  dplyr::select(formatted_google_address, address) %>%
  dplyr::inner_join(temp_res2[3328:5000, ], by = "formatted_google_address") %>%
  dplyr::inner_join(
    remax_data %>%
      dplyr::filter(address != "#1702-565 SMITHE STREETVancouver, BC, V6B 0E4") %>%
      dplyr::slice(3328:5000), by = "address"
  ) %>%
  dplyr::filter(id.x == id.y) %>%
  dplyr::distinct(address, .keep_all = TRUE) %>%
  dplyr::distinct(formatted_google_address, .keep_all = TRUE) %>%
  dplyr::bind_rows(
    ff
  ) %>%
  dplyr::select(-dplyr::contains("id"))
  dplyr::mutate(maps = "Yes") %>%
  arrow::write_dataset(
    path = here::here("data/housing_data/remax"),
    partitioning = c("maps", "date"),
    existing_data_behavior = "delete_matching"
  )

