insert_lat_lng_maps_api <- function(raw_df,
                                    api_key,
                                    website) {
  temp_res <- dplyr::tibble()
  for (i in 1:nrow(raw_df)) {
    description <- tryCatch(
      {
        df <- googleway::google_geocode(
          address = raw_df$address[i],
          key = api_key,
          simplify = TRUE
        )

        temp <- df %>% .[["results"]]
        temp_check <- temp %>% as_tibble()
        if (nrow(temp_check) > 1) {
          print("Bigger than 1")
          temp <- temp %>% as_tibble()
        }
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
          id = raw_df$id[i],
          address = raw_df$address[i]
        )

        temp_res <- temp_res %>%
          dplyr::bind_rows(
            maps_res
          )

        if (i %% 100 == 0) {
          print(tail(temp_res, 1))
        }
      },
      error = function(e) {
        cat("ERROR :", "No description", "\n")
      }
    )
  }

  raw_df %>%
    dplyr::inner_join(
      temp_res,
      by = c("id", "address")
    ) %>%
    dplyr::select(-dplyr::contains("id")) %>%
    dplyr::mutate(maps = "Yes") %>%
    arrow::write_dataset(
      path = here::here(stringr::str_glue("data/housing_data/{website}")),
      partitioning = c("maps", "date"),
      existing_data_behavior = "delete_matching"
    )
}
