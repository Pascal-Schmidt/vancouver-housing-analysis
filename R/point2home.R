get_urls_point2home <- function() {
  cities <- c("Vancouver", "Burnaby", "Richmond", "Surrey")
  base_urls <- stringr::str_glue("https://www.point2homes.com/CA/Real-Estate-Listings/BC/{cities}.html")
  all_urls <- c()
  for (i in seq_along(base_urls)) {
    all_urls <- base_urls[i] %>%
      paste0(., "?page=", 2:20) %>%
      c(base_urls[i], .) %>%
      c(all_urls, .)
  }

  individual_urls <- c()
  for (i in seq_along(all_urls)) {
    print(all_urls[i])
    page <- all_urls[i] %>%
      xml2::read_html()
    Sys.sleep(runif(1, 2, 4))

    individual_urls <- page %>%
      rvest::html_elements(".item-address-title") %>%
      rvest::html_elements("a") %>%
      rvest::html_attr("href") %>%
      paste0("https://www.point2homes.com/", .) %>%
      c(individual_urls, .) %>%
      unique()

    print(length(individual_urls))

    # write urls out
    arrow::write_parquet(
      dplyr::tibble(
        urls = unique(individual_urls),
        date = Sys.Date()
      ),
      here::here(stringr::str_glue("data/urls/point2home/{Sys.Date()}.parquet"))
    )
  }
}

point2home_scraper <- function(urls) {
  p2h_df <- dplyr::tibble()
  for (i in seq_along(urls)) {
    tryCatch(
      {
        page <- xml2::read_html(urls[i])

        address <- page %>%
          rvest::html_elements(".address-container") %>%
          rvest::html_text() %>%
          stringr::str_remove_all("(\r|\n)") %>%
          stringr::str_squish()

        price <- page %>%
          rvest::html_elements(".property-summary-top") %>%
          rvest::html_elements(".price") %>%
          rvest::html_text() %>%
          stringr::str_remove_all("(\r|\n)") %>%
          stringr::str_squish()

        characteristics <- page %>%
          rvest::html_elements(".characteristics-cnt") %>%
          rvest::html_elements("li") %>%
          rvest::html_text() %>%
          stringr::str_remove_all("(\r|\n)") %>%
          stringr::str_squish() %>%
          .[1:4]

        names(characteristics) <- c("beds", "bahs", "square_feet", "type")
        characteristics <- dplyr::bind_rows(characteristics)

        char_names <- page %>%
          rvest::html_elements(".details-charcs") %>%
          rvest::html_elements("dl") %>%
          rvest::html_elements("dt") %>%
          rvest::html_text()

        char_values <- page %>%
          rvest::html_elements(".details-charcs") %>%
          rvest::html_elements("dl") %>%
          rvest::html_elements("dd") %>%
          rvest::html_text() %>%
          stringr::str_remove_all("(\r|\n)") %>%
          stringr::str_squish()

        names(char_values) <- char_names
        char_values <- dplyr::bind_rows(char_values)

        amenities_name <- page %>%
          rvest::html_elements(".features-list.cols") %>%
          rvest::html_elements(".features-list-title") %>%
          rvest::html_text()

        amenities <- page %>%
          rvest::html_elements(".features-list.cols") %>%
          rvest::html_elements("ul") %>%
          rvest::html_text() %>%
          stringr::str_remove_all("(\r|\n)") %>%
          stringr::str_squish()

        names(amenities) <- amenities_name
        amenities <- dplyr::bind_rows(amenities)

        p2h_df <- p2h_df %>%
          dplyr::bind_rows(
            characteristics %>%
              dplyr::bind_cols(
                id = i,
                price = price,
                address = address,
                char_values,
                amenities,
                date = Sys.Date(),
                url = urls[i]
              )
          )

        arrow::write_parquet(
          p2h_df,
          stringr::str_glue("data/housing_data/point2home/{Sys.Date()}.parquet")
        )
        print(p2h_df[nrow(p2h_df), ] %>% dplyr::select(id, address, square_feet, dplyr::everything()))
      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      }
    )
  }
}
