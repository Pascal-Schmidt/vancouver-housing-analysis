get_urls_remmax <- function(max_page) {
  urls <- c()
  for (i in 1:max_page) {
    print(i)
    page <- xml2::read_html(stringr::str_glue("https://www.remax.ca/bc/vancouver-real-estate?pageNumber={i}"))
    urls <- urls %>%
      c(
        .,
        page %>%
          rvest::html_nodes("a") %>%
          rvest::html_attr("href") %>%
          .[stringr::str_detect(., "r[0-9]+-lst")]
      )

    # write urls out
    arrow::write_parquet(
      dplyr::tibble(
        urls = unique(urls),
        date = Sys.Date()
      ),
      stringr::str_glue("data/urls/remax/{Sys.Date()}.parquet")
    )
  }
}

remax_scraper <- function(urls) {
  df <- dplyr::tibble()
  for (i in seq_along(unique(urls))) {
    page2 <- tryCatch(
      {
        xml2::read_html(urls[[i]])
      },
      error = function(e) {
        cat("ERROR :", "Cannot read page", "\n")
      }
    )


    description <- tryCatch(
      {
        page2 %>%
          rvest::html_elements("#description") %>%
          rvest::html_text()
      },
      error = function(e) {
        cat("ERROR :", "No description", "\n")
      }
    )

    price <- tryCatch(
      {
        page2 %>%
          rvest::html_nodes("div[data-cy='property-price']") %>%
          rvest::html_text()
      },
      error = function(e) {
        cat("ERROR :", "No price", "\n")
      }
    )

    address <- tryCatch(
      {
        page2 %>%
          rvest::html_nodes("div[data-cy='property-address']") %>%
          rvest::html_text()
      },
      error = function(e) {
        cat("ERROR :", "No address", "\n")
      }
    )

    bed <- tryCatch(
      {
        page2 %>%
          rvest::html_nodes("span[data-cy='property-beds']") %>%
          rvest::html_text(trim = TRUE) %>%
          .[[1]]
      },
      error = function(e) {
        cat("ERROR :", "No beds", "\n")
      }
    )

    bath <- tryCatch(
      {
        page2 %>%
          rvest::html_nodes("span[data-cy='property-baths']") %>%
          rvest::html_text(trim = TRUE) %>%
          .[[1]]
      },
      error = function(e) {
        cat("ERROR :", "No baths", "\n")
      }
    )

    type <- tryCatch(
      {
        page2 %>%
          rvest::html_nodes("span[data-cy='property-type']") %>%
          rvest::html_text(trim = TRUE) %>%
          .[[1]]
      },
      error = function(e) {
        cat("ERROR :", "No Type", "\n")
      }
    )

    sqft <- tryCatch(
      {
        page2 %>%
          rvest::html_nodes("span[data-cy='property-sqft']") %>%
          rvest::html_text(trim = TRUE) %>%
          .[[1]]
      },
      error = function(e) {
        cat("ERROR :", "No sqft", "\n")
      }
    )

    tryCatch(
      {
        df <- df %>%
          dplyr::bind_rows(
            dplyr::tibble(
              id = i,
              price = price,
              description = description,
              address = address,
              sqft = sqft,
              type = type,
              bed = bed,
              bath = bath,
              date = Sys.Date(),
              urls = urls[i]
            )
          )
      },
      error = function(e) {
        cat("ERROR :", "Data set", "\n")
      }
    )

    arrow::write_parquet(
      df,
      stringr::str_glue("data/housing_data/remax/{Sys.Date()}.parquet")
    )
    print(df[nrow(df), ])
  }
}
