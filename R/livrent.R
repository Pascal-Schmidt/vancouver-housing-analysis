get_liv_rent_links <- function() {
  Sys.setenv(
    https_proxy = stringr::str_glue("http://user-{creds$user_name}:{creds$password}@gate.smartproxy.com:7000")
  )

  base <- "https://liv.rent/rental-listings/city/"
  cities <- c("Vancouver", "Burnaby", "Surrey", "Richmond")
  urls_for_cities <- purrr::map2_chr(
    .x = cities, .y = base,
    ~ paste0(.y, .x)
  )

  urls_to_house_page <- c()
  for (i in seq_along(urls_for_cities)) {
    # get base urls for each city
    city <- urls_for_cities[i] %>%
      xml2::read_html()
    Sys.sleep(runif(1, min = 1, max = 5))

    # get the total page number
    num_pages <- city %>%
      rvest::html_elements(".dYlnez") %>%
      rvest::html_text() %>%
      .[length(.) - 1] %>%
      as.integer()

    # build all page urls for each city
    urls <- urls_for_cities[i] %>%
      paste0("?page=", 2:num_pages) %>%
      c(urls_for_cities[i], .)

    print(i)

    for (j in seq_along(urls)) {
      # get urls to individual housing data
      urls_to_house_page_temp <- urls[j] %>%
        xml2::read_html()
      Sys.sleep(runif(1, min = 1, max = 5))
      print(j)
      urls_to_house_page <- urls_to_house_page_temp %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href") %>%
        .[stringr::str_detect(., "rental-listings/detail")] %>%
        paste0("https://liv.rent", .) %>%
        c(urls_to_house_page, .)
      print(urls_to_house_page)
      # write urls out
      arrow::write_parquet(
        dplyr::tibble(
          urls = unique(urls_to_house_page),
          date = Sys.Date()
        ),
        stringr::str_glue("data/urls/livrent/{Sys.Date()}.parquet")
      )
    }
  }
}

# get_liv_rent_links(urls_for_cities)




get_livrent_data <- function(urls) {
  Sys.setenv(
    https_proxy = stringr::str_glue("http://user-{creds$user_name}:{creds$password}@gate.smartproxy.com:7000")
  )

  urls_to_house <- urls

  final_df <- dplyr::tibble()
  for (i in seq_along(urls_to_house)) {
    tryCatch(
      {
        page <- xml2::read_html(urls_to_house[i])
        Sys.sleep(runif(1, 2, 5))

        type <- page %>%
          rvest::html_nodes(".bqPpVV") %>%
          rvest::html_text(trim = TRUE) %>%
          .[1]

        location <- page %>%
          rvest::html_nodes("h1") %>%
          rvest::html_text(trim = TRUE) %>%
          .[1]

        details <- page %>%
          rvest::html_nodes(".gPKLlL") %>%
          rvest::html_text(trim = TRUE) %>%
          purrr::set_names(stringr::str_remove(., ".* ")) %>%
          dplyr::bind_rows()

        price <- page %>%
          rvest::html_nodes(".dQVbOr") %>%
          rvest::html_text(trim = TRUE)

        amenities <- page %>%
          rvest::html_nodes(".kbCOeX") %>%
          rvest::html_text() %>%
          list()

        amenities_2 <- page %>%
          rvest::html_nodes(".cOXQKd") %>%
          rvest::html_text() %>%
          list()

        remDr$navigate(urls_to_house[i])
        Sys.sleep(runif(1, 5, 6))

        description <- tryCatch(
          {
            webElem <- remDr$findElements(using = "class", value = "jOmXHt")
            webElem[[1]]$clickElement()
            Sys.sleep(runif(1, 1, 2))

            webElem <- remDr$findElements(using = "class", value = "ftOGWz")
            description <- webElem[[1]]$getElementText() %>%
              .[[1]]
          },
          error = function(e) {
            cat("ERROR in Selenium:", conditionMessage(e), "\n")
          }
        )

        final_df <- dplyr::bind_cols(
          ID = i,
          type = type,
          price = price,
          location = location,
          details,
          dplyr::tibble(amenities = amenities),
          dplyr::tibble(amenities_2 = amenities_2),
          description = description,
          url = urls_to_house[i],
          date = Sys.Date()
        ) %>%
          dplyr::bind_rows(
            final_df, .
          )

        dplyr::bind_cols(
          ID = i,
          type = type,
          price = price,
          location = location,
          details,
          dplyr::tibble(amenities = amenities),
          dplyr::tibble(amenities_2 = amenities_2),
          description = description,
          url = urls_to_house[i],
          date = Sys.Date()
        ) %>%
          print()
      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      }
    )

    arrow::write_parquet(
      final_df,
      stringr::str_glue("data/housing_data/livrent/{Sys.Date()}.parquet")
    )
  }

  container_name <- system('docker ps --format "{{.Names}}"', intern = TRUE)
  system(stringr::str_glue("docker stop {container_name}"))
}
