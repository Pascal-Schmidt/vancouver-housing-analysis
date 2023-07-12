get_zumper_urls <- function() {
  Sys.setenv(
    https_proxy = stringr::str_glue("http://user-{creds$user_name}:{creds$password}@gate.smartproxy.com:7000")
  )

  base_url <- "https://www.zumper.com/apartments-for-rent/"
  cities <- c("vancouver-bc", "richmond-bc", "surrey-bc", "burnaby-bc")
  city_urls <- base_url %>%
    paste0(cities)

  all_links <- c()
  for (i in seq_along(city_urls)) {
    tryCatch(
      {
        page <- city_urls[i] %>%
          xml2::read_html()
        num_listings <- page %>%
          rvest::html_nodes(".css-1h2afvk") %>%
          rvest::html_text() %>%
          stringr::str_extract("[0-9]+ results") %>%
          readr::parse_number()
        num_pages <- 1:ceiling(num_listings / 46)
        individual_urls <- paste0(city_urls[i], "?page=", num_pages)

        for (j in seq_along(individual_urls)) {
          links <- individual_urls[j] %>%
            xml2::read_html() %>%
            rvest::html_nodes("[type='application/ld+json']") %>%
            rvest::html_text() %>%
            .[[1]] %>%
            rjson::fromJSON() %>%
            unlist() %>%
            .[stringr::str_detect(names(.), "url")] %>%
            unname() %>%
            unique()

          all_links <- c(all_links, links) %>%
            unique()

          # write urls out
          arrow::write_parquet(
            dplyr::tibble(
              urls = unique(all_links),
              date = Sys.Date()
            ),
            stringr::str_glue("data/urls/zumper/{Sys.Date()}.parquet")
          )
        }
      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      }
    )
  }
}


zumper_scraper <- function(urls) {
  final_df <- dplyr::tibble()
  for (i in seq_along(urls)) {
    tryCatch(
      {
        page <- xml2::read_html(urls[i])
        price <- page %>%
          rvest::html_nodes(".Header_price__3mK31") %>%
          rvest::html_text(trim = TRUE)

        address <- page %>%
          rvest::html_nodes(".Header_headerText__39zS4") %>%
          rvest::html_text(trim = TRUE)

        header <- page %>%
          rvest::html_nodes(".Header_fixedContent__ikv5z") %>%
          rvest::html_nodes("span") %>%
          rvest::html_text(trim = TRUE) %>%
          list() %>%
          dplyr::tibble(header = .)

        summary_content <- page %>%
          rvest::html_nodes(".SummaryIcon_summaryText__3eC0M") %>%
          rvest::html_text(trim = TRUE)

        summary_content <- summary_content %>%
          purrr::set_names(
            stringr::str_remove(., "[0-9]+") %>%
              stringr::str_squish()
          ) %>%
          dplyr::bind_rows() %>%
          janitor::clean_names()

        ammenities <- page %>%
          rvest::html_nodes(".css-1itwk24") %>%
          rvest::html_text(trim = TRUE) %>%
          list() %>%
          dplyr::tibble(amenities = .)

        see_more_vec <- page %>%
          rvest::html_nodes(".chakra-button") %>%
          rvest::html_text() %>%
          {
            which(. == "See more")
          }

        description_class <- page %>%
          rvest::html_nodes(".chakra-button") %>%
          .[see_more_vec] %>%
          rvest::html_attrs() %>%
          .[[1]] %>%
          .[names(.) == "class"] %>%
          unname() %>%
          stringr::str_split(" ") %>%
          .[[1]] %>%
          .[length(.)]

        remDr$navigate(urls[i])
        Sys.sleep(runif(1, 5, 6))

        description <- tryCatch(
          {
            webElem <- remDr$findElements(using = "class", value = description_class)
            webElem[[1]]$clickElement()
            Sys.sleep(runif(1, 1, 2))

            webElem <- remDr$findElements(using = "class", value = "css-1puvci5")
            description <- webElem[[1]]$getElementText() %>%
              stringr::str_replace_all("\n", " ") %>%
              stringr::str_replace_all("\\s+", " ") %>%
              dplyr::tibble(description = .)
          },
          error = function(e) {
            cat("ERROR in Selenium:", conditionMessage(e), "\n")
          }
        )

        final_df <- dplyr::bind_cols(
          ID = i,
          header,
          price = price,
          location = address,
          summary_content,
          ammenities,
          description,
          url = urls[i],
          date = Sys.Date()
        ) %>%
          dplyr::bind_rows(
            final_df, .
          )

        dplyr::bind_cols(
          ID = i,
          type = header,
          price = price,
          location = address,
          summary_content,
          ammenities,
          description,
          url = urls[i],
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
      stringr::str_glue("data/housing_data/zumper/{Sys.Date()}.parquet")
    )
  }

  container_name <- system('docker ps --format "{{.Names}}"', intern = TRUE)
  system(stringr::str_glue("docker stop {container_name}"))
}
