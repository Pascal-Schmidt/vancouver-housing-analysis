get_craigslist_links <- function() {
  base <- "https://vancouver.craigslist.org/search/apa"
  cities <- c("Vancouver", "Burnaby", "Surrey", "Richmond")
  urls_for_cities <- purrr::map2_chr(
    .x = cities, .y = base,
    ~ paste0(.y, "?query=", .x)
  )

  urls_to_house_vec <- c()
  for (i in seq_along(urls_for_cities)) {
    # get the total page number
    remDr$navigate(urls_for_cities[i])
    Sys.sleep(runif(1, min = 4, max = 6))
    page_num <- remDr$findElements(using = "class", value = "cl-page-number")[[1]]$getElementText()[[1]]
    Sys.sleep(runif(1, min = 1, max = 2))
    num_pages <- readr::parse_number(stringr::str_sub(page_num, start = -6))

    seq_num_pages <- floor(num_pages / 120) - 2

    # build all page urls for each city
    urls <- urls_for_cities[i] %>%
      paste0("#search=1~gallery~", 1:seq_num_pages, "~0")

    for (j in seq_along(urls)) {
      # get urls to individual housing data
      remDr$navigate(urls[j])
      Sys.sleep(runif(1, min = 4, max = 6))

      urls_to_house_page <- remDr$findElements(using = "class", value = "main")
      Sys.sleep(runif(1, min = 1, max = 2))
      urls_to_house_page <- unlist(sapply(urls_to_house_page, function(x) {
        x$getElementAttribute("href")
      }))
      Sys.sleep(runif(1, min = 2, max = 4))

      # remove duplicate urls
      ids <- urls_to_house_page %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(
          id = dplyr::row_number()
        ) %>%
        dplyr::distinct(value, .keep_all = TRUE) %>%
        dplyr::pull(id)
      urls_to_house_vec <- urls_to_house_page[ids] %>%
        c(urls_to_house_vec, .)

      length(unique(urls_to_house_vec)) %>% print()

      # write urls out
      arrow::write_parquet(
        dplyr::tibble(
          urls = unique(urls_to_house_vec),
          date = Sys.Date()
        ),
        here::here(stringr::str_glue("data/urls/craigslist/{Sys.Date()}.parquet"))
      )
    }
  }
}

# get_craigslist_links(urls_for_cities)


craigslist_scraper <- function(craigslist_urls) {
  final_df <- dplyr::tibble()
  for (i in seq_along(craigslist_urls)) {
    tryCatch(
      {
        url_individual <- craigslist_urls[i] %>%
          xml2::read_html()

        square_feet <- url_individual %>%
          rvest::html_elements(".attrgroup") %>%
          rvest::html_text() %>%
          stringr::str_extract("[0-9].*ft") %>%
          na.omit()

        ad_posted <- url_individual %>%
          rvest::html_elements("#display-date") %>%
          rvest::html_elements(".date.timeago") %>%
          rvest::html_text(trim = TRUE) %>%
          stringr::str_remove(" .*") %>%
          lubridate::ymd() %>%
          dplyr::tibble(
            date_posted = .
          )

        price <- url_individual %>%
          rvest::html_elements(".price") %>%
          rvest::html_text(trim = TRUE) %>%
          dplyr::tibble(
            price = .
          )

        housing_information <- url_individual %>%
          rvest::html_nodes("#ld_posting_data") %>%
          rvest::html_text(trim = TRUE) %>%
          tidyjson::spread_all() %>%
          dplyr::as_tibble()

        description <- url_individual %>%
          rvest::html_nodes("#postingbody") %>%
          rvest::html_text(trim = TRUE) %>%
          stringr::str_replace_all("\n", " ") %>%
          stringr::str_replace_all("\\s+", " ") %>%
          tolower() %>%
          dplyr::tibble(
            description = .
          )

        if (nrow(housing_information) == 0) {
          final_df <- final_df %>%
            dplyr::bind_rows(
              dplyr::bind_cols(
                ID = i,
                url = craigslist_urls[i]
              )
            )
        } else {
          final_df <- final_df %>%
            dplyr::bind_rows(
              dplyr::bind_cols(
                ID = i,
                ad_posted,
                square_feet = square_feet,
                price,
                housing_information,
                description,
                url = craigslist_urls[i],
                date = Sys.Date()
              ) %>%
                dplyr::mutate_all(~ as.character(.))
            )
        }

        dplyr::bind_cols(
          ID = i,
          ad_posted,
          square_feet,
          price,
          housing_information,
          description,
          url = craigslist_urls[i],
          date = Sys.Date()
        ) %>% print()
      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      }
    )

    arrow::write_parquet(final_df, here::here(stringr::str_glue("data/housing_data/craigslist/{Sys.Date()}.parquet")))
  }
}
