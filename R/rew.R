get_urls_rew <- function() {
  city <- c("vancouver", "surrey", "burnaby", "richmond")
  urls_city <- stringr::str_glue("https://www.rew.ca/properties/areas/{city}-bc")
  urls <- c()
  for (i in seq_along(urls_city)) {
    print(urls_city[[i]])
    # read in url of individual city
    tryCatch(
      {
        page <- xml2::read_html(urls_city[i])
      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      }
    )

    Sys.sleep(runif(1, 2, 4))

    # get all urls on first page
    urls <- page %>%
      rvest::html_nodes(".displaypanel-body") %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      .[stringr::str_detect(., "(developments/|properties/[0-9]+)")] %>%
      unique() %>%
      c(urls, .)

    # get pages of city
    max_pages <- page %>%
      rvest::html_nodes(".paginator-page") %>%
      rvest::html_text() %>%
      .[length(.)] %>%
      readr::parse_number()

    all_urls <- urls_city[i] %>%
      paste0("/page/", 2:max_pages)

    for (j in seq_along(all_urls)) {
      print(j)
      tryCatch(
        {
          page <- xml2::read_html(all_urls[j])
        },
        error = function(e) {
          cat("ERROR :", conditionMessage(e), "\n")
        }
      )

      Sys.sleep(runif(1, 2, 4))

      urls <- page %>%
        rvest::html_nodes(".displaypanel-body") %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href") %>%
        .[stringr::str_detect(., "(developments/|properties/[0-9]+)")] %>%
        unique() %>%
        c(urls, .)

      # write urls out
      arrow::write_parquet(
        dplyr::tibble(
          urls = paste0("https://www.rew.ca", unique(urls)),
          date = Sys.Date()
        ),
        stringr::str_glue("data/urls/rew/{Sys.Date()}.parquet")
      )
    }
  }
}


rew_housing_scraper <- function(urls_rew) {
  df <- dplyr::tibble()
  for (i in 1:length(urls_rew)) {
    tryCatch(
      {
        page <- xml2::read_html(urls_rew[i])
        Sys.sleep(runif(1, 6, 7))

        descripion <- page %>%
          rvest::html_nodes(".listingoverview") %>%
          rvest::html_nodes("section") %>%
          rvest::html_text() %>%
          .[length(.)]

        address <- page %>%
          rvest::html_nodes(".listingheader-address-wrapper") %>%
          rvest::html_text() %>%
          stringr::str_remove_all("\n")

        square_feet <- page %>%
          rvest::html_nodes(".listingheader-details") %>%
          rvest::html_nodes("li") %>%
          rvest::html_text() %>%
          .[stringr::str_detect(., "Sqft")]

        square_feet <- page %>%
          rvest::html_nodes(".listingheader-details") %>%
          rvest::html_nodes("li") %>%
          rvest::html_text() %>%
          .[stringr::str_detect(., "Sqft")]

        df <- df %>%
          dplyr::bind_rows(
            page %>%
              rvest::html_nodes(".sectionblock") %>%
              rvest::html_nodes(".lineddisplay") %>%
              rvest::html_nodes("section") %>%
              rvest::html_text2() %>%
              stringr::str_split("\n") %>%
              {
                dplyr::tibble(
                  col_names = purrr::map_chr(., `[`, 1),
                  values = purrr::map_chr(., `[`, 2)
                )
              } %>%
              tidyr::pivot_wider(names_from = col_names, values_from = values) %>%
              dplyr::bind_cols(
                ID = i,
                descripion = descripion,
                square_feet = square_feet,
                address = address,
                date = Sys.Date(),
                url = urls_rew[i]
              )
          )


        arrow::write_parquet(
          df,
          stringr::str_glue("data/housing_data/rew/{Sys.Date()}.parquet")
        )
        print(df[nrow(df), ] %>% dplyr::select(ID, address, square_feet, dplyr::everything()))
      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      }
    )
  }
}
