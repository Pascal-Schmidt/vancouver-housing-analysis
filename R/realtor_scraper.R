# remDr$navigate("https://www.realtor.ca/bc/vancouver/real-estate")
# remDr$screenshot(file = 'test.png')
# f <- remDr$findElements(using='class',value="geetest_radar_tip")[[1]]$clickElement()
# webElem <- remDr$findElements(using = "class", value = "paginationTotalPagesNum")
# num_pages <- webElem[[length(webElem)]]$getElementText()[[1]] %>%
#   readr::parse_number()
#
# urls <- list()
# x <- remDr$findElements(using = "class", value = "listingDetailsLink")
# urls[[1]] <- x %>%
#   purrr::map(
#     ~ .x$getElementAttribute("href")
#   )
#
# for (i in 2:50) {
#   webElem <- remDr$findElements(using = "class", value = "lnkNextResultsPage")
#   Sys.sleep(1)
#   webElem[[3]]$clickElement()
#   Sys.sleep(3)
#   x <- remDr$findElements(using = "class", value = "listingDetailsLink")
#   Sys.sleep(2)
#   urls[[i]] <- x %>%
#     purrr::map(
#       ~ .x$getElementAttribute("href")
#     )
#   Sys.sleep(2)
# }
#
# urls2 <- unlist(urls)
# df <- dplyr::tibble()
# for (i in seq_along(urls2)) {
#   page <- xml2::read_html(urls2[i])
#   Sys.sleep(2)
#   price <- page %>%
#     rvest::html_nodes("#listingPrice") %>%
#     rvest::html_text(trim = TRUE)
#
#   address <- page %>%
#     rvest::html_nodes("#listingAddress") %>%
#     rvest::html_text()
#
#   bed <- page %>%
#     rvest::html_nodes("#BedroomIcon") %>%
#     rvest::html_text(trim = TRUE) %>%
#     readr::parse_number()
#
#   bath <- page %>%
#     rvest::html_nodes("#BathroomIcon") %>%
#     rvest::html_text(trim = TRUE) %>%
#     readr::parse_number()
#
#   description <- page %>%
#     rvest::html_nodes("#propertyDescriptionCon") %>%
#     rvest::html_text(trim = TRUE)
#
#   labels_summary <- page %>%
#     rvest::html_nodes("#PropertySummary") %>%
#     rvest::html_nodes(".propertyDetailsSectionContentLabel") %>%
#     rvest::html_text()
#
#   values_summary <- page %>%
#     rvest::html_nodes("#PropertySummary") %>%
#     rvest::html_nodes(".propertyDetailsSectionContentValue") %>%
#     rvest::html_text()
#
#   summary <- values_summary %>%
#     purrr::set_names(labels_summary) %>%
#     dplyr::bind_rows()
#
#   labels_features <- page %>%
#     rvest::html_nodes("#propertyDetailsBuilding_BuildingFeatures") %>%
#     rvest::html_nodes(".propertyDetailsSectionContentLabel") %>%
#     rvest::html_text(trim = TRUE)
#
#   values_features <- page %>%
#     rvest::html_nodes("#propertyDetailsBuilding_BuildingFeatures") %>%
#     rvest::html_nodes(".propertyDetailsSectionContentValue") %>%
#     rvest::html_text(trim = TRUE)
#
#   features <- values_features %>%
#     purrr::set_names(labels_features) %>%
#     dplyr::bind_rows()
#
#   df <- df %>%
#     dplyr::bind_rows(
#       dplyr::tibble(
#         ID = i,
#         price = price,
#         address = address,
#         bed = bed,
#         bath = bath,
#         description = description
#       ) %>%
#         dplyr::bind_cols(
#           summary, features
#         )
#     )
#
#   dplyr::tibble(
#     ID = i,
#     price = price,
#     address = address,
#     bed = bed,
#     bath = bath,
#     description = description,
#     url = urls2[i]
#   ) %>%
#     dplyr::bind_cols(
#       summary, features
#     ) %>% print()
#
#   arrow::write_parquet(
#     df,
#     stringr::str_glue("data/housing_data/realtor/{Sys.Date()}.parquet")
#   )
#
# }
