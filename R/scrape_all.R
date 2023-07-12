library(RSelenium)
library(tidyverse)
library(rvest)
library(config)
library(xml2)

list.files("R") %>%
  .[stringr::str_detect(., "scrape_all|realtor_scraper|create_final_df|analysis|add_lat_lng_script", negate = TRUE)] %>%
  here::here("R", .) %>%
  purrr::walk(
    ~ source(.)
  )
creds <- config <- config::get()

Sys.setenv(
  https_proxy = stringr::str_glue("http://user-{creds$user_name}:{creds$password}@gate.smartproxy.com:7000")
)

# ### liv.rent scraper ###
# get_liv_rent_links()
# new_links <- get_new_urls("data/urls/livrent")
# open docker
# system("open -a Docker")
# system("docker run -d -p 4445:4444 selenium/standalone-firefox:latest")
# remDr <- remoteDriver(port = 4445L)
# remDr$open()
# # run scraper
# get_livrent_data(new_links$urls)

### craigslist scraper ###
system("open -a Docker")
system("docker run -d -p 4445:4444 selenium/standalone-firefox:latest")
remDr <- remoteDriver(port = 4445L)
remDr$open()
get_craigslist_links()
new_links <- get_new_urls("data/urls/craigslist")
craigslist_scraper(new_links$urls)


### zumper scraper ###
get_zumper_urls()
new_links <- get_new_urls("data/urls/zumper")
system("open -a Docker")
system("docker run -d -p 4445:4444 selenium/standalone-firefox:latest")
remDr <- remoteDriver(port = 4445L)
remDr$open()
# run scraper
zumper_scraper(new_links$urls)

### rew scraper ###
get_urls_rew()
new_links <- get_new_urls("data/urls/rew") %>%
  dplyr::filter(stringr::str_detect(urls, "vancouver"))
rew_housing_scraper(new_links$urls)

### remax scraper ###
get_urls_remmax(30)
new_links <- get_new_urls("data/urls/remax")
remax_scraper(new_links$urls)

### point2home scraper ###
get_urls_point2home()
new_links <- get_new_urls("data/urls/point2home")
point2home_scraper(new_links$urls)


system("open -a Docker")
system("docker run -d -p 4445:4444 selenium/standalone-firefox:latest")
remDr <- remoteDriver(port = 4445L)
remDr$open()

urls_to_house_page <- c()
urls <- paste0("https://www.zolo.ca/vancouver-real-estate/page-", 1:60)
for (i in seq_along(urls)) {
  remDr$navigate(urls[i])
  Sys.sleep(runif(1, min = 2, max = 4))
  urls_to_house_page_temp <- remDr$findElements(using = "class", value = "card-listing--image-link")
  Sys.sleep(runif(1, min = 1, max = 2))
  urls_to_house_page <- c(urls_to_house_page, unlist(sapply(urls_to_house_page_temp, function(x) {
    x$getElementAttribute("href")
  })))
  Sys.sleep(runif(1, min = 2, max = 4))
  print(tail(urls_to_house_page))
}
arrow::write_parquet(
  dplyr::tibble(
    urls = urls_to_house_page,
    date = Sys.Date()
  ),
  here::here(stringr::str_glue("data/urls/zolo/{Sys.Date()}.parquet"))
)

zolo_urls <- dplyr::tibble(
  urls = urls_to_house_page,
  date = Sys.Date()
)[["urls"]][2]
new_links <- get_new_urls("data/urls/zolo") %>%
  dplyr::distinct()

df_zolo <- dplyr::tibble()
for (i in 1:nrow(new_links)) {
  tryCatch(
    {
      # navigate to homepage
      remDr$navigate(new_links$urls[i])
      Sys.sleep(runif(1, 2, 4))

      # beds, baths, and square feet
      bed_bath_sqft <- remDr$findElements(using = "class", value = "tile-data")
      bed_bath_sqft <- unlist(sapply(bed_bath_sqft, function(x) {
        x$getElementText()
      }))
      bed_bath_sqft <- bed_bath_sqft %>%
        purrr::set_names(stringi::stri_trim_both(stringr::str_remove(., "[0-9]+"))) %>%
        dplyr::bind_rows()

      # location
      location <- remDr$findElements(using = "class", value = "listing-location")
      location <- unlist(sapply(location, function(x) {
        x$getElementText()
      }))
      location <- location %>%
        stringr::str_remove("\n[0-9]+.*") %>%
        stringr::str_replace("\n", " ")

      # price
      price <- remDr$findElements(using = "class", value = "listing-price")
      price <- unlist(sapply(price, function(x) {
        x$getElementText()
      }))


      # details
      column_label <- remDr$findElements(using = "class", value = "column-label")
      column_label <- unlist(sapply(column_label, function(x) {
        x$getElementText()
      }))
      column_label <- column_label[nchar(column_label) > 0]

      column_value <- remDr$findElements(using = "class", value = "column-value")
      column_value <- unlist(sapply(column_value, function(x) {
        x$getElementText()
      }))
      column_value <- column_value[nchar(column_value) > 0]

      names(column_value) <- column_label
      column_value <- dplyr::bind_rows(column_value)

      temp_zolo <- dplyr::bind_cols(
        ID = i,
        price = price,
        column_value,
        location = location,
        bed_bath_sqft,
        date = Sys.Date()
      )
      print(temp_zolo)

      df_zolo <- df_zolo %>%
        dplyr::bind_rows(temp_zolo)
    },
    error = function(e) {
      cat("ERROR :", conditionMessage(e), "\n")
    }
  )

  arrow::write_parquet(
    df_zolo, here::here(stringr::str_glue("data/housing_data/zolo/{Sys.Date()}.parquet"))
  )
}
