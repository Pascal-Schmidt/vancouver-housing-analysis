get_new_urls <- function(path) {
  result <- arrow::open_dataset(here::here(path)) %>%
    dplyr::group_by(urls) %>%
    dplyr::summarise(
      n = n(),
      date = min(date)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n == 1, date == Sys.Date()) %>%
    dplyr::collect()
  return(result)
}
