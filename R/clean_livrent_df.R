clean_liverent_df <- function(df) {
  df <- df %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      bedrooms = ifelse(is.na(bedrooms) & !is.na(bedroom), bedroom, bedrooms),
      bedrooms = ifelse(is.na(bedrooms) & !is.na(studio), 0, bedrooms),
      bathrooms = ifelse(is.na(bathrooms) & !is.na(bathroom), bathroom, bathrooms),
      bathrooms = ifelse(is.na(bathrooms) & !is.na(bath), bath, bathrooms),
      furnished = ifelse(is.na(furnished) & !is.na(unfurnished), unfurnished, furnished),
      furnished = ifelse(is.na(furnished) & !is.na(available), available, furnished)
    ) %>%
    dplyr::select(id:url) %>%
    dplyr::mutate_at(vars(price, bedrooms, bathrooms, ft2), ~ readr::parse_number(.)) %>%
    na.omit()
  return(df)
}
