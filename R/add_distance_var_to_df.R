add_distance_var_to_df <- function(new_var, homes_df, places_df, method = "min_dist", radius) {
  # ceate new variables
  homes_df[[new_var]] <- NA
  radius <- radius
  print(new_var)

  if (method == "min_dist") {
    for (i in 1:nrow(homes_df)) {
      if (i %% 100 == 0) {
        print(i)
      }
      dist1 <- geosphere::distVincentyEllipsoid(
        p1 = homes_df[i, c("lng", "lat")],
        p2 = places_df[, c("lng", "lat")]
      ) %>%
        as.vector() %>%
        min()
      homes_df[[new_var]][i] <<- dist1
    }
  } else {
    for (i in 1:nrow(homes_df)) {
      if (i %% 100 == 0) {
        print(i)
      }
      length <- geosphere::distVincentyEllipsoid(
        p1 = homes_df[i, c("lng", "lat")],
        p2 = places_df[, c("lng", "lat")]
      ) %>%
        as.vector() %>%
        .[. < radius] %>%
        length()
      homes_df[[new_var]][i] <<- length
    }
  }
}
