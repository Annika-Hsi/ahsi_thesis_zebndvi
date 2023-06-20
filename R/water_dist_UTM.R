#' A function to calculate distances between zebra sightings and the nearest water point
#'
#' @param df a data frame containing metadata (presumably for zebras and cattle) and containing the columns "Date," "GPS.x," "GPS.y," and "Distance.to.water"
#' @param water_loc a data frame of coordinates (Arc 1960) for water locations (presumably in Mpala)
#'
#' @return a data frame identical to df but with the Distance.to.water column filled in
#' @export
#'

water_dist_UTM <- function(df, water_loc){

  # go through each line of the raw whitesheets
  for (r_row in 1:nrow(df)) {

    ws_y <- df$GPS.y[r_row]
    ws_x <- df$GPS.x[r_row]

    min_dist <- Inf

    for (w_row in 1:nrow(water_loc)) {
      watersrc_y <- water_loc$GPS.y[w_row]
      watersrc_x <- water_loc$GPS.x[w_row]

      # calculate distance
      dist <- (ws_y - watersrc_y)^2 + (ws_x - watersrc_x)^2

      # find smallest distance
      if (dist < min_dist) {
        min_dist <- dist
      }
    }
    df$Distance.to.water[r_row] <- min_dist
  }
  df
}
