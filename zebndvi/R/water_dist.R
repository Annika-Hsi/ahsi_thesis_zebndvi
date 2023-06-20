#' A function to calculate the distance between given coordinates and water locations at Mpala
#'
#' @param df a data frame containing metadata (presumably for zebras and cattle) and containing the columns "Date," "Latitude," "Longitude," and "Distance.to.water"
#' @param water_loc a data frame of coordinates (Latitude/Longitude) for water locations (presumably in Mpala)
#'
#' @returns a data frame identical to df but with the Distance.to.water column filled in
#' @export
#'
water_dist <- function(df, water_loc){

  # go through each line of the raw whitesheets
  for (r_row in 1:nrow(df)) {
    #if (df$Date[r_row] == "") {
      #break
    #}

    ws_lat <- df$Latitude[r_row]
    ws_long <- df$Longitude[r_row]

    min_dist <- Inf

    for (w_row in 1:nrow(water_loc)) {
      watersrc_lat <- water_loc$Latitude[w_row]
      watersrc_long <- water_loc$Longitude[w_row]

      # calculate distance
      dist <- (ws_lat - watersrc_lat)^2 + (ws_long - watersrc_long)^2

      # find smallest distance
      if (dist < min_dist) {
        min_dist <- dist
      }
    }
    df$Distance.to.water[r_row] <- min_dist
  }
  df
}
