#' A function to match vegetatino to the nearest animal sighting on the same day
#'
#' @param ws a data frame containing metadata (presumably on zebras and cattle) containing rows "Latitude" and "Longitude"
#' @param vs a data frame containing statistics (presumably on vegetation) containing rows "Latitude" and "Longitude"
#'
#' @returns data frame containing vs rows with corresponding ws rows
#' @export
#'

vs_ws_match <- function(ws, vs) {
  sub_df <- as.data.frame(matrix(nrow = nrow(vs), ncol = ncol(ws)))
  for (i in 1:nrow(vs)) {
    ws$Date <- as.Date(ws$Date, "%d/%m/%Y")
    vs$Date <- as.Date(vs$Date, "%d/%m/%Y")
    Date <- vs$Date[i]
    curr_ws <- dplyr::filter(ws, Date == vs$Date[i])
    min_dist <- Inf
    vs_lat <- vs$Latitude[i]
    vs_long <- vs$Longitude[i]
    min_row <- ws[1, ]
    for (j in 1:nrow(curr_ws)) {
      dist <- raster::pointDistance(c(vs_long, vs_lat), c(curr_ws$Longitude[j], curr_ws$Latitude[j]), lonlat = TRUE)
      if (dist < min_dist) {
        min_dist <- dist
        min_row <- curr_ws[j, ]
      }
    }
    sub_df[i, ] <- min_row
  }
  df <- as.data.frame(cbind(vs, sub_df))
  colnames(df) <- c(colnames(vs), colnames(ws))
  df
}
