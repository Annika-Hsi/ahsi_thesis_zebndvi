#' An NDVI-coordinate matching function
#'
#' @param start_date a vector of dates in the form YYYY-MM-DD that correspond to Sentinel tiff files
#' @param df a data frame that has columns "Date," "Latitude," and "Longitude"
#' @param tiff_path_a  string containing the first half of the path name for the tiff file before the date
#' @param tiff_path_b a string containing the second half of the path name for the tiff file after the date
#'
#' @returns a vector of NDVIs matched to the coordinates and dates given as input
#' @export
#'

ndvi_match <- function(start_date, df, tiff_path_a, tiff_path_b) {
  # create vector to store selected NDVIs
  df_NDVIs <- c()

  # row in stats data frame
  row <- 1

  # loop through each row in the stats data frame
  for (row in 1:nrow(df)) {

    # find proper sentinel data
    date <- df$Date[row]
    for (interval_start in 1:length(start_date)) {
      if (lubridate::interval(as.Date(date, format = '%d/%m/%Y'),
                   as.Date(start_date[interval_start])) >= 0) {
        break
      }
    }

    # save start date and file name of tiff that correspond to date interval
    if (interval_start == length(start_date)) {
      sentinel_start <- start_date[interval_start]
    }
    else {
      sentinel_start <- start_date[interval_start - 1]
    }
    day <- paste0(strsplit(sentinel_start, "-")[[1]][2], "-", strsplit(sentinel_start, "-")[[1]][3])
    file_name <- paste0(tiff_path_a, day, tiff_path_b)

    # load sentinel data from tiff
    NDVI <- raster::raster(x = file_name)
    NDVI <- raster::flip(NDVI, direction = 'y')

    # convert to data frame
    NDVI_df <- raster::as.data.frame(NDVI, xy = TRUE)

    # average pixel width and height of current tiff
    tiff_lats <- unique(NDVI_df$y)
    tiff_longs <- unique(NDVI_df$x)
    l_lats <- c()
    l_longs <- c()

    for (i in 1:(length(tiff_lats) - 1)) {
      l_lats[i] <- abs(tiff_lats[i + 1] - tiff_lats[i])
    }
    for (j in 1:(length(tiff_longs) - 1)) {
      l_longs[j] <- abs(tiff_longs[j + 1] - tiff_longs[j])
    }

    avg_l_lat <- mean(l_lats)
    avg_l_long <- mean(l_longs)

    # get origin coordinates
    origin_lat <- tiff_lats[length(tiff_lats)]
    origin_long <- tiff_longs[1]

    # get lat and long of current transect
    df_lat <- df$Latitude[row]
    df_long <- df$Longitude[row]

    # use formula N = kl + c and calculate k
    k_n <- round((df_lat - origin_lat)/avg_l_lat)
    k_e <- round((df_long - origin_long)/avg_l_long)
    m <- strsplit(day, "-")[[1]][1]
    d <- strsplit(day, "-")[[1]][2]
    tiff_ndvis <- c(NDVI_df %>% dplyr::select(all_of("ndvi")))
    df_NDVIs[row] <- tiff_ndvis[[1]][400*k_n + k_e]
  }
  df_NDVIs
}
