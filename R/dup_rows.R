#' A row duplicating function to weight data (for statistics and maps)
#'
#' @param df a data frame containing metadata (presumably for zebras and cattle) and containing the column "Total.zebras"
#'
#' @returns a new data frame with duplicated rows
#' @export
#'
#' @examples
#'   dup_rows(ws)

# DUPLICATE ROWS TO WEIGHT DATA FOR STATISTICS AND MAP MAKING
# MUST HAVE COLUMN FOR SIGHTING SIZE
dup_rows <- function(df, col) {
  weighted_df <- data.frame(matrix(nrow = sum(df[, col]), ncol = ncol(df)))
  weighted_row <- 1
  for (i in 1:nrow(df)) {
    curr_row <- df[i, ]
    reps <- curr_row[1, col]
    for (rep in 1:reps) {
      weighted_df[weighted_row, ] <- curr_row
      weighted_row <- weighted_row + 1
    }
  }
  weighted_df
}
