#' A function to calculate vegetation statistics/metrics from raw vegetation sheets. Equations taken from Daniel Rubenstein.
#'
#' @param raw_vegetation a data frame containing data from vegetation transects
#' @param stats_df an empty header data frame to be filled out (look for raw_vegetation_header.csv)
#'
#' @return filled out stats_df
#' @export
#'

veg_stats <- function(raw_vegetation, stats_df) {

  # convert rows that need type change
  str(raw_vegetation)
  class(raw_vegetation$`Max height`) <- "double"
  class(raw_vegetation$`Forb height`) <- "double"
  class(stats_df$Loop) <- "character"
  class(stats_df$`GPSx`) <- "double"
  class(stats_df$`GPSy`) <- "double"
  class(stats_df$`Longitude`) <- "double"
  class(stats_df$`Latitude`) <- "double"
  class(stats_df$`Metadata file`) <- "double"
  class(stats_df$`% Cover`) <- "double"
  class(stats_df$`% Leaf`) <- "double"
  class(stats_df$`% Green`) <- "double"
  class(stats_df$`% Seeds`) <- "double"
  class(stats_df$`% Forbs`) <- "double"
  class(stats_df$`Average Height`) <- "double"
  class(stats_df$`SD Height`) <- "double"
  class(stats_df$`CV Height`) <- "double"
  class(stats_df$`Average Hits/Pin`) <- "double"
  class(stats_df$`Average Leaves/Pin`) <- "double"
  class(stats_df$`Sp. Diversity`) <- "double"

  raw_row <- 1

  stats_row <- 1

  # remove all rows where there were 2 species of grass
  for (r in 2:nrow(raw_vegetation)) {
    if (is.na(raw_vegetation$Date[r])) {
      break
    }
    if (raw_vegetation$`Pin Number`[r] == raw_vegetation$`Pin Number`[r - 1]) {
      raw_vegetation <- raw_vegetation[-c(r),]
    }
  }

  while(raw_row < nrow(raw_vegetation)) {
    file_name <- raw_vegetation$`Filename`[raw_row]
    if (is.na(file_name)) {
      break
    }

    df <- dplyr::filter(raw_vegetation, `Filename` == toString(file_name))
    df_sub <- dplyr::select(df, 12:17, 19:21)
    df_sub[is.na(df_sub)] <- 0

    stats_df[stats_row, 8] <- df[1, 8]

    brnst <- dplyr::filter(dplyr::select(df_sub, "Brown stem"), `Brown stem` != 0)
    sum_brnst <- 0
    count_brnst <- nrow(brnst)
    if (count_brnst != 0) {
      sum_brnst <- sum(brnst)
    }

    grnst <- dplyr::filter(dplyr::select(df_sub, "Green stem"), filter(`Green stem` != 0))
    sum_grnst <- 0
    count_grnst <- nrow(grnst)
    if (count_grnst != 0) {
      sum_grnst <- sum(grnst)
    }

    brnlf <- dplyr::filter(dplyr::select(df_sub, "Brown leaf"), `Brown leaf` != 0)
    sum_brnlf <- 0
    count_brnlf <- nrow(brnlf)
    if (count_brnlf != 0) {
      sum_brnlf <- sum(brnlf)
    }

    grnlf <- dplyr::filter(dplyr::select(df_sub, "Green leaf"), `Green leaf` != 0)
    sum_grnlf <- 0
    count_grnlf <- nrow(grnlf)
    if (count_grnlf != 0) {
      sum_grnlf <- sum(grnlf)
    }

    seed <- dplyr::filter(dplyr::select(df_sub, "Seed"), `Seed` != 0)
    count_seed <- nrow(seed)
    sum_seed <- 0
    if (count_seed != 0) {
      sum_seed <- sum(seed)
    }

    brnfrb <- dplyr::filter(dplyr::select(df_sub, "Brown forb"), `Brown forb` != 0)
    count_brnfrb <- nrow(brnfrb)
    sum_brnfrb <- 0
    if (count_brnfrb != 0) {
      sum_brnfrb <- sum(brnfrb)
    }

    grnfrb <- dplyr::filter(dplyr::select(df_sub, "Green forb"), `Green forb` != 0)
    sum_grnfrb <- 0
    count_grnfrb <- nrow(grnfrb)
    if (count_grnfrb != 0) {
      sum_grnfrb <- sum(grnfrb)
    }

    height <- dplyr::filter(dplyr::select(df_sub, "Max height"), `Max height` != 0)
    sum_height <- 0
    count_height <- nrow(height)
    if (count_height != 0) {
      sum_height <- sum(height)
    }

    hits_pin <-
      rename(
        as.data.frame(
        rowSums(
        dplyr::select(as.data.frame(df_sub), 1:5, 7:8))), 'sums'= ".")
    num_zeros <- nrow(
      dplyr::filter(hits_pin, `sums` == 0))
    sum_hits_pin <- sum(hits_pin)
    count_hits_pin <- nrow(hits_pin)

    sum_parts <- sum(sum_brnfrb, sum_brnlf, sum_brnst, sum_grnfrb, sum_grnlf, sum_grnst, sum_seed)
    count_parts <- sum(count_brnfrb, count_brnlf, count_brnst, count_grnfrb, count_grnlf, count_grnst, count_seed)

    sum_grass <- sum(sum_brnlf, sum_grnlf, sum_brnst, sum_grnst, sum_seed)
    count_grass <- sum(hits_pin$count_brnlf, hits_pin$count_grnlf, hits_pin$count_brnst, hits_pin$count_grnst, hits_pin$count_seed)

    sum_green <- sum(sum_grnlf, sum_grnst, sum_grnfrb)
    sum_leaf <- sum(sum_grnlf, sum_brnlf)

    # STATS
    stats_df$`% Cover`[stats_row] <- (1 - (num_zeros/count_hits_pin))
    stats_df$`% Leaf`[stats_row] <- sum_leaf/sum_grass
    stats_df$`% Green`[stats_row] <- sum_green/sum_parts
    stats_df$`% Seeds`[stats_row] <- sum_seed/sum_grass
    stats_df$`% Forbs`[stats_row] <- (count_brnfrb + count_grnfrb)/sum_parts

    # create new 'Max height' vector without zeros in it
    i_original <- 1
    i_new <- 1
    height_vector <- c()
    while (i_original <= nrow(df_sub)) {
      curr <- df_sub$`Max height`[i_original]
      if (curr != 0) {
        height_vector[i_new] <- curr
        i_new <- i_new + 1
      }
      i_original <- i_original + 1
    }

    stats_df$`Average Height`[stats_row] <- mean(height_vector)
    stats_df$`SD Height`[stats_row] <- sd(height_vector)
    stats_df$`CV Height`[stats_row] <- sd(height_vector)/mean(height_vector)
    stats_df$`Average Hits/Pin`[stats_row] <- sum_hits_pin/(count_hits_pin - num_zeros)
    stats_df$`Average Leaves/Pin`[stats_row] <- sum_leaf/(count_brnlf + count_grnlf)

    stats_df[stats_row, 1:2] <- df[1, 1:2]
    stats_df[stats_row, 3] <- df[1, 9]
    stats_df[stats_row, 4:7] <- df[1, 3:6]

    # calculate species diversity

    # find number of pins that had grass
    k <- 1
    num_grass <- 0
    while (k <= nrow (df)) {
      if (!is.na(df$`Grass species`[k])) {
        num_grass <- num_grass + 1
      }
      k <- k + 1
    }

    # get each unique grass species
    # cut off at 10 species
    species <- unique(na.omit(df$`Grass species`))
    if (length(species) > 10) {
      species <- species[1:10]
    }

    # count the number of occurrences of each grass species
    i <- 1
    grass_occurrences <- c()
    grass_row <- 1
    while (i <= length(species)) {
      j <- 1
      num_occurrences <- 0
      curr_species <- species[i]
      if (!is.na(curr_species)) {
        while (j <= nrow(df)) {
          s <- df$`Grass species`[j]
          if (!is.na(s)) {
            if (s == curr_species) {
              num_occurrences <- num_occurrences + 1
            }
          }
          j <- j + 1
        }
        grass_occurrences[grass_row] <- num_occurrences
        grass_row <- grass_row + 1
      }
      i <- i + 1
    }

    # calculate the percent of plants made up of each grass
    # then calculate Shannon's diversity index
    x <- 1
    shannon_sum <- 0
    while (x <= length(grass_occurrences)) {
      percent <- grass_occurrences[x]/num_grass
      shannon_sum <- shannon_sum + (-1*(percent*log10(percent)))
      x <- x + 1
    }

    stats_df$`Sp. Diversity`[stats_row] <- shannon_sum

    raw_row <- raw_row + nrow(df)
    stats_row <- stats_row + 1
  }

  stats_df
}
