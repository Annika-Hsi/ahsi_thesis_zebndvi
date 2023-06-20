#' A function to generate the Wildbook metadata spreadsheet
#'
#' @param white a data frame containing metadata for zebras that can be used to fill out a Wildbook spreadsheet
#' @param PATH the path name telling you where the photos are (i.e., "/Users/annikahsi/Desktop/SUMMER 2022/photos/28.6.22N/group")
#' @param date the date in the format "dd/m/yyyy"
#'
#' @return the filled in wildbook data frame
#' @export

generate_wb <- function(white, PATH, date) {
  # read csv with only sorted photo numbers
  wildbook <- read.csv(paste0(PATH,"/WildbookSheet.csv"))

  # set columns that are the same for all sightings
  wildbook$Encounter.year <- 2022
  wildbook$Encounter.genus <- "Equus"
  wildbook$Encounter.livingStatus <- "alive"  # be sure to change this if you see a dead zebra which is rare

  # read master copy of raw white sheets
  white <- read.csv("~/Desktop/SUMMER 2022/wildbook processing/MasterRawWhitesheets2022.csv")
  # get rid of non-zebras
  white <- dplyr::filter(white,!is.na(`Pic.Start.72.`))

  # extract rows with relevant date from master raw white sheets
  white <- dplyr::filter(white, Date == date)
  nrow(white)

  photo_row <- 1
  for (photo_row in 1:nrow(wildbook)) {
    # photo number
    filename <- wildbook[photo_row,1]
    filenumber <- as.numeric(strsplit(strsplit(filename,"DSCN")[[1]][2],".JPG")[[1]])

    the.dazzle <- 0

    for (the.dazzle in 1:nrow(white)) {
      # if no photos taken of the dazzle
      if (white$Pic.Start.72.[the.dazzle] == 0) {
        next
      }
      # if the current photo is inside the row's photo range
      if (filenumber >= as.numeric(white$Pic.Start.72.[the.dazzle]) &
          filenumber <= as.numeric(white$Pic.End.72.[the.dazzle])) {
        break
      }
    }
    # copy row data once correct row is found
    wildbook$Encounter.decimalLatitude[photo_row] <- white$Latitude[the.dazzle]
    wildbook$Encounter.decimalLongitude[photo_row] <- white$Longitude[the.dazzle]

    # convert and copy the species
    if (white$Species[the.dazzle] == "GZ") {
      wildbook$Encounter.specificEpithet[photo_row] <- "grevyi"
    }
    else if (white$Species[the.dazzle] == "PZ") {
      wildbook$Encounter.specificEpithet[photo_row] <- "quagga"
    }

    # copy the location
    wildbook$Encounter.locationID[photo_row] <- white$Location[the.dazzle]
    wildbook$Encounter.verbatimLocality[photo_row] <- white$Loop[the.dazzle]

    # behavior
    if (white$Activity[the.dazzle] == "St") {
      wildbook$Encounter.behavior[photo_row] <- "Standing"
    }
    else if (white[the.dazzle, 20] == "Gr") {
      wildbook[photo_row, 17] <- "Grazing"
    }
    else if (white[the.dazzle, 20] == "Wa") {
      wildbook[photo_row, 17] <- "Walking"
    }
    else if (white[the.dazzle, 20] == "Dr") {
      wildbook[photo_row, 17] <- "Drinking"
    }
    else if (white[the.dazzle, 20] == "Re") {
      wildbook[photo_row, 17] <- "Resting"
    }

    # grass data
    wildbook[photo_row, 19:22] <- white[the.dazzle, 22:25]
    wildbook[photo_row, 18] <- paste0("<", white[the.dazzle, 21])
    if (white[the.dazzle, 22] == "B") {
      wildbook[photo_row,19] <- "brown"
    }
    else if (white[the.dazzle, 22] == "BG") {
      wildbook[photo_row,19] <- "brown with some green"
    }
    else if (white[the.dazzle, 22] == "GB") {
      wildbook[photo_row,19] <- "green with some brown"
    }
    else {
      wildbook[photo_row,19] <- "green"
    }
    if (white$`Bush.type`[the.dazzle] == "LB") {
      wildbook[photo_row, 23] <- "light"
    }
    else if (white$`Bush.type`[the.dazzle] == "OG") {
      wildbook[photo_row, 23] <- "open grassland"
    }
    else if (white$`Bush.type`[the.dazzle] == "MB") {
      wildbook[photo_row, 23] <- "medium"
    }
    else {
      wildbook[photo_row, 23] <- "thick"
    }

    # other species
    other_species <- strsplit(white$Other.species[the.dazzle], ", ")[[1]]
    if (length(other_species) > 0) {
      wildbook[photo_row, 25] <- other_species[1]
    }
    if (length(other_species) > 1) {
      wildbook[photo_row, 26] <- other_species[2]
    }
    if (length(other_species) > 2) {
      wildbook[photo_row, 27] <- other_species[3]
    }

    # weather
    rain <- white$Rain[the.dazzle]
    if (rain == "NR") {
      wildbook[photo_row, 28] <- "no rain"
    }
    else if (rain == "LR") {
      wildbook[photo_row, 28] <- "light"
    }
    else {
      wildbook[photo_row, 28] <- "heavy"
    }
    sun <- white$Sun[the.dazzle]
    if (sun == "FS") {
      wildbook[photo_row, 29] <- "full"
    }
    else if (sun == "PS") {
      wildbook[photo_row, 29] <- "part"
    }
    else {
      wildbook[photo_row, 29] <- "no sun"
    }
    wind <- white$Wind[the.dazzle]
    if (wind == "NW") {
      wildbook[photo_row, 30] <- "no wind"
    }
    else if (wind == "LW") {
      wildbook[photo_row, 30] <- "light"
    }
    else if (wind == "MW") {
      wildbook[photo_row, 30] <- "medium"
    }
    else {
      wildbook[photo_row, 30] <- "strong"
    }

    # number zebras
    wildbook$Occurrence.individualCount[photo_row] <- white$Total.zebras[the.dazzle]

    # convert and copy date and time
    date <- strsplit(white$Date[the.dazzle], "/")[[1]]
    wildbook$Encounter.day[photo_row] <- date[1]
    wildbook$Encounter.month[photo_row] <- date[2]
    time <- strsplit(as.character(white$Time[the.dazzle]), "")[[1]]
    if (length(time) == 4) {
      time <- c("0", time)
    }
    wildbook$Encounter.hour[photo_row] <- paste(time[1:2], collapse = "")
    wildbook$Encounter.minutes[photo_row] <- paste(time[4:5], collapse = "")

    # in case the sighting had both species you have to ID the animal in the photo by looking at it
    if (white[the.dazzle,"Multispecies"] == 1) {
      wildbook[photo_row,13] <- "" # Remove species designation
      wildbook$Occurrence.individualCount[photo_row] <- ""  # remove number of individuals
      cat("Warning: Species entry in `",filename,"` needs to be handled manually.\n",sep="")
    }

    # occurrence id
    species <- white$Species[the.dazzle]
    if (species == "GZ" | species == "PZ") {
      wildbook$Encounter.occurrenceID[photo_row] <- paste0(species, "_", date[1], "-", date[2], "-", date[3], "_", white$Whitesheet.Filename[the.dazzle], "-", white$Whitesheet.Entry.Number[the.dazzle])
    }
    if (white[the.dazzle, "Multispecies"] == 1) {
      wildbook$Encounter.occurrenceID[photo_row] <- paste0("GZPZ", "_", date[1], "-", date[2], "-", date[3], "_", white$Whitesheet.Filename[the.dazzle], "-", white$Whitesheet.Entry.Number[the.dazzle])
    }

  }

  wildbook$Encounter.submitterID <- "ahsi"
  wildbook$Encounter.submitter0.emailAddress <- "ahsi@princeton.edu"
  wildbook$Encounter.project0.researchProjectName <- "Cattle-Zebra Interactions (Princeton University)"

  # replace NAs with ""
  for (coln in colnames(wildbook)) {
    column <- wildbook[,coln]
    column[is.na(column)]<-""
    wildbook[,coln] <- column
  }

  wildbook
}
