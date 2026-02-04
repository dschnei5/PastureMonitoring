######################################################################################
##############################cleanup-data-dirs.R#####################################
######################################################################################

####User Defined Functions####

cleanup.csv.from.www <- function() {
  # Remove CSV files from www/DataOut folders (no longer needed, app uses GeoJSON)
  base::message("Cleaning up CSV files from ShinyApps www/DataOut folders...")
  
  # Find all ShinyApps directories
  shiny.dirs <- list.dirs(paste0(d.dir,"/ShinyApps"), recursive = FALSE, full.names = TRUE)
  
  for (shiny.dir in shiny.dirs) {
    www.dataout <- paste0(shiny.dir, "/www/DataOut")
    if (dir.exists(www.dataout)) {
      # Find all CSV files recursively in www/DataOut
      csv.files <- list.files(www.dataout, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
      if (length(csv.files) > 0) {
        base::message(paste("Removing", length(csv.files), "CSV files from", www.dataout))
        file.remove(csv.files)
      }
    }
  }
  
  base::message("CSV cleanup complete")
}
print("cleanup.csv.from.www - successfully loaded");