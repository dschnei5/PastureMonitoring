######################################################################################
################################load-libraries.R######################################
######################################################################################

list.of.packages <- c("rgdal","lubridate","raster","sp","plyr","doParallel","parallelly","geojsonio","beepr","gsubfn","stringi","tools", "keyring","jsonlite","autothresholdr","tryCatchLog","futile.logger", "gmailr","dplyr", "purrr", "readr");

####User defined functions####

loadlibraries <- function(pkg="List of Packages"){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  suppressPackageStartupMessages(sapply(pkg, require, character.only = TRUE))
  print(paste0(pkg,"-successfully loaded"))
}

####END SCRIPT####
