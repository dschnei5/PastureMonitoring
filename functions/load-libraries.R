######################################################################################
################################load-libraries.R######################################
######################################################################################

list.of.packages <- c("rgdal","lubridate","raster","sp","plyr","doParallel","geojsonio","beepr","gsubfn","stringi","tools", "keyring","jsonlite","autothresholdr");

####User defined functions####

loadlibraries <- function(pkg="List of Packages"){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  suppressPackageStartupMessages(sapply(pkg, require, character.only = TRUE))
  message(paste0(pkg,"-successfully loaded",sep = "\n"))
}

####END SCRIPT####
