######################################################################################
#####################control-setup-4-post-processing.R################################
######################################################################################

####User Defined Functions####

source(file = "functions/create-calib.R");

source(file = "functions/cleanup-data-dirs.R");

####Execute Processing####

if (Interactive){
  
  message(paste0("Checking for shapefiles in: ",d.dir,"/shapefiles"));
  shps <- check4shapefiles(dd = d.dir);
  message(paste0("Checking for Treemasks in: ",d.dir,"/treemasks"));
  tm2create <- check4treemasks(dd = d.dir,t = tiles);
  if (is.null(tm2create)) {
    message("No new treemasks required - moving on...")
  } else {
    tm2create <- select.treemasks(tm = tm2create)
    lapply(tm2create,create.treemask)
  }
}
