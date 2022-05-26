######################################################################################
#####################control-setup-4-post-processing.R################################
######################################################################################

####User Defined Functions####

print("Loading user defined functions...");

source(file = "functions/cleanup-data-dirs.R");

####Execute Processing####

if (Interactive) {
  
  message(paste0("Checking for shapefiles in: ",d.dir,"/shapefiles"));
  shps <- check4shapefiles(dd = d.dir);
  message(paste0("Checking for Treemasks in: ",d.dir,"/treemasks"));
  tm2create <- check4treemasks(dd = d.dir,ss = s.dir,t = tiles);
  if (is.null(tm2create)) {
    message("No new treemasks required - moving on...")
  } else {
    tm2create <- select.treemasks(tm = tm2create)
    lapply(tm2create,create.treemask)
  }
  #load.pm();
  shp.mods <- unlist(lapply(shps,list.shp.pms));
  un.av.mods <- chk4mods.unavail(mods = shp.mods, kill = FALSE);
  print(un.av.mods);
  build.new.mods();
  chk4mods.unavail(mods = shp.mods, kill = TRUE);
  load.propmeta();
  sapply(paste0(d.dir,"\\",c("dataout","shinyapps","emailout")),create.dirs)
  md5 <- md5.check(dd = d.dir)
  UCRS <- load.UCRS()
  property.nam <- which.farms(dd = d.dir);
  if ('New Property' %in% property.nam) {
    property.nam <- process.new.farm(pm = propmeta)
  }
  shps <- check4shapefiles(dd = d.dir);
  

  
   
  
}

if (!Interactive) {}

####END SCRIPT####
