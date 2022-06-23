######################################################################################
#####################control-setup-4-post-processing.R################################
######################################################################################

####User Defined Functions####

print("Loading user defined functions...");

####Execute Processing####

if (Interactive) {
  
  base::message(paste0("Checking for shapefiles in: ",d.dir,"/shapefiles"));
  shps <- check4shapefiles(dd = d.dir);
  base::message(paste0("Checking for Treemasks in: ",d.dir,"/treemasks"));
  tm2create <- check4treemasks(dd = d.dir,ss = s.dir,t = tiles);
  if (is.null(tm2create)) {
    base::message("No new treemasks required - moving on...")
  } else {
    tm2create <- select.treemasks(tm = tm2create)
    lapply(tm2create,create.treemask)
  }
  shp.mods <- unlist(lapply(shps,list.shp.pms));
  shp.mods <- shp.mods[!duplicated(shp.mods)];
  un.av.mods <- chk4mods.unavail(mods = shp.mods, kill = FALSE);
  print(un.av.mods);
  build.new.mods();
  chk4mods.unavail(mods = shp.mods, kill = TRUE);
  load.propmeta();
  sapply(paste0(d.dir,"/",c("dataout","shinyapps","emailout")),create.dirs)
  MD5.check1 <- md5.check(dd = d.dir)
  UCRS <- load.UCRS()
  property.nam <- which.farms(dd = d.dir);
  if ('New Property' %in% property.nam) {
    property.nam <- process.new.farm(pm = propmeta)
    skip <- FALSE
    fast <- fast.fun()
  } else if ('All' %in% property.nam) {
    property.nam <- propmeta$Property
    skip <- skip.fun()
    fast <- fast.fun()
  } else {
    skip <- skip.fun()
    fast <- fast.fun()
  }
  print(paste("Skip =", skip, "Fast" = fast))
}

if (!Interactive) {
  shps <- check4shapefiles(dd = d.dir);
  md5 <- md5.check(dd = d.dir)
  UCRS <- load.UCRS()
  load.propmeta()
  property.nam <- propmeta$Property
  skip <- post.skip
  fast <- post.fast
}


####END SCRIPT####
