######################################################################################
##############################image-intersections.R###################################
######################################################################################

####User defined functions####
test_intersection <- function(a,b,c){
  out <- tryCatch(
    {
      message("Double checking layer intersection")
      #length(crop(a,b))>0;
      #mask(a,b);
      ndvi1 <- crop(a,b);
      mask(ndvi1,c);
      return(TRUE);
    },
    error=function(cond) {
      message(paste("Shapefile doesn't intersect ndvi raster"))
      # Choose a return value in case of error
      return(FALSE)
    },
    finally={
      
    }
  )    
  return(out)
};
print("test_intersection - successfully loaded!")
md5post <- function() {
  GeoJSON.Files <- list.files(paste0(wd,"/DataOut"), recursive = TRUE, pattern = ".geojson", full.names = TRUE);
  GeoJSON.Files.MD5 <- md5sum(files = GeoJSON.Files);
  MD5.check2 <- data.frame(filename = GeoJSON.Files,
                           MD5Checksum2 = GeoJSON.Files.MD5,
                           stringsAsFactors = FALSE);
  MD5.check2$property2 <- gsub("_GDM_AllAvailableDates.geojson","",sapply(strsplit(MD5.check2$filename, split = "/"),"[",7));
  row.names(MD5.check2) <- 1:length(MD5.check2$filename)
  rm(GeoJSON.Files,GeoJSON.Files.MD5); 
  gc()
  MD5.check3 <- base::merge(MD5.check1,MD5.check2, by = "filename", all.y = TRUE )
  MD5.check4 <- MD5.check3[MD5.check3$MD5Checksum!=MD5.check3$MD5Checksum2,]
  
  if (length(MD5.check4$filename) >= 1) {
    
    message("Deploying master app");
    master.app.acct <- get("master.app.acct", envir = .GlobalEnv);
    master.app <- get("master.app", envir = .GlobalEnv);
    master.app2 <- sub("[$/]","",master.app)
    avail.app.accts <- get("avail.app.accts", envir = .GlobalEnv);
    message(paste("Deploying master app:",master.app2));
    rsconnect::deployApp(paste0(wd,"/ShinyApps",master.app),account = master.app.acct, forceUpdate = getOption("rsconnect.force.update.apps", TRUE));
    
    
    MD5.check4$AppFile <- propmeta$AppFile[propmeta$Property  %in%  MD5.check4$property2]
    MD5.check4$AppFile <- gsub("/www/DataOut","",MD5.check4$AppFile);
    MD5.check4$AppFile2nd <- propmeta$AppFile2nd[propmeta$Property  %in%  MD5.check4$property2]
    MD5.check4$AppFile2nd <- gsub("/www/DataOut","",MD5.check4$AppFile2nd);
    Apps2GoUp <- c(MD5.check4$AppFile,MD5.check4$AppFile2nd)
    Apps2GoUp <- Apps2GoUp[!duplicated(Apps2GoUp)]
    Apps2GoUp <- Apps2GoUp[!grepl(paste0(master.app2),Apps2GoUp)];
    av.shiny.apps <- av.shiny.apps[av.shiny.apps$AppFile  %in%  Apps2GoUp,]
    for (l in seq_along(av.shiny.apps$AppFile)){
      message(paste("Deploying app", (l+1), "of", length(av.shiny.apps$AppFile)+1));
      Sys.sleep(1);
      rsconnect::deployApp(paste0(wd,av.shiny.apps$AppFile[l]),account = av.shiny.apps$ShinyAppAccnt[l], forceUpdate = getOption("rsconnect.force.update.apps", TRUE));
    };# END l loop
    rm(MD5.check1,MD5.check2,MD5.check3,MD5.check4,avail.app.accts,av.shiny.apps,av.shiny.apps2, master.app, master.app.acct, master.app2)
    gc()
  } else {
    message("No Shiny Apps Required Updating")
    Sys.sleep(3)
  }
}

####END SCRIPT####