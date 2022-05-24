######################################################################################
######################################setup.R#########################################
######################################################################################

####User Defined Functions####
check4pw <- function(usr = "Username", ser = "Service"){
  klst <- key_list(service = ser)
  if (length(klst$service)==0) {
    con <- if (interactive()) stdin() else file('stdin');
    message(paste0("Please enter your password for ",ser,":"))
    psswrd <- scan(file=con, sep=',', nlines=1, what = 'character', quiet=TRUE);
    key_set_with_value(service = ser,username = usr ,password = psswrd);
  } else {
    klst <- klst[klst$username==usr,]
    if (length(klst$service)==0){
      con <- if (interactive()) stdin() else file('stdin');
      message(paste0("Please enter your password for ",ser,":"));
      psswrd <- scan(file=con, sep=',', nlines=1, what = 'character', quiet=TRUE);
      key_set_with_value(service = ser,username = usr ,password = psswrd);
    } else {
      psswrd <- key_get(service = ser, username = usr)
    }
  }
  return(psswrd)
};
message("check4pw - successfully loaded");
cld.connect <- function() {
  message(paste("Connecting to cloud drive:", cld.dir))
  pwd <- check4pw(usr = cld.usr, ser = cld.ser.nm)
  cmd1 <- paste0("net use ",drv.l,": \\\\",cld.dir," /user:",cld.usr," ",pwd)
  system(cmd1, wait=TRUE)
  d.dir <- paste0(drv.l,":")
};
message("cld.connect - successfully loaded");
cld.disconnect <- function() {
  message(paste("Disconnecting from cloud drive:", cld.dir))
  cmd2 <- paste0("net use ",d.dir," ", "/delete");
  system(cmd2, wait=TRUE)
};
message("cld.disconnect - successfully loaded");
create.dirs <- function(dir = "Data Directory"){
  dir.create(dir,showWarnings = FALSE, recursive = TRUE)
};
message("create.dirs - successfully loaded");
check4shapefiles <- function(dd = "Data Directory") {
  shp.dir <- (paste0(dd,"/shapefiles"))
  shp.names <- list.files(path = shp.dir, pattern = ".shp", ignore.case = TRUE, full.names = FALSE)
  req.cols <- c("PADD_NAME", "PAS_TYP")
  if (length(shp.names)==0) {
    warning("Warning: Preprocessing of available images completed but post processing won't occur...")
    stop(paste0("No Shapefiles available, please add at least one to: ", d.dir,"/shapefiles"))
  } else {
    shps <- list()
    for (i in seq_along(shp.names)){
      shp <- gsub(".shp","",shp.names[i],ignore.case = TRUE)
      boundary1 <- suppressWarnings(readOGR(shp.dir,shp));
      if (req.cols[1] %in% names(boundary1@data) & req.cols[2] %in% names(boundary1@data)) {
        shps <- c(shps,boundary1)
      } else {
        warning(paste0("Warning: Shapefile ", shp, " didn't have the required columns so was skipped..." ))
      }
    }
    if (length(shps)==0) {
      warning("Warning: Preprocessing of available images completed but post processing won't occur...")
      stop(paste0("No Shapefiles with correct columns available, please add at least one to: ", d.dir,"/shapefiles"))
    } else {
      return(shps)
    }
  }
};
message("check4shapefiles - successfully loaded");
check4treemasks <- function(dd = "Data Directory", t = "Tiles") {
  tm.dir <- paste0(dd,"/treemasks")
  tm.nms <- paste0(tm.dir,"/T",t,"_treemask_-vesRemoved.tif")
  avail.tms <- list.files(tm.dir,pattern = ".tif",full.names = TRUE)
  tm2create <- tm.nms[!tm.nms %in% avail.tms]
  tm.nms <- t[!tm.nms %in% avail.tms]
  dd2 <- rep(dd,length(tm.nms))
  tm2create <- list(tm2create,tm.nms,dd2)
  if (length(tm2create[[1]])>0) {return(tm2create)}
};
message("check4treemasks - successfully loaded");
select.treemasks <- function(tm = "Tree Masks to Create") {
  message(paste0("New treemask required for:T",tm[[2]],sep = "\n" ))
  tm1 <- c("All",paste0("T",tm[[2]]),"None")
  con <- if (interactive()) stdin() else file('stdin');
  message("Which new treemask would you like to process? Select All, None or one/multiple others separated by commas:");
  for (i in seq_along(tm1)){
    message(paste0(tm1[i]," [",i-1,"] ?"))
  }
  tm.no <- scan(file=con, sep=',', nlines=1, what = 'integer', quiet=TRUE);
  tm.no <- as.numeric(tm.no);
  tm.nam <- tm1[tm.no+1];
  rm(i,con);
  if ('None' %in% tm.nam) {
    message("Skipping treemask creation.  Please note shapefiles that lie on the identified tiles won't be processed")
    return(NULL)
  } else if ('All' %in% tm.nam) {
    message('Creating treemask for all identified tiles')
    tm2 <- tm[[1]]
    tm3 <- tm[[2]]
    tm4 <- tm[[3]]
    for (i in seq_along(tm2)){
      tm6 <- c(tm2[i],tm3[i],tm4[i])
      if (!exists("tm5")){
        tm5 <- list(tm6)
      } else {
        tm5 <- c(tm5,list(tm6))
      }
    }
    rm(tm2,tm3,tm4,tm6,i)
    return(tm5)
  } else {
    message(paste0("Creating treemask for:",tm.nam,sep = "\n"))
    tm2 <- tm[[1]][tm.no]
    tm3 <- tm[[2]][tm.no]
    tm4 <- tm[[3]][tm.no]
    for (i in seq_along(tm2)){
      tm6 <- c(tm2[i],tm3[i],tm4[i])
      if (!exists("tm5")){
        tm5 <- list(tm6)
      } else {
        tm5 <- c(tm5,list(tm6))
      }
    }
    rm(tm2,tm3,tm4,tm6,i)
    return(tm5)
  }
};
message("select.treemasks - successfully loaded");
create.treemask <- function(tm = " tm2create list") {
  print(paste0("Creating treemask for T",tm[2]))
  img.dir <- paste0(tm[3],"/sentinelimages/T",tm[2])
  img.dtes1 <- list.dirs(img.dir,recursive = FALSE, full.names = TRUE)
  if (length(img.dtes1)!=0) {
    img.dtes2 <- list.dirs(img.dir,recursive = FALSE, full.names = FALSE)
    img.dtes2 <- img.dtes2[sapply(img.dtes1,done.files)]
    img.dtes1 <- img.dtes1[sapply(img.dtes1,done.files)]
    con <- if (interactive()) stdin() else file('stdin');
    message("Which image would you like to use to create treemask? (Select one only):");
    for (i in seq_along(img.dtes2)){
      message(paste0(img.dtes2[i]," [",i,"] ?"))
    }
    im.no <- scan(file=con, sep=',', nlines=1, what = 'integer', quiet=TRUE);
    im.no <- as.numeric(im.no[1]);
    rm(i,con);
    print(paste("Importing:",img.dtes2[im.no]))
    img.dir <- paste0(img.dir,"/",img.dtes2[im.no]);
    img.name <- list.files(img.dir,pattern = "ndvi.tif",recursive = TRUE, full.names = TRUE);
    sent.tif.ndvi <- raster::stack(img.name);
    t1 <- now()
    print(paste(t1,"- Setting -ve image pixels to 0..."))
    sent.tif.ndvi[sent.tif.ndvi <= 0] <- NA;
    print(paste(now(),"- Done - run time =",ceiling(difftime(now(),t1,units = "sec")),"seconds"))
    t1 <- now()
    print(paste(t1,"- Determining OTSU Threshold..."))
    img.thr <- auto_thresh(abs(as.integer(getValues(sent.tif.ndvi)*1000000)), method = "Otsu", ignore_na = TRUE);
    print(paste(now(),"- Done it's",img.thr," - run time =",ceiling(difftime(now(),t1,units = "sec")),"seconds"))
    t1 <- now()
    print(paste(t1,"- Creating the mask..."))
    sent.tif.ndvi[(sent.tif.ndvi * 1000000) <= img.thr[1]] <- 0;
    sent.tif.ndvi[(sent.tif.ndvi * 1000000) > img.thr[1]] <- 255;
    print(paste(now(),"- Done - run time =",ceiling(difftime(now(),t1,units = "sec")),"seconds"))
    t1 <- now()
    print(paste(t1,"- Writing mask to file..."))
    writeRaster(x = sent.tif.ndvi,
                filename = paste0(tm[3],"/treemasks/T",tm[2],"_treemask_-vesRemoved.tif"),
                format = "GTiff", # save as a tif
                datatype='INT2S', # save as a INTEGER rather than a float
                overwrite = TRUE);
    print(paste(now(),"- Done - run time =",ceiling(difftime(now(),t1,units = "sec")),"seconds"))
    rm(sent.tif.ndvi)
    gc()
    } else {
    warning(paste0("No pre-processed imagery available for T",tm[2]," - no mask created"))
  }
  
};
message("create.treemask - successfully loaded")
####END SCRIPT####