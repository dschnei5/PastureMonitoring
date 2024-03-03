######################################################################################
######################################setup.R#########################################
######################################################################################

####User Defined Functions####
{
  done.files <- function (x) {
    (any(grepl("ProcessingCompleted.inf",list.files(x))==TRUE)|any(grepl("ProcessingCompleted.txt",list.files(x))==TRUE))
  };
  print("done.files - successfully loaded");
  check4pw <- function(usr = "Username", ser = "Service") {
    klst <- key_list(service = ser)
    if (length(klst$service)==0) {
      con <- if (interactive()) stdin() else file('stdin');
      print(paste0("Please enter your password for ",ser,":"))
      psswrd <- scan(file=con, sep=',', nlines=1, what = 'character', quiet=TRUE);
      key_set_with_value(service = ser,username = usr ,password = psswrd);
    } else {
      klst <- klst[klst$username==usr,]
      if (length(klst$service)==0){
        con <- if (interactive()) stdin() else file('stdin');
        print(paste0("Please enter your password for ",ser,":"));
        psswrd <- scan(file=con, sep=',', nlines=1, what = 'character', quiet=TRUE);
        key_set_with_value(service = ser,username = usr ,password = psswrd);
      } else {
        psswrd <- key_get(service = ser, username = usr)
      }
    }
    return(psswrd)
  };
  print("check4pw - successfully loaded");
  cld.connect.d <- function() {
    print(paste("Connecting to cloud drive:", cld.dir))
    pwd <- check4pw(usr = cld.usr, ser = cld.ser.nm)
    cmd0 <- paste0("net use ",drv.l,": ","/delete")
    system(cmd0, wait=TRUE)
    cmd1 <- paste0("net use ",drv.l,": \\\\",cld.dir," /user:",cld.usr," ",pwd)
    system(cmd1, wait=TRUE)
    d.dir <- paste0(drv.l,":/")
  };
  print("cld.connect.d - successfully loaded");
  cld.connect.s <- function() {
    print(paste("Connecting to cloud drive:", cld.s.dir))
    pwd <- check4pw(usr = cld.s.usr, ser = cld.s.ser.nm)
    cmd0 <- paste0("net use ",drv.l.s,": ","/delete")
    system(cmd0, wait=TRUE)
    cmd1 <- paste0("net use ",drv.l.s,": \\\\",cld.s.dir," /user:",cld.s.usr," ",pwd)
    system(cmd1, wait=TRUE)
    s.dir <- paste0(drv.l.s,":/")
  };
  print("cld.connect.s - successfully loaded");
  cld.disconnect.d <- function() {
    print(paste("Disconnecting from cloud drive:", cld.dir))
    cmd2 <- paste0("net use ",sub("/","",d.dir)," ", "/delete");
    system(cmd2, wait=TRUE)
  };
  print("cld.disconnect.d - successfully loaded");
  cld.disconnect.s <- function() {
    print(paste("Disconnecting from cloud drive:", cld.s.dir))
    cmd2 <- paste0("net use ",sub("/","",s.dir)," ", "/delete");
    system(cmd2, wait=TRUE)
  };
  print("cld.disconnect.s - successfully loaded");
  create.dirs <- function(dir = "Data Directory") {
    dir.create(dir,showWarnings = FALSE, recursive = TRUE)
  };
  print("create.dirs - successfully loaded");
  check4shapefiles <- function(dd = "Data Directory") {
    shp.dir <- (paste0(dd,"/shapefiles"))
    shp.names <- list.files(path = shp.dir, pattern = ".shp", ignore.case = TRUE, full.names = FALSE)
    shp.names <- shp.names[!grepl("shp.",shp.names)]
    shp.rda.names <- list.files(path = shp.dir, pattern = ".rda", ignore.case = TRUE, full.names = FALSE)
    rda.names <- gsub(".rda","",shp.rda.names,ignore.case = TRUE)
    req.cols <- c("PADD_NAME", "PAS_TYP")
    if (length(shp.names)==0) {
      warning("Warning: Preprocessing of available images completed but post processing won't occur...")
      stop(paste0("No Shapefiles available, please add at least one to: ", d.dir,"/shapefiles"))
    } else {
      shps <- list()
      for (i in seq_along(shp.names)){
        shp <- gsub(".shp","",shp.names[i],ignore.case = TRUE)
        if (shp %in% rda.names) {
          shp.rda <- shp.rda.names[grepl(paste0(shp,".rda"),shp.rda.names)]
          fn <- paste0(shp.dir,"//",shp.rda)
          load(fn)
          shps <- c(shps,get(gsub(".rda","",shp.rda,ignore.case = TRUE)))
          a <- length(shps)
          names(shps) <- c(names(shps)[-a],shp)
          rm(list = gsub(".rda","",shp.rda,ignore.case = TRUE))
        } else {
          boundary1 <- suppressWarnings(readOGR(shp.dir,shp));
          if (req.cols[1] %in% names(boundary1@data) & req.cols[2] %in% names(boundary1@data)) {
            shps <- c(shps,boundary1)
            a <- length(shps)
            names(shps) <- c(names(shps)[-a],shp)
            assign(names(shps[a]),boundary1)
            save(list = names(shps[a]),file = paste0(shp.dir,"//",names(shps[a]),".rda"))
            rm(list = names(shps[a]))
            rm(boundary1)
          } else {
            warning(paste0("Warning: Shapefile ", shp, " didn't have the required columns so was skipped..." ))
          }
        }

      }
      if (length(shps)==0) {
        warning("Warning: Preprocessing of available images completed but post processing won't occur...")
        stop(paste0("No Shapefiles with correct columns available, please add at least one to: ", d.dir,"/shapefiles"))
      }
        return(shps)
    }
  };
  print("check4shapefiles - successfully loaded");
  check4treemasks <- function(dd = "Data Directory", ss = "Sentinel Directory", t = "Tiles") {
    tm.dir <- paste0(dd,"/treemasks")
    tm.nms <- paste0(tm.dir,"/T",t,"_treemask_-vesRemoved.tif")
    avail.tms <- list.files(tm.dir,pattern = ".tif",full.names = TRUE)
    tm2create <- tm.nms[!tm.nms %in% avail.tms]
    tm.nms <- t[!tm.nms %in% avail.tms]
    dd2 <- rep(dd,length(tm.nms))
    ss2 <- rep(ss,length(tm.nms))
    tm2create <- list(tm2create,tm.nms,dd2,ss2)
    if (length(tm2create[[1]])>0) {return(tm2create)}
  };
  print("check4treemasks - successfully loaded");
  select.treemasks <- function(tm = "Tree Masks to Create") {
    base::message(paste0("New treemask required for:T",tm[[2]],sep = "\n" ))
    tm1 <- c("All",paste0("T",tm[[2]]),"None")
    con <- if (interactive()) stdin() else file('stdin');
    print("Which new treemask would you like to process? Select All, None or one/multiple others separated by commas:");
    for (i in seq_along(tm1)){
      print(paste0(tm1[i]," [",i-1,"] ?"))
    }
    tm.no <- scan(file=con, sep=',', nlines=1, what = 'integer', quiet=TRUE);
    tm.no <- as.numeric(tm.no);
    tm.nam <- tm1[tm.no+1];
    rm(i,con);
    if ('None' %in% tm.nam) {
      base::message("Skipping treemask creation.  Please note shapefiles that lie on the identified tiles won't be processed")
      return(NULL)
    } else if ('All' %in% tm.nam) {
      base::message('Creating treemask for all identified tiles')
      tm2 <- tm[[1]]
      tm3 <- tm[[2]]
      tm4 <- tm[[3]]
      tm5 <- tm[[4]]
      for (i in seq_along(tm2)){
        tm7 <- c(tm2[i],tm3[i],tm4[i],tm5[i])
        if (!exists("tm6")){
          tm6 <- list(tm7)
        } else {
          tm6 <- c(tm6,list(tm7))
        }
      }
      rm(tm2,tm3,tm4,tm5,tm7,i)
      return(tm6)
    } else {
      print(paste0("Creating treemask for:",tm.nam,sep = "\n"))
      tm2 <- tm[[1]][tm.no]
      tm3 <- tm[[2]][tm.no]
      tm4 <- tm[[3]][tm.no]
      tm5 <- tm[[4]][tm.no]
      for (i in seq_along(tm2)){
        tm7 <- c(tm2[i],tm3[i],tm4[i],tm5[i])
        if (!exists("tm6")){
          tm6 <- list(tm7)
        } else {
          tm6 <- c(tm6,list(tm7))
        }
      }
      rm(tm2,tm3,tm4,tm5,tm7,i)
      return(tm6)
    }
  };
  print("select.treemasks - successfully loaded");
  create.treemask <- function(tm = " tm2create list") {
    print(paste0("Creating treemask for T",tm[2]))
    img.dir <- paste0(tm[4],"/T",tm[2])
    img.dtes1 <- list.dirs(img.dir,recursive = FALSE, full.names = TRUE)
    if (length(img.dtes1)!=0) {
      img.dtes2 <- list.dirs(img.dir,recursive = FALSE, full.names = FALSE)
      img.dtes2 <- img.dtes2[sapply(img.dtes1,done.files)]
      img.dtes1 <- img.dtes1[sapply(img.dtes1,done.files)]
      con <- if (interactive()) stdin() else file('stdin');
      print("Which image would you like to use to create treemask? (Select one only):");
      for (i in seq_along(img.dtes2)){
        print(paste0(img.dtes2[i]," [",i,"] ?"))
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
  print("create.treemask - successfully loaded")
  load.pm <- function() {
    pmods.ls <- list.files(paste0(w.dir,"/pasturemodels"),pattern = ".rda");
    for (i in seq_along(pmods.ls)){
      load(file = paste0(w.dir,"/pasturemodels/",pmods.ls[i]), verbose = FALSE, envir = .GlobalEnv);
      Sys.sleep(0);
    }; #End i Loop
    rm(i);
    Sys.sleep(2);
    return(pmods.ls)
  };
  print("load.pm - successfully loaded");
  list.shp.pms <- function(x) {
    as.character(unique(x@data$PAS_TYP))
  };
  print("list.shp.pms - successfully loaded");
  chk4mods.unavail <- function(mods = "Character vector of PM names", kill = "TRUE/FALSE") {
    load.pm()
    l.mods <- ls(envir = .GlobalEnv)[grep(".glm",ls(envir = .GlobalEnv))]
    l.mods <- gsub(".glm","",l.mods)
    mods <- mods[!mods %in% l.mods]
    if (length(mods)==0) {
      base::message("All pasture models needed for current shapefiles are loaded")
      return(NULL)
    } else {
      if (kill){
        base::message("Some pasture models needed for current shapefiles are not available")
        base::message(paste0("Please update shapefile PAS_TYP columns to only include available model names or add a calibration .csv file to ", d.dir, "/calibrationdata before next run"))
        Sys.sleep(10)
        stop(paste0("Unavailable and required pasture model:",mods, sep = "\n"))
      }
      if (!kill){
        print("Some pasture models needed for current shapefiles are not available - will try and make them from calibration data if available")
        return(mods)
      }
    }
  };
  print("chk4mods.unavail - successfully loaded");
  build.new.mods <- function(dd = d.dir) {
    calib.dta.dir <- paste0(dd,"/calibrationdata")
    calib.dta.files1 <- list.files(calib.dta.dir, pattern = "_calib.csv", ignore.case = TRUE, full.names = FALSE)
    calib.dta.files1 <- gsub("_calib.csv","",calib.dta.files1)
    mods2build <- chk4mods.unavail(calib.dta.files1,kill = FALSE)
    if (is.null(mods2build)) {
      print("No new model data available - not building any new models")
    } else {
      for (i in seq_along(mods2build)){
        mod.nam <- mods2build[i]
        print(paste("Building", mod.nam, "model..."))
        dta.in <- read.csv(paste0(calib.dta.dir,"/",mod.nam,"_calib.csv"),header = TRUE, stringsAsFactors = FALSE)
        print(head(dta.in))
        Sys.sleep(2)
        load(file = paste0(w.dir,"/pasturemodels/ACS211NDVI-SentinelNDVI.rda"), verbose = TRUE);
        #get('dta.lm1',envir = .GlobalEnv)
        dta.cor <- data.frame('NDVI.ACS211' = dta.in$NDVI, stringsAsFactors = FALSE);
        dta.cor$NDVI.SENTINEL <- predict(dta.lm1,newdata = dta.cor,type = 'response');
        dta.in$NDVI <- dta.cor$NDVI.SENTINEL;
        rm(dta.lm1);
        ndvi1 <- paste0(mod.nam,".CC_NDVI")
        gdm1 <- paste0(mod.nam,".GDM")
        colnames(dta.in) <- c(ndvi1,gdm1);
        ndvi2 <- paste0("dta.in$",ndvi1)
        gdm2 <- paste0("dta.in$",gdm1)
        dta.in <- dta.in[with(dta.in, order(eval(parse(text=ndvi2)))), ];
        mod.nam2 <- paste0(mod.nam,".glm")
        assign(mod.nam2,glm(eval(parse(text=gdm1))~eval(parse(text=ndvi1)), family = quasipoisson(link = "log"), data = dta.in))
        pred.seq <- data.frame(col1 = sort(eval(parse(text=ndvi2))))
        colnames(pred.seq) <- ndvi1
        pred.glm <- predict(eval(parse(text=mod.nam2)),pred.seq,type = 'response');
        x11()
        plot(eval(parse(text=gdm2))~eval(parse(text=ndvi2)), pch = 16, cex = 0.75, main = mod.nam, xlab = "CropCircle Derived Sentinel NDVI", ylab = "Green Dry Matter (kg/ha)");
        lines(y = pred.glm, x = pred.seq[[1]], lty = 2, col = 'red');
        Sys.sleep(10)
        dev.off()
        pdf(file = paste0(w.dir,"/pasturemodels/",mod.nam,"_calib.pdf"))
        plot(eval(parse(text=gdm2))~eval(parse(text=ndvi2)), pch = 16, cex = 0.75, main = mod.nam, xlab = "CropCircle Derived Sentinel NDVI", ylab = "Green Dry Matter (kg/ha)");
        lines(y = pred.glm, x = pred.seq[[1]], lty = 2, col = 'red');
        dev.off()
        pm <- eval(parse(text=mod.nam2))
        save(list = mod.nam2, file = paste0(w.dir,"/pasturemodels/",mod.nam,"_calib.rda"))
        print(paste("...", mod.nam, "model built"))
        Sys.sleep(3)
      }
    }
  };
  print("build.new.mods - successfully loaded");
  load.propmeta <- function() {
    if(file.exists(paste0(w.dir,"/settings/property-metadata.rda"))) {
      load(file = paste0(w.dir,"/settings/property-metadata.rda"), verbose = TRUE, envir = .GlobalEnv);
    } else {
      propmeta <- data.frame( "ID" = NA,
                              "Property" = NA,
                              "Sentinel" = NA,
                              "Shapefile" = NA,
                              "TreeMask" = NA,
                              "OutLoc" = NA,
                              "CRS.no" = NA,
                              "AppFile" = NA,
                              "AppFile2nd" = NA,
                              "Logo" = NA,
                              "MonthlyOnly" = NA,
                              "ShinyAppAccnt" = NA,
                              "ShinyAppAccnt2nd"= NA,
                              stringsAsFactors = FALSE)
      save(list = "propmeta", file = paste0(w.dir,"/settings/property-metadata.rda"))
      load(file = paste0(w.dir,"/settings/property-metadata.rda"), verbose = TRUE, envir = .GlobalEnv);
    }
    
  };
  print("load.propmeta - successfully loaded");
  md5.check <- function(dd = "Data Directory") {
    GeoJSON.Files <- list.files(paste0(dd,"/DataOut"), recursive = TRUE, pattern = ".geojson", full.names = TRUE);
    if (length(GeoJSON.Files)>0) {
      a <- length(strsplit(GeoJSON.Files[1], split = "/")[[1]])-2
      GeoJSON.Files.MD5 <- md5sum(files = GeoJSON.Files);
      MD5.check1 <- data.frame(filename = GeoJSON.Files,
                               MD5Checksum = GeoJSON.Files.MD5,
                               stringsAsFactors = FALSE);
      MD5.check1$property <- gsub("_GDM_AllAvailableDates.geojson","",sapply(strsplit(MD5.check1$filename, split = "/"),"[",a));
      row.names(MD5.check1) <- 1:length(MD5.check1$filename)
      rm(a,GeoJSON.Files,GeoJSON.Files.MD5);
      return(MD5.check1)
    } else {
      return(NULL)
    }
  };
  print("md5.check - successfully loaded");
  which.farms <- function(dd = "Data Directory") {
    avfarm.ls <- list.dirs(paste0(dd,"/dataout"),recursive = FALSE, full.names = FALSE);
    if (length(avfarm.ls)==0) {
      avfarm.ls <- c("New Property");
    } else {
      avfarm.ls <- c("All",avfarm.ls,"New Property");
    }
    con <- if (interactive()) stdin() else file('stdin');
    print("Which properties would you like to process? Select All, New Property or one/multiple others separated by commas:");
    for (i in seq_along(avfarm.ls)){
      print(paste(avfarm.ls[i],"[",i-1,"] ?"))
    }
    property.no <- scan(file=con, sep=',', nlines=1, what = 'integer', quiet=TRUE);
    property.no <- as.numeric(property.no);
    property.nam <- avfarm.ls[property.no+1];
    rm(i,con,property.no);
    return(property.nam)
  };
  print("which.farms - successfully loaded");
  test_intersection2 <- function(a,b){
    out <- tryCatch(
      {
        print("Checking layer intersection")
        #length(crop(a,b))>0;
        #mask(a,b);
        ndvi1 <- crop(a,b);
        #mask(ndvi1,c);
        print("Success - layers intersect!")
        return(TRUE);
      },
      error=function(cond) {
        print(paste("Shapefile doesn't intersect ndvi raster"))
        # Choose a return value in case of error
        return(FALSE)
      },
      finally={
        
      }
    )    
    return(out)
  };
  print("test_intersection2 - successfully loaded!");
  check.which.tile <- function(ss = "Sentinel Directory",dd = "Data Directory", shp.nam = "Shapefile Name", shps = "Loaded Shapefiles") {
    print(paste("Testing",shp.nam,"for intersection with available Sentinel tiles"));
    Sys.sleep(2);
    img.loc <- ss
    print(paste0("Connecting to folder: ",img.loc));
    img.ls <- list.dirs(img.loc, recursive = FALSE, full.names = FALSE);
    img.ls <- img.ls[grepl("^T",img.ls)]
    for (i in seq_along(img.ls)){
      a <- list.dirs(paste0(img.loc,"/",img.ls[i]),recursive = FALSE)
      if (length(a)>0) {
        a <- a[sapply(a,done.files)]
        if (length(a)==0) {
          img.ls <- img.ls[-i]
        }
      } else {
        img.ls <- img.ls[-i]
      }
    }
    rm(i)
    
    tm.list <- list.files(paste0(dd,"/treemasks/"), pattern = ".tif$")
    tm.list <- gsub("_treemask_-vesRemoved.tif","",tm.list)
    img.ls <- img.ls[img.ls %in% tm.list]
    
           if (length(img.ls) < 1) {
      warning(paste("Sentinel Images not found at",img.loc));
      Sys.sleep(2);
      warning("No processed imagery found
            1. Ensure data directory is accessible
            2. Ensure some imagery is downloaded then rerun script");
      Sys.sleep(5);
      stop("No processed imagery found to match with shapefiles");
    };
    Sys.sleep(3);
    print("Available Images: ");
    print(img.ls);
    Sys.sleep(3);
    boundary1 <- shps[shp.nam %in% names(shps)][[1]]
    int.lay <- data.frame(matrix(ncol = 2, nrow = 0));
    colnames(int.lay) <- c("Layer","Intersects");
    for (i in seq_along(img.ls)) {
      print(paste("Testing:", img.ls[i]));
      treemask.nam <- paste0(dd,"/treemasks/",img.ls[i],"_treemask_-vesRemoved.tif")
      treemask1 <- raster(treemask.nam);
      dims1 <- extent(boundary1);
      int.lay[i,] <- c(img.ls[i],test_intersection2(treemask1,dims1));
      rm(treemask.nam, dims1, treemask1);
      gc();
      gc();
      Sys.sleep(0)
    }; # END i Loop
    rm(i)
    int.lay$Intersects <- as.logical(int.lay$Intersects);
    int.lay <- int.lay$Layer[int.lay$Intersects];
    if (length(int.lay) > 1) {
      con <- if (interactive()) stdin() else file('stdin');
      base::message("Your shapefile intersected more than one layer, please choose the layer to use:");
      for (i in seq_along(int.lay)){
        print(paste(int.lay[i],"[",i,"] ?"))
      }
      int.lay.no <- scan(file=con, sep=',', nlines=1, what = 'integer', quiet=TRUE);
      int.lay.no <- as.numeric(int.lay.no);
      int.lay <- int.lay[int.lay.no];
      rm(int.lay.no,con)
    }
    if (length(int.lay)==0) {
      warning("Sorry, your shapefile didn't overlap any imagery or you havent created a treemask for that tile yet.
            Please download some and create a corresponding tree mask before retrying...");
      print("Script will autoclose in 10secs...")
      Sys.sleep(10);
      stop(paste("Need Images for:",shp.nam));
    }
    return(int.lay);
  };
  print("check.which.tile - successfully loaded");
  load.UCRS <- function() {
    south <- paste0("+proj=utm +zone=",1:60," +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    north <- paste0("+proj=utm +zone=",1:60," +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    world <- c(south,north)
    UCRS <- sapply(world,CRS)
    return(UCRS)
  }
  print("load.UCRS - successfully loaded");
  process.new.farm <- function(pm = "Property Meta-Data") {
    pm = propmeta
    newpropertyrow <- data.frame(ID = max(propmeta$ID)+1, stringsAsFactors = FALSE);
    newpropertyrow[1,(is.infinite(newpropertyrow$ID[1]))] <- 1
    {con <- if (interactive()) stdin() else file('stdin');
      print("What is the name of your new property?");
      property.nam <- scan(file=con, sep=',', nlines=1, what = 'character', quiet=TRUE);
      property.nam <- make.names(gsub(" ","",property.nam), allow_ = FALSE)
      property.nam <- gsub("[.]","-",property.nam)
      print(paste("This script is processing data for new property:", property.nam, "..."));
      rm(con);
      Sys.sleep(3);} # Set new property name
    newpropertyrow$Property <- property.nam
    {con <- if (interactive()) stdin() else file('stdin');
      print(c(paste("Please ensure shapefile for",property.nam, "is in "),paste0(d.dir,"/shapefiles")));
      Sys.sleep(1);
      print("Ensure that your shapefile has a 'PADD_NAME' and a 'PAS_TYP' column in the attributes table");
      Sys.sleep(1);
      print("Once shapefile is ready, Press [enter] to proceed");
      scan(file=con, sep=',', nlines=1, what = 'integer', quiet=TRUE);
      rm(con);} # Check Shapefiles
    shps <- check4shapefiles(dd = d.dir)
    {avshp.ls <- names(shps)
      con <- if (interactive()) stdin() else file('stdin');
      print("Select your shapefile:");
      for (i in seq_along(avshp.ls)){
        print(paste(avshp.ls[i],"[",i,"] ?"))
      }
      shp.no <- scan(file=con, sep=',', nlines=1, what = 'integer', quiet=TRUE);
      shp.no <- as.numeric(shp.no);
      shp.nam <- avshp.ls[shp.no];
      rm(i,con,shp.no);
      print(paste("You selected the shapefile named:", shp.nam));
      print(paste("You entered property name:", property.nam))
      Sys.sleep(2);
      if (shp.nam!=property.nam) {
        print("Note: The property name you provide will appear on the apps");
        base::message("Shapefile name chosen doesn't match your entered property name 
            Would you like to: 
            [a] change your property name
            [b] leave it
            [c] select shapefile again??");
        con <- if (interactive()) stdin() else file('stdin');
        dec <- scan(file=con, sep=',', nlines=1, what = 'character', quiet=TRUE);
        rm(con);
        Sys.sleep(2);
        if (dec == "a") {
          print("Enter your new property name:")
          con <- if (interactive()) stdin() else file('stdin');
          property.nam <- scan(file=con, sep=',', nlines=1, what = 'character', quiet=TRUE);
          rm(con);
          print(paste("Final property name is", property.nam, "moving on..."));
          Sys.sleep(2)
        };
        if (dec == "b") {
          print(paste("Sticking with", property.nam, "and moving on..."));
          Sys.sleep(2)
        };
        if (dec == "c") {
          con <- if (interactive()) stdin() else file('stdin');
          print("Select your shapefile again:");
          for (i in seq_along(avshp.ls)){
            print(paste(avshp.ls[i],"[",i,"] ?"))
          }
          shp.no <- scan(file=con, sep=',', nlines=1, what = 'integer', quiet=TRUE);
          shp.no <- as.numeric(shp.no);
          shp.nam <- avshp.ls[shp.no];
          rm(i,con,shp.no);
          Sys.sleep(1);
          print(paste("This time you selected the shapefile named:", shp.nam));
          Sys.sleep(1);
          print(paste("You entered property name:", property.nam, ", moving on..."));
          Sys.sleep(2);
        }
        
      } else {
        print("Shapefile name matches property name, moving on...");
        Sys.sleep(2)}
    } # Select Shapefile
    img.nam <- check.which.tile(ss = s.dir,dd = d.dir,shp.nam = shp.nam,shps = shps)
    print(paste("Your new property", property.nam, "is covered by the", img.nam, "imagery"))
    newpropertyrow$Sentinel <- img.nam;
    newpropertyrow$Shapefile <- shp.nam
    newpropertyrow$TreeMask <- paste0("/treemasks/", img.nam, "_treemask_-vesRemoved.tif");
    newpropertyrow$OutLoc <- paste0("/dataout/",property.nam);
    print(paste0("Creating new output directory [",d.dir,"/dataout/",property.nam,"]"));
    dir.create(paste0(d.dir,"/dataout/",property.nam),showWarnings = TRUE, recursive = FALSE);
    dir.create(paste0(d.dir,"/dataout/", property.nam,"/Maps"), showWarnings = FALSE);
    boundary1 <- shps[shp.nam %in% names(shps)][[1]]
    if( grepl("+south",crs(boundary1))) {
      newpropertyrow$CRS.no <- as.integer(na.omit((1:60)[unlist(lapply(UCRS,function(x){compareCRS(x,crs(boundary1))}))]))
    }
    if( grepl("+north",crs(boundary1))) {
      newpropertyrow$CRS.no <- as.integer(na.omit((1:60)[unlist(lapply(UCRS,function(x){compareCRS(x,crs(boundary1))}))]))
    }
    if (dim(newpropertyrow)[2]==6) {
      print("Sorry, your shapefiles CRS doesn't match the currently supported CRSs.  
            Please ensure the shapefile is in WGS84 UTM Zone XXY
            Where XX = the zone (1:60) and y = the hemisphere (S or N)");
      print("Script will autoclose in 10secs...")
      Sys.sleep(10);
      stop("shapefiles CRS doesn't match the currently supported CRS");
    }
    {con <- if (interactive()) stdin() else file('stdin');
      print("What Shiny App do you want the new property to appear on?");
      av.shiny.apps <- c(list.dirs(path = paste0(d.dir,"/shinyapps"),recursive = FALSE, full.names = FALSE),"New");
      av.shiny.apps.locs <- list.dirs(path = paste0(d.dir,"/shinyapps"),recursive = FALSE, full.names = FALSE);
      av.shiny.apps.map.loc <- list.dirs(path = paste0(d.dir,"/shinyapps"),recursive = FALSE, full.names = FALSE);
      
      for (i in seq_along(av.shiny.apps)){
        print(paste(av.shiny.apps[i],"[",i,"] ?"))
      }
      app.no <- scan(file=con, sep=',', nlines=1, what = 'integer', quiet=TRUE);
      app.no <- as.numeric(app.no);
      rm(con);
      if (app.no <= length(av.shiny.apps.locs)) {
        newpropertyrow$AppFile <- paste0("/shinyapps/",av.shiny.apps.locs[app.no],"/www/DataOut");
        newpropertyrow$AppFile2nd <- NA;
        dir.create(paste0(d.dir,newpropertyrow$AppFile[1],"/",property.nam,"/GeoJson"),recursive = TRUE);
        print(paste("You new property will appear on", av.shiny.apps.locs[app.no], "and the master app..."))
        Sys.sleep(3);
      } else {
        con <- if (interactive()) stdin() else file('stdin');
        print("What name would you like to give the new shiny app?");
        new.app.name <- scan(file=con, sep=',', nlines=1, what = 'character', quiet=TRUE);
        new.app.name <- paste0(new.app.name,"_",stri_rand_strings(n = 1,length = 15,pattern = "[A-Za-z0-9]"));
        new.app.name <- gsub(" ","",new.app.name)
        dir.create(paste0(d.dir,"/shinyapps/",new.app.name,"/www/DataOut/",property.nam,"/GeoJson"),recursive = TRUE);
        file.copy(paste0(w.dir,"/shinyapp/app.R"),paste0(d.dir,"/shinyapps/",new.app.name,"/app.R"),overwrite = FALSE,copy.date = TRUE);
        newpropertyrow$AppFile <- paste0("/shinyapps/",new.app.name,"/www/DataOut");
        newpropertyrow$AppFile2nd <- NA;
        rm(con);
        print(paste("You new property will appear on", new.app.name, "and the master app..."))
        Sys.sleep(3);
      };
    } # Create Shiny App Name and Dirs
    {con <- if (interactive()) stdin() else file('stdin');
      print("What logo do you want to display on the maps?");
      log.ls <- list.files(paste0(w.dir,"/logos"));
      for (i in seq_along(log.ls)){
        print(paste(log.ls[i],"[",i,"] ?"))
      }
      log.no <- scan(file=con, sep=',', nlines=1, what = 'integer', quiet=TRUE);
      log.no <- as.numeric(log.no);
      newpropertyrow$Logo <- log.ls[log.no];
      rm(con,i);
      if (app.no >= length(av.shiny.apps.locs)) {
        print(paste("Copying", log.ls[log.no], "to your new shiny app to display along with PARG logo..."));
        file.copy(paste0(w.dir,"/logos/",log.ls[log.no]),paste0(d.dir,"/shinyapps/",new.app.name,"/www/logo2.png"),overwrite = FALSE,copy.date = TRUE);
        file.copy(paste0(w.dir,"/logos/parg.png"),paste0(d.dir,"/shinyapps/",new.app.name,"/www/parg.png"),overwrite = FALSE,copy.date = TRUE);
      };} # Choose logo to display
    {con <- if (interactive()) stdin() else file('stdin');
      print("Should this property only display 1 image per month on app?");
      im.freq <- c("TRUE", "FALSE");
      for (i in seq_along(im.freq)){
        print(paste(im.freq[i],"[",i,"] ?"))
      }
      im.no <- scan(file=con, sep=',', nlines=1, what = 'integer', quiet=TRUE);
      im.no <- as.numeric(im.no);
      newpropertyrow$MonthlyOnly <- im.freq[im.no];
      rm(con,i)} # One Image per month?
    {con <- if (interactive()) stdin() else file('stdin');
      print("Which shinyapps.io account should this properties app sit on?");
      for (i in seq_along(avail.app.accts)){
        print(paste(avail.app.accts[i],"[",i,"] ?"))
      }
      app.no <- scan(file=con, sep=',', nlines=1, what = 'integer', quiet=TRUE);
      app.no <- as.numeric(app.no);
      newpropertyrow$ShinyAppAccnt <- avail.app.accts[app.no];} # Which App Acct?
    rm(con,i,app.no,im.no,im.freq,log.ls,log.no,img.nam,avshp.ls);
    gc();
    gc();
    newpropertyrow$ShinyAppAccnt2nd <- NA
    propmeta <- rbind(propmeta,newpropertyrow);
    propmeta <- propmeta[!is.na(propmeta$Property),]
    propmeta <- propmeta[order(propmeta$Property),];
    propmeta$ID <- 1:length(propmeta$ID);
    save(propmeta, file = paste0(w.dir,"/settings/property-metadata.rda"));
    rm(propmeta);
    load(file = paste0(w.dir,"/settings/property-metadata.rda"), envir = .GlobalEnv);
    print("Property Metadata Updated");
    Sys.sleep(3);
    avfarm.ls <- list.dirs(paste0(d.dir,"/dataout"),recursive = FALSE, full.names = FALSE);
    con <- if (interactive()) stdin() else file('stdin');
    print("Please choose your new property only:");
    for (i in seq_along(avfarm.ls)){
      print(paste(avfarm.ls[i],"[",i,"] ?"))
    }
    property.no <- scan(file=con, sep=',', nlines=1, what = 'integer', quiet=TRUE);
    property.no <- as.numeric(property.no);
    property.nam <- avfarm.ls[property.no];
    rm(i,con,property.no);
    return(property.nam);
  }
  print("process.new.farm - successfully loaded");
  skip.fun <- function() {
    con <- if(interactive()) stdin() else file('stdin');
    print("Do you want to reprocess all imagery or skip previously processed [all/skip]");
    skip <- scan(file = con,nlines = 1,what = 'character',quiet = TRUE);
    rm(con)
    skip <- tolower(skip)
    if (skip == 'skip'| skip == 's' | skip == 'S' | skip == 'yes' | skip == 'skip them!' | skip == 'true') {
      skip <- TRUE;
      base::message("Skipping previously processed imagery for each selected property!");
      Sys.sleep(1);
    } else {
      skip <- FALSE;
      base::message("Reprocessing all imagery for each selected property!");
      print("Hit [Esc] to cancel at any time")
      Sys.sleep(1);
    }
    return(skip)
  };
  print("skip.fun - successfully loaded");
  fast.fun <- function() {
    con <- if(interactive()) stdin() else file('stdin');
    print("Do you want to parallel compute the post processing? [yes/no]");
    fast <- scan(file = con,nlines = 1,what = 'character',quiet = TRUE);
    rm(con)
    fast <- tolower(fast)
    if (fast == 'yes' | fast == 'fast'| fast == 'f' | fast == 'true') {
      fast <- TRUE;
      base::message("Parallel computing the estimates for each selected properties!");
      Sys.sleep(1);
    } else {
      fast <- FALSE;
      base::message("Not Parallel computing the property estimates for each selected property");
      base::message("Hit [Esc] to cancel at any time")
      Sys.sleep(1);
    }
    return(fast)
  };
  print("fast.fun - successfully loaded");
}
####END SCRIPT####
