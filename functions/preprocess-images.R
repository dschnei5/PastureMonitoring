######################################################################################
##############################preprocess-images.R#####################################
######################################################################################

####User Defined Functions####

any.zips <- function (x) {
  any(grepl("..zip$",list.files(x))==TRUE)
};
message("any.zips - successfully loaded!");
sen.folds <- function (d = "Data Directory") {
  sentinel.folds <- list.dirs(path = paste0(d,"/sentinelimages"), recursive = FALSE);
  sentinel.folds <- sentinel.folds[!grepl("sentinelimages_ntmosaic",sentinel.folds)];
  sentinel.dirs <- vector();
  for (i in seq_along(sentinel.folds)){
    if (length(list.dirs(sentinel.folds[i]))>1) {
      sentinel.dirs.tmp <- list.dirs(sentinel.folds[i], recursive = FALSE);
      sentinel.lng <- unlist(lapply(sentinel.dirs.tmp,done.files));
      sentinel.dirs.tmp <- sentinel.dirs.tmp[!sentinel.lng];
      sentinel.lng2 <- unlist(lapply(sentinel.dirs.tmp,any.zips));
      sentinel.dirs.tmp <- sentinel.dirs.tmp[sentinel.lng2];
      sentinel.dirs.tmp <- sentinel.dirs.tmp[grepl("Sentinel_",sentinel.dirs.tmp)];
      sentinel.dirs <- c(sentinel.dirs,sentinel.dirs.tmp);
    }
  } # END i Loop
  rm(i, sentinel.dirs.tmp, sentinel.lng, sentinel.lng2);
  message(paste(Sys.time(),"- There are", length(sentinel.dirs), "directories to pre-process.  It can take up to 40mins/tile/directory to process..."));
  if(length(sentinel.dirs)>0) {print(sentinel.dirs)};
  return(sentinel.dirs)
};
message("sen.folds - successfully loaded");
unzip <- function(dir = "Directory", fn = "FileName") {
  comm.zip <- paste0("powershell -command Expand-Archive -Path ",paste0(dir,"/",fn)," -DestinationPath ", paste0(dir,"/unzipped"), " -Force"  );
  comm.zip <- gsub("/","\\\\",comm.zip)
  for (i in seq_along(comm.zip)){
    system(comm.zip[i], wait = TRUE);
    print(paste(Sys.time(),"- File", files.zip[i], "unzipped"));
  }; #END i Loop
  rm(i);
};
message("unzip - successfully loaded");
sen2cor <- function(dir = "Directory"){
  files.SAFE <- list.dirs(path=paste0(dir,"/unzipped"), recursive = FALSE, full.names = TRUE);
  files.sen2cor <- list.dirs(path=paste0(dir,"/unzipped"), recursive = FALSE, full.names = FALSE);
  comm.sen2cor <- paste0("L2A_Process.bat ",files.SAFE);
  comm.sen2cor <- gsub("/","\\\\",comm.sen2cor)
  shell(comm.sen2cor, wait = TRUE);
  print(paste(Sys.time(),"- File", files.sen2cor, "processed"));
  rm(comm.sen2cor,comm.zip,files.SAFE,files.zip);
}
message("sen2cor - successfully loaded");
preprocess.sentinel <- function(x) {
  #d.dir <- get("d.dir", envir = .GlobalEnv);
  files.zip <- list.files(x,pattern = "\\.zip",include.dirs = FALSE);
  suppressWarnings(dir.create(paste0(x,"/unzipped")));
  suppressWarnings(dir.create(paste0(x,"/maps")));
  suppressWarnings(dir.create(paste0(x,"/ready")));
  if (length(list.dirs(paste0(x,"/unzipped"),recursive = FALSE))==0) {
    unzip(dir = x,fn = files.zip)
    sen2cor(dir = x)
    } else if (length(list.dirs(paste0(x,"/unzipped"),recursive = FALSE))==1) {
      unlink(list.dirs(paste0(x,"/unzipped"),recursive = FALSE,full.names = TRUE),recursive = TRUE);
      unzip(dir = x,fn = files.zip)
      sen2cor(dir = x)
    } else if (length(list.dirs(paste0(x,"/unzipped"),recursive = FALSE))==2) {
      files.READY <- list.dirs(path=paste0(x,"/unzipped"), recursive = FALSE, full.names = TRUE);
      files.READY <- files.READY[grepl("MSIL2A",files.READY)];
      files <- list.files(files.READY, pattern = "._10m.jp2$", recursive = TRUE);
      if (length(files) < 7){
        unlink(files.READY, recursive = TRUE);
        unzip(dir = x,fn = files.zip)
        sen2cor(dir = x)
      }
    }

};
message("preprocess.sentinel - successfully loaded");
create.tifs <- function(x) {
  files.READY <- list.dirs(path=paste0(x,"/unzipped"), recursive = FALSE, full.names = TRUE);
  files.READY <- files.READY[grepl("MSIL2A",files.READY)];
  if (length(files.READY)>=2){
    df <- file.info(list.dirs(path=paste0(x,"/unzipped"),full.names = TRUE,recursive=FALSE));
    files.READY <- rownames(df)[which.max(df$mtime)]
  }
  files <- list.files(files.READY, pattern = "._10m.jp2$", recursive = TRUE);
  if (length(files) >= 7){
    files.READY2 <- list.dirs(path=paste0(x,"/unzipped"), recursive = FALSE, full.names = FALSE);
    files.READY2 <- files.READY2[grepl("MSIL2A",files.READY2)];
    imageryname <- substr(files.READY2[1],1,19)
    imagerydate <- substr(imageryname,12,19)
    imagerydate <- ymd(imagerydate)
    imagerydate <- as.character(imagerydate)
    img01.bands <- list.files(files.READY[1], pattern = "._10m.jp2$", full.names = TRUE, ignore.case = TRUE, recursive = TRUE)[c(4,3,2,5)];
    img01.bands <- gsub("/","\\\\",img01.bands)
    print(paste(Sys.time(),"- Creating stack, this takes time..."))
    r1 <- raster(readGDAL(img01.bands[1]));
    r2 <- raster(readGDAL(img01.bands[2]));
    r3 <- raster(readGDAL(img01.bands[3]));
    r4 <- raster(readGDAL(img01.bands[4]));
    img01 <- stack(r1,r2,r3,r4);
    rm(r1,r2,r3,r4);
    gc();
    t1 <- now();
    print(paste(t1,"Writing RGB raster to file, this takes time..."));
    writeRaster(img01, file= paste0(x,"/ready/",imageryname,"_rgb.tif"), format="GTiff", overwrite=TRUE);
    print(paste(now(),"- Done - run time =",ceiling(difftime(now(),t1,units = "sec")),"seconds"));
    t1 <- now();
    print(paste(t1,"- Converting stack to brick, this takes time..."));
    img02 <- brick(img01);
    print(paste(now(),"- Done - run time =",ceiling(difftime(now(),t1,units = "sec")),"seconds"));
    rm(img01);
    gc();
    gc();
    t1 <- now();
    print(paste(t1,"- Creating NDVI image, this takes time..."));
    ndvi <- (img02[[4]] - img02[[1]]) / (img02[[4]] + img02[[1]]);
    print(paste(now(),"- Done - run time =",ceiling(difftime(now(),t1,units = "sec")),"seconds"));
    t1 <- now();
    print(paste(t1,"Writing NDVI raster to file, this takes time..."));
    writeRaster(x = ndvi,file= paste0(x,"/ready/",imageryname,"_ndvi.tif"), format = "GTiff", overwrite = TRUE);
    print(paste(now(),"- Done - run time =",ceiling(difftime(now(),t1,units = "sec")),"seconds"));
    out.mess <- "This file indicates that preprocessing had been performed.  It is generated to ensure that further processing is not attempted on this folder.  Please delete this file, along with the 'unzipped'; 'ready' and 'maps' folders if you wish to rerun the preprocessing loop on this raw image folder"
    write.table(out.mess, file = paste0(x,"/ProcessingCompleted.inf"), row.names = FALSE, col.names = FALSE);
    rm(t1,img02,ndvi);
    gc();
    gc();

  }
};
message("create.tifs - successfully loaded");
preprocess.fast <- function(dirs = "Sentinel Directories Requiring Preprocessing") {
  no_cores <- detectCores();
  registerDoParallel(makeCluster(no_cores,outfile="tmp//debug_file.txt"));  #8 cores works well for quad core processor (you can set it to as many cores as you like but the process of allocating task to all the individual cores you create increases the overall processing time when you add too many - there's a sweet spot)
  getDoParWorkers();
  a2 <- seq_along(dirs);
  foreach(j=a2, .packages=c('lubridate','raster', 'rgdal', 'sp'), .export = ls(.GlobalEnv)) %dopar% {
    preprocess.sent(dirs[j])
    create.tifs(dirs[j])
  };
  stopImplicitCluster();
};
message("preprocess.fast - successfully loaded")

####END SCRIPT####