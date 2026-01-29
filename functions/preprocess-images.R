######################################################################################
##############################preprocess-images.R#####################################
######################################################################################

####User Defined Functions####

any.zips <- function (x) {
  any(grepl("..zip$",list.files(x))==TRUE)
};
print("any.zips - successfully loaded!");
sen.folds <- function (ss = "Sentinel Directory") {
  sentinel.folds <- list.dirs(path = ss, recursive = FALSE);
  sentinel.folds <- sentinel.folds[!grepl("NTmosaic",sentinel.folds)];
  sentinel.dirs <- vector();
  for (i in seq_along(sentinel.folds)){
    base::message(paste("Checking folder", i, "of", length(sentinel.folds), "Folder:", sentinel.folds[i]))
    if (length(list.dirs(sentinel.folds[i],recursive = FALSE))>=1) {
      sentinel.dirs.tmp <- list.dirs(sentinel.folds[i], recursive = FALSE);
      base::message("Checking for previously completed files")
      sentinel.lng <- unlist(pblapply(sentinel.dirs.tmp,done.files));
      sentinel.dirs.tmp <- sentinel.dirs.tmp[!sentinel.lng];
      base::message("Checking For Zipped Files")
      sentinel.lng2 <- unlist(pblapply(sentinel.dirs.tmp,any.zips));
      sentinel.dirs.tmp <- sentinel.dirs.tmp[sentinel.lng2];
      sentinel.dirs.tmp <- sentinel.dirs.tmp[grepl("Sentinel_",sentinel.dirs.tmp)];
      sentinel.dirs <- c(sentinel.dirs,sentinel.dirs.tmp);
    }
  } # END i Loop
  suppressWarnings(rm(i, sentinel.dirs.tmp, sentinel.lng, sentinel.lng2));
  base::message(paste(Sys.time(),"- There are", length(sentinel.dirs), "directories to pre-process.  It can take up to 40mins/tile/directory to process..."));
  if(length(sentinel.dirs)>0) {base::message(paste("Image:",sentinel.dirs, " ", sep = "\n"))};
  return(sentinel.dirs)
};
print("sen.folds - successfully loaded");
unzip <- function(dir = "Directory", fn = "FileName" ) {
  # For Linux: using unzip command
  for (i in seq_along(fn)){
    comm.zip <- paste0("unzip -q -o ", dir, "/", fn[i], " -d ", dir, "/unzipped");
    system(comm.zip, wait = TRUE);
    print(paste(Sys.time(),"- File", fn[i], "unzipped"));
  }; #END i Loop
  rm(i);
};
print("unzip - successfully loaded");
sen2cor <- function(dir = "Directory"){
  # For Linux: using L2A_Process.py script
  files.SAFE <- list.dirs(path=paste0(dir,"/unzipped"), recursive = FALSE, full.names = TRUE);
  files.sen2cor <- list.dirs(path=paste0(dir,"/unzipped"), recursive = FALSE, full.names = FALSE);
  for (i in seq_along(files.SAFE)) {
    comm.sen2cor <- paste0("L2A_Process.py ", files.SAFE[i]);
    system(comm.sen2cor, wait = TRUE);
    print(paste(Sys.time(),"- File", files.sen2cor[i], "processed"));
  }
  rm(files.SAFE);
}
print("sen2cor - successfully loaded");
dwnld.imgs <- function(x = "tile"){
  pw <- check4pw(usr = cop.usr,ser = "Copernicus")
  paste0("Checking last ", numdaysback,"days for imagery with <", cld.pc,"% cloud for tile ",x)
  query01 <- paste0("wget --no-check-certificate --user=",cop.usr," --password=",pw," --auth-no-challenge --output-document=tmp/query_results.txt \"https://scihub.copernicus.eu/dhus/search?q=",x," AND producttype:S2MSI1C AND cloudcoverpercentage:[0 TO ",cld.pc,"] AND endposition:[NOW-",numdaysback,"DAYS TO NOW]&format=json\"");
  system(query01, wait = TRUE)
  info <- jsonlite::fromJSON(txt = "tmp/query_results.txt")
  unlink("tmp/query_results.txt")
  numresults1 <- info$feed$`opensearch:totalResults` 
  img.dates <- info$feed$entry$summary
  img.href <- info$feed$entry$link
  base::message(paste("Number of",x,"cloud free images found within the last", numdaysback,"days =",numresults1))
  if (numresults1 > 0) {
    print(paste("Downloading new images for", x));
    for (i in 1:numresults1){
      print(i)
      UUID <- img.href[[i]][[1]][1];
      img.date <- strapplyc(strsplit(img.dates[i],",")[[1]][1],"[0-9]{4}-[0-9]{2}-[0-9]{2}", simplify = TRUE)
      wd <- getwd()
      new.wd <- paste0(s.dir,"/T",x,"/Sentinel_",img.date)
      creationsuccess <- suppressWarnings(dir.create(new.wd,recursive = TRUE));
      if (!creationsuccess) {
        len.dir <- length(list.files(new.wd,recursive = TRUE));
        if (len.dir >= 1) {
          creationsuccess <- FALSE
        } else {
          creationsuccess <- TRUE
        }
      }
      
      if (creationsuccess){
        setwd(new.wd);
        query02 <- paste0("wget --no-http-keep-alive --content-disposition --auth-no-challenge --continue --user=",cop.usr," --password=",pw," \"",UUID,"\"");
        ##https://scihub.copernicus.eu/dhus/odata/v1/Products('14215f82-94c1-442b-b470-db44cf64d0cb')/$value
        system(query02, wait = TRUE);
        setwd(wd)
        if(i<numresults1){
          base::message(paste("...completed downloading",x,"image for",img.date,"moving to next image"))
        } else {
          base::message(paste("...completed downloading",x,"image for",img.date,"all done, moving to next tile"))
        }
      } else {
        if(i<numresults1){
          base::message(paste("...already had",x,"image for",img.date,"moving to next image"))
        } else {
          base::message(paste("...already had",x,"image for",img.date,"all done, moving to next tile"))
        }
        
      };
    }
  }
};
print("dwnld.imgs - successfully loaded")
preprocess.sentinel <- function(x) {
  #d.dir <- get("d.dir", envir = .GlobalEnv);
  files.zip <- list.files(x,pattern = "\\.zip",include.dirs = FALSE);
  suppressWarnings(dir.create(paste0(x,"/unzipped")));
  suppressWarnings(dir.create(paste0(x,"/maps")));
  suppressWarnings(dir.create(paste0(x,"/ready")));
  if (length(list.dirs(paste0(x,"/unzipped"),recursive = FALSE))==0) {
    unzip(dir = x,fn = files.zip)
    if (length(list.dirs(paste0(x,"/unzipped"),recursive = FALSE))==1) {
      sen2cor(dir = x)
    }
  } else if (length(list.dirs(paste0(x,"/unzipped"),recursive = FALSE))==1) {
    unlink(list.dirs(paste0(x,"/unzipped"),recursive = FALSE,full.names = TRUE),recursive = TRUE);
    unzip(dir = x,fn = files.zip)
    if (length(list.dirs(paste0(x,"/unzipped"),recursive = FALSE))==1) {
      sen2cor(dir = x)
    }
  } else if (length(list.dirs(paste0(x,"/unzipped"),recursive = FALSE))==2) {
    files.READY <- list.dirs(path=paste0(x,"/unzipped"), recursive = FALSE, full.names = TRUE);
    files.READY <- files.READY[grepl("MSIL2A",files.READY)];
    files <- list.files(files.READY, pattern = "._10m.jp2$", recursive = TRUE);
    if (length(files) < 7){
      unlink(files.READY, recursive = TRUE);
      unzip(dir = x,fn = files.zip)
      sen2cor(dir = x)
    }
    print(paste0(x, " - Preprocessing Done"))
  }
  
};
print("preprocess.sentinel - successfully loaded");
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
    base::message(paste(t1,"- Converting stack to brick, this takes time..."));
    img01 <- brick(img01)
    base::message(paste(now(),"- Done - run time =",ceiling(difftime(now(),t1,units = "sec")),"seconds"));
    filename.rgb <- paste0(x,"/ready/",imageryname,"_rgb.tif");
    if (!file.exists(filename.rgb) | file.size(filename.rgb) < 1000 ) {
      t1 <- now();
      base::message(paste(t1,"Writing RGB raster to file, this takes time..."));
      writeRaster(img01, file= filename.rgb, format="GTiff", overwrite=TRUE)
      base::message(paste(now(),"- Done - run time =",ceiling(difftime(now(),t1,units = "sec")),"seconds"))
      };
    filename.ndvi <- paste0(x,"/ready/",imageryname,"_ndvi.tif")
    if (!file.exists(filename.ndvi) | file.size(filename.ndvi) < 1000) {
      t1 <- now();
      base::message(paste(t1,"- Creating NDVI image, this takes time..."));
      ndvi <- (img01[[4]] - img01[[1]]) / (img01[[4]] + img01[[1]]);
      base::message(paste(now(),"- Done - run time =",ceiling(difftime(now(),t1,units = "sec")),"seconds"));
      t1 <- now();
      base::message(paste(t1,"Writing NDVI raster to file, this takes time..."));
      writeRaster(x = ndvi,file= paste0(x,"/ready/",imageryname,"_ndvi.tif"), format = "GTiff", overwrite = TRUE);
      base::message(paste(now(),"- Done - run time =",ceiling(difftime(now(),t1,units = "sec")),"seconds"));
      }
    rm(t1,im01,ndvi)
    gc()
    gc()
    if (file.exists(filename.rgb) & file.exists(filename.ndvi)) {
      out.mess <- "This file indicates that preprocessing had been performed.  It is generated to ensure that further processing is not attempted on this folder.  Please delete this file, along with the 'unzipped'; 'ready' and 'maps' folders if you wish to rerun the preprocessing loop on this raw image folder"
      write.table(out.mess, file = paste0(x,"/ProcessingCompleted.inf"), row.names = FALSE, col.names = FALSE);
    }

  }
};
print("create.tifs - successfully loaded");
preprocess.fast <- function(dirs = "Sentinel Directories Requiring Preprocessing") {
  no_cores <- availableCores();
  if (!is.na(max.cores)) {no_cores <- max.cores}
  c0 <- parallel::makeCluster(no_cores,outfile="tmp/pre_parallel_debug_file.log")
  registerDoParallel(c0);  #8 cores works well for quad core processor (you can set it to as many cores as you like but the process of allocating task to all the individual cores you create increases the overall processing time when you add too many - there's a sweet spot)
  autoStopCluster(c0);
  getDoParWorkers();
  a2 <- seq_along(dirs);
  foreach(j=a2, .packages=c('lubridate','raster', 'rgdal', 'sp'), .export = ls(.GlobalEnv)) %dopar% {
    preprocess.sentinel(dirs[j])
    create.tifs(dirs[j])
  };
  stopCluster(c0);
  gc();
  gc();
};
print("preprocess.fast - successfully loaded");
done.files.mos <- function (x) {
  (any(grepl("ProcessingCompleted.inf",list.files(x))==TRUE)|any(grepl("ProcessingCompleted.txt",list.files(x))==TRUE))
};
print("done.files.mos - successfully loaded");
setup.create.mosaic <- function(x) {
  base::message("Creating Required Mosaics - Please Wait")
  mos.inf <- read.csv(paste0(w.dir,"/settings/mosaic.csv"), header = TRUE, stringsAsFactors = FALSE)
  sentinel.folds <- list.dirs(path = s.dir, recursive = FALSE);
  sentinel.folds <- sentinel.folds[!apply(sapply(mos.inf$mosaic.name,function(x){grepl(x,sentinel.folds)}),1,any)]
  
  for (i in seq_along(mos.inf$mosaic.name)){
    # i = 1
    dir.create(paste0(s.dir,"/",mos.inf$mosaic.name[i]),showWarnings = FALSE);
    mos.imgs <- paste0(s.dir,"/",strsplit(mos.inf$included.tiles[i],"-")[[1]]);
    sentinel.folds.mos <- sentinel.folds[sentinel.folds %in% mos.imgs];
    mos.db <- data.frame(matrix(nrow = 0,ncol = 2));
    colnames(mos.db) <- c("Date","Image");
    for (k in seq_along(sentinel.folds.mos)){
      # k = 1
      sentinel.dirs.tmp <- as.data.frame(list.dirs(sentinel.folds.mos[k], recursive = FALSE, full.names = TRUE));
      colnames(sentinel.dirs.tmp) <- "Image"
      sentinel.dirs.tmp$Date <- gsub(".+?[?=Sentinel_]","",sentinel.dirs.tmp$Image) # .+? will match any characters as few as possible until a "Sentinel_" is found, without counting the "Sentinel_".
      sentinel.dirs.tmp <- sentinel.dirs.tmp[,c(2,1)]
      mos.db <- rbind(mos.db,sentinel.dirs.tmp)
      rm(sentinel.dirs.tmp)
    };# END k Loop
    mos.db <- mos.db[unlist(lapply(as.character(mos.db$Image),done.files.mos)),]
    rm(k)
    img.dates <- unique(mos.db$Date);
    img.dates <- img.dates[order(img.dates)]
    base::message(paste("Creating",length(img.dates), "mosaic images for", mos.inf$mosaic.name[i]));
    sentinel.dirs.tmp <- list.dirs(paste0(s.dir,"/",mos.inf$mosaic.name[i]), recursive = FALSE);
    done.dates <- as.data.frame(gsub(".+?[?=Sentinel_]","",sentinel.dirs.tmp), stringsAsFactors=FALSE);
    colnames(done.dates) <- "Date"
    base::message(paste("Already have", length(done.dates$Date), "done"));
    done.dates$Dir <- sentinel.dirs.tmp;
    done.dates$Done <- unlist(lapply(sentinel.dirs.tmp,done.files.mos));
    done.dates$NumImgs <- NA;
    for (j in seq_along(done.dates$Done)){
      if(done.dates$Done[j]==TRUE){
        done.dates$NumImgs[j] <- as.numeric(gsub("[[:space:]]","",(gsub('"',"",(grep("[0-9]",noquote(readLines(paste0(done.dates$Dir,"/ProcessingCompleted.txt")[j])),value = TRUE))))))
      }
    }; # End i Loop
    rm(j);
    done.dates <- done.dates[done.dates$Done,];
    done.dates$NumImgs[is.na(done.dates$NumImgs)] <- 0;
    no_cores <- availableCores();
    if (!is.na(max.cores)) {no_cores <- max.cores}
    c1 <- parallel::makeCluster(no_cores,outfile="tmp/mosaic_parallel_debug_file.log")
    registerDoParallel(c1);
    autoStopCluster(c1)
    a2 <- seq_along(img.dates);
    foreach(j=a2, .packages=c('raster', 'rgdal', 'sp'), .export = ls(.GlobalEnv)) %dopar% {
      if (img.dates[j] %in% done.dates$Date) {
        if (sum(mos.db$Date==img.dates[j],na.rm = TRUE) <= done.dates$NumImgs[done.dates$Date %in% img.dates[j]]){
          print(paste("No new imagery for", img.dates[j], "- Not creating new mosaic"));
        } else {
          print(paste("New imagery available for", img.dates[j], "- Creating new mosaic"));
          unlink(paste0(s.dir,"/",mos.inf$mosaic.name[i],"/Sentinel_",img.dates[j]),recursive = TRUE);
          dir.out <- paste0(s.dir,"/",mos.inf$mosaic.name[i],"/Sentinel_",img.dates[j],"/ready");
          dir.create(dir.out,showWarnings = FALSE, recursive = TRUE);
          mos.imgs.tmp <- paste0(as.character(mos.db$Image[mos.db$Date==img.dates[j]]),"/ready");
          mosaic.create(mos = mos.imgs.tmp,dir = dir.out)
        };
      } else {
        dir.out <- paste0(s.dir,"/",mos.inf$mosaic.name[i],"/Sentinel_",img.dates[j],"/ready");
        dir.create(dir.out,showWarnings = FALSE, recursive = TRUE);
        mos.imgs.tmp <- paste0(as.character(mos.db$Image[mos.db$Date==img.dates[j]]),"/ready");
        mosaic.create(mos = mos.imgs.tmp,dir = dir.out)
      };
    };
    stopCluster(c1);
    gc();
    gc();
  };
};
print("setup.create.mosaic - successfully loaded");
mosaic.create <- function(mos = "Mosaic Images", dir = "Output Directory") {
  ndvi.tifs <- list.files(mos);
  ndvi.nam <- unique(ndvi.tifs[grepl("_ndvi.tif$",ndvi.tifs)]);
  rgb.nam <- unique(ndvi.tifs[grepl("_rgb",ndvi.tifs)]);
  ndvi.imgs <- paste0(mos,"/",ndvi.nam);
  rgb.imgs <- paste0(mos,"/",rgb.nam);
  dir1 <- sub("/ready","",dir)
  if(length(ndvi.imgs)==1) {
    # For Linux: using cp command
    sauce1 <- ndvi.imgs[1];
    target1 <- paste0(dir,"/",ndvi.nam)
    comm.cp1 <- paste0("cp ", sauce1, " ", target1);
    copied1 <- system(comm.cp1, wait = TRUE);
    print(paste("Copied 1 =",copied1))
    sauce2 <- rgb.imgs[1];
    target2 <- paste0(dir,"/",rgb.nam)
    comm.cp2 <- paste0("cp ", sauce2, " ", target2);
    copied2<- system(comm.cp2, wait = TRUE);
    print(paste("Copied 2 =",copied2))
    if (copied1 == 0 & copied2 == 0) {
      out.mess <- paste0("This file indicates that preprocessing had been performed.  It is generated to ensure that further processing is not attempted on this folder.  Please delete this file, along with the 'unzipped'; 'ready' and 'maps' folders if you wish to rerun the preprocessing loop on this raw image folder
        ",length(mos));
      write.table(out.mess, file = paste0(dir1,"/ProcessingCompleted.txt"), row.names = FALSE, col.names = FALSE);
    };
  } else {
    ndvi.imgs <- lapply(ndvi.imgs, raster);
    #names(ndvi.imgs)[1:2] <- c('x','y');
    names(ndvi.imgs) <- NULL;
    ndvi.imgs$fun <- mean;
    #ndvi.imgs$tolerance <- 0.05;
    ndvi.imgs$filename <- paste0(dir,"/",ndvi.nam);
    ndvi.imgs$overwrite <- TRUE;
    do.call(mosaic,ndvi.imgs);
    rm(ndvi.imgs);
    gc();
    gc();
    rgb.imgs <- lapply(rgb.imgs, raster);
    names(rgb.imgs) <- NULL;
    rgb.imgs$fun <- mean;
    rgb.imgs$filename <- paste0(dir,"/",rgb.nam);
    rgb.imgs$overwrite <- TRUE;
    do.call(mosaic,rgb.imgs);
    rm(rgb.imgs);
    gc();
    gc();
    out.mess <- paste0("This file indicates that preprocessing had been performed.  It is generated to ensure that further processing is not attempted on this folder.  Please delete this file, along with the 'unzipped'; 'ready' and 'maps' folders if you wish to rerun the preprocessing loop on this raw image folder
        ",length(mos));
    write.table(out.mess, file = paste0(dir1,"/ProcessingCompleted.txt"), row.names = FALSE, col.names = FALSE);
  }
}
print("mosaic.create - successfully loaded")
####END SCRIPT####


