######################################################################################
############################control-pre-processing.R##################################
######################################################################################

####Load functions####

print("Loading user defined functions...");

source(file = "functions/preprocess-images.R");

####Execute Processing####

# Download new images
if (Interactive){
  print("Would you like to check for new imagery? [yes/no]");
  con <- if (interactive()) stdin() else file('stdin');
  newimg <- scan(file=con, sep=',', nlines=1, what = 'character', quiet=TRUE);
  newimg <- tolower(newimg)
  if (newimg == 'yes' | newimg == 'y' | newimg == 'yeah' | newimg == 'yep' | newimg == 'true') {
    base::message("Checking for new imagery to download...");
    sapply(tiles,dwnld.imgs);
  } else {
    base::message("Skipping new imagery download...")
  }
  rm(con,newimg);
} else {
  sapply(tiles,dwnld.imgs);
};

# Preprocess Sentinel-2 L2A products (unzip only, no sen2cor needed)
base::message("Checking for new L2A imagery that needs unzipping...")
sentinel.dirs <- sen.folds(ss = s.dir);
if (Interactive){
  print("Would you like to preprocess available imagery? [yes/no]");
  con <- if (interactive()) stdin() else file('stdin');
  preimg <- scan(file=con, sep=',', nlines=1, what = 'character', quiet=TRUE);
  preimg <- tolower(preimg)
  if (preimg == 'yes' | preimg == 'y' | preimg == 'yeah' | preimg == 'yep' | preimg == 'true') {
    base::message("Unzipping new L2A imagery...");
    if(length(sentinel.dirs)>0 & !pre.fast){
      sapply(sentinel.dirs,preprocess.sentinel)
      sapply(sentinel.dirs,create.tifs)
    }
    if(length(sentinel.dirs)>0 & pre.fast){
      preprocess.fast(dirs = sentinel.dirs)
    };
  } else {
    base::message("Skipping new imagery preprocessing...")
  }
  rm(con,preimg);
} else {
  if(length(sentinel.dirs)>0 & !pre.fast){
    sapply(sentinel.dirs,preprocess.sentinel)
    sapply(sentinel.dirs,create.tifs)
  }
  if(length(sentinel.dirs)>0 & pre.fast){
    preprocess.fast(dirs = sentinel.dirs)
  };
};

#Create Image Mosaics

if (Interactive){
  print("Would you like to create mosaics? [yes/no]");
  con <- if (interactive()) stdin() else file('stdin');
  mosimg <- scan(file=con, sep=',', nlines=1, what = 'character', quiet=TRUE);
  mosimg <- tolower(mosimg)
  if (mosimg == 'yes' | mosimg == 'y' | mosimg == 'yeah' | mosimg == 'yep' | mosimg == 'true') {
    base::message("Creating mosaic imagery...");
    base::message(paste("Create Mosaics Set To:",create.mosaics))
    if (create.mosaics) {
      setup.create.mosaic()
    }
  } else {
    base::message("Skipping mosaic creation...")
  }
} else {
  if (create.mosaics) {
    setup.create.mosaic()
  }
} 

# Clean up 
gc();
gc();
print(paste(Sys.time(),"- Pre-processing Complete..."))

####END SCRIPT####
