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

# Preprocess Sentinel-1C product to 2A
base::message("Checking what new imagery needs preprocessing with Sen2Cor...")
sentinel.dirs <- sen.folds(ss = s.dir);
if (Interactive){
  print("Would you like to preprocess available imagery? [yes/no]");
  con <- if (interactive()) stdin() else file('stdin');
  preimg <- scan(file=con, sep=',', nlines=1, what = 'character', quiet=TRUE);
  preimg <- tolower(preimg)
  if (preimg == 'yes' | preimg == 'y' | preimg == 'yeah' | preimg == 'yep' | preimg == 'true') {
    base::message("Preprocessing new imagery...");
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

# Clean up 
gc();
gc();
print(paste(Sys.time(),"- Pre-processing Complete..."))

####END SCRIPT####