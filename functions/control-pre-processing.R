######################################################################################
############################control-pre-processing.R##################################
######################################################################################

####Load functions####

print("Loading user defined functions...");

source(file = "functions/preprocess-images.R");

####Execute Processing####

# Download new images
sapply(tiles,dwnld.imgs);

# Preprocess Sentinel-1C product to 2A
sentinel.dirs <- sen.folds(ss = s.dir);
if(length(sentinel.dirs)>0 & !pre.fast){
  sapply(sentinel.dirs,preprocess.sentinel)
  sapply(sentinel.dirs,create.tifs)
  }
if(length(sentinel.dirs)>0 & pre.fast){
  preprocess.fast(dirs = sentinel.dirs)
};

# Clean up 
if (exists("cld.dir")) {cld.disconnect()};
gc();
gc();
print(paste(Sys.time(),"- Pre-processing Complete..."))

####END SCRIPT####