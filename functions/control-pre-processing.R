######################################################################################
############################control-pre-processing.R##################################
######################################################################################

####Load functions####

print("Loading user defined functions...");

source(file = "functions/load-libraries.R");

source(file = "functions/image-download.R");

source(file = "functions/preprocess-images.R");

source(file = "functions/image-intersections.R");

source(file = "functions/app-deploy.R");

####Execute Processing####

# Load Libraries
print("Loading required packages...");
loadlibraries(pkg = list.of.packages);
rm(list.of.packages,loadlibraries);

# Setup Directories
print("Setting up directories...");
if (exists("cld.dir")) {d.dir <- cld.connect()};
sapply(paste0(d.dir,"\\SentinelImages\\T",tiles),create.dirs)
dir.create(paste0(d.dir,"\\TreeMasks"),recursive = TRUE,showWarnings = FALSE);

# Download new images
sapply(tiles,dwnld.imgs);

# Preprocess Sentinel-1C product to 2A
sentinel.dirs <- sen.folds(d = d.dir);
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
print("Preprocessing Completed.... Auto Close in 5 seconds")
####END SCRIPT####