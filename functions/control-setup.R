######################################################################################
################################control-setup.R#######################################
######################################################################################

####User Defined Functions####

source(file = "functions/load-libraries.R");

print("Loadings settings file....");
source(file = "settings/settings.R");

print("Loading user defined functions...");
source(file = "functions/setup.R");

source(file = "functions/image-download.R")

####Execute Processing####

# Load Libraries
print("Loading required packages...");
loadlibraries(pkg = list.of.packages);
rm(list.of.packages,loadlibraries);

# Setup Directories
print("Setting up directories...");
if (exists("cld.dir")) {d.dir <- cld.connect()};
sapply(paste0(d.dir,"\\sentinelimages\\T",tiles),create.dirs)
dir.create(paste0(d.dir,"\\treemasks"),recursive = TRUE,showWarnings = FALSE)
dir.create(paste0(d.dir,"\\shapefiles"),recursive = TRUE,showWarnings = FALSE)
dir.create(paste0(d.dir,"\\calibrationdata"),recursive = TRUE,showWarnings = FALSE)
####END SCRIPT####