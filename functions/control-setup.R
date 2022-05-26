######################################################################################
################################control-setup.R#######################################
######################################################################################

####User Defined Functions####

print("Loadings settings file....");
source(file = "settings/settings.R");

flog.appender(appender.file(paste0(w.dir,"/tmp/processor.log")))
flog.threshold('WARNING')

print("Loading user defined functions...");
source(file = "functions/setup.R");

source(file = "functions/image-download.R")

####Execute Processing####

# Setup Directories
print("Setting up directories...");
if (exists("cld.dir")) {d.dir <- cld.connect()};
sapply(paste0(s.dir,"\\T",tiles),create.dirs)
sapply(paste0(d.dir,"\\",c("treemasks","shapefiles","calibrationdata")),create.dirs)

####END SCRIPT####