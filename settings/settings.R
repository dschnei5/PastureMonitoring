######################################################################################
###################################settings.R#########################################
######################################################################################

Interactive <- TRUE # Set TRUE for: 
                    # - the 1st run ever on this machine
                    # - adding new shapefiles  
                    # - selecting individual properties to process
                    # - reprocessing select image dates for a property with a new calibration
                    # - redeploying individual shiny apps
                    # - adding a new tile to the list and treemask generation
                    # - new calibration generation
                    # - updating an existing password or changing a username or account
                    # - farm/property metadata editing
                    # Set FALSE for: 
                    # - auto, non-interactive, deployment 

####Set Directories####

w.dir <- "/home/user/R/PastureMonitoring"; # Local Working Directory (required) - adjust /home/user to your home directory

setwd(w.dir);

d.dir <- "/home/user/R/PastureMonitoringData"; # Local Data Directory (hash out if cloud.dir used) - adjust /home/user to your home directory

# cld.dir <- "/mnt/cloud/Data/PastureMonitoringData"; # Cloud Directory (un-hash if needed) - adjust /mnt/cloud to your mount point

# cld.mnt.pt <- "/mnt/cloud"; # Cloud mount point for Linux (required if using cloud directory)

# cld.mnt.pt.s <- "/mnt/sentinel"; # Sentinel cloud mount point for Linux (required if using cloud directory)

s.dir <- "/home/user/R/PastureMonitoringSentinel" # Location of Sentinel Images (hash out if cloud.s.dir used) - adjust /home/user to your home directory

# cld.s.dir <- "/mnt/sentinel/Data/SentinelImages"; # Cloud Sentinel Directory (un-hash if needed) - adjust /mnt/sentinel to your mount point

####Set Accounts####

# cld.usr <- "username"; # Cloud drive username (if applicable) - for Linux SMB mounts

# cld.ser.nm <- "cloud_share"; # Cloud service identifier (if applicable) - not needed for Linux

# cld.s.usr <- "username"; # Cloud Sentinel drive username (if applicable) - for Linux SMB mounts

# cld.s.ser.nm <- "sentinel_share"; # Cloud Sentinel service identifier (if applicable) - not needed for Linux

cop.usr <- "XXXXXXX"; # Copernicus open access hub username (required)

master.app <- "/XxxxxxXXXXX_YXXxXXxY"

avail.app.accts <- c("xxxxx","xxxxx"); # Master Shiny apps account (required)

master.app.acct <- "xxxxx"

####Other Settings####

tiles <- c("56JLL","56JLN"); # The sentinel tiles to download (required)

cld.pc <- 5; # percentage of cloud allowed for image download (required)

numdaysback <- 10; # number of days into past to look for imagery, sentinel collected every 5 days (required)

post.fast <- FALSE; # Use parallel computing to perform post-processing when running processor not interactively (required)

pre.fast <- FALSE; # Use parallel computing to perform pre-processing when running processor not interactively (required)

post.skip <- TRUE; #Skip previously post-processed imagery for all farms when running processor not interactively (TRUE/FALSE - required) 

delete.leftovers <- FALSE # Clean up SentinelImages in storage location to save space (required)

create.mosaics <- FALSE # Use "TRUE" if you have farms that overlap imagery boundaries. Edit "mosaic.inf" in settings folder

max.cores <- NA; # Set numeric to limit number of parallel compute cores used for pre and post processing, NA available cores are detected

####Email Out Settings####

this_hw <- "New Imagery Available on Pastures App" # Email Subject Line

email_sender <- 'Xxxx Xxxxxxx <xxxxxxx@gmail.com>' # Name and Email address of sender

optional_bcc <- 'Xxxx Xxxxxxx <xxxxxxx@gmail.com>' # Name and Email address of BCC recipient

eo.appname <- "send-mail-from-r" # GMail appname

# key_set(service = eo.appname, username = "pastureapp",prompt = "Enter your GMailr Application Key")
# key_set(service = paste0(eo.appname,"-secret"), username = "pastureapp",prompt = "Enter your GMailr Application Secret")

eo.key <- key_get(service = eo.appname, username = "pastureapp") # GMail application key

eo.secret <- key_get(service = paste0(eo.appname,"-secret"), username = "pastureapp") # GMail secret 

####END SCRIPT####