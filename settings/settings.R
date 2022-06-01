######################################################################################
###################################settings.R#########################################
######################################################################################

Interactive <- FALSE # Set TRUE for: 
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

w.dir <- "C:/R/PastureMonitoring"; # Local Working Directory (required)

setwd(w.dir);

d.dir <- "C:/R/PastureMonitoringData"; # Local Data Directory (hash out if cloud.dir used)

# cld.dir <-"cloud.une.edu.au\\parg\\dschnei5\\Precision7520_Sync\\R\\PastureMonitoringData"; # Cloud Directory (un-hash if needed) 

# drv.l <- "z"; # Drive letter to temporarily allocate to cloud directory on mount (required if using cloud directory)

s.dir <- "C:/R/PastureMonitoringSentinel" # Location of Sentinel Images

####Set Accounts####

cld.usr <- "UNE\\dschnei5"; # Cloud drive username (if applicable)

cld.ser.nm <- "Cloud.UNE"; # Cloud service name (if applicable)

cop.usr <- "dschnei5"; # Copernicus open access hub username (required)

master.app <- "/PastureApp_eh3kTu23W2d0i4Y"

avail.app.accts <- c("une-ers","parg"); # Master Shiny apps account (required)

master.app.acct <- "parg"

####Other Settings####

tiles <- c("56JLL","56JLN"); # The sentinel tiles to download (required)

cld.pc <- 5; # percentage of cloud allowed for image download (required)

numdaysback <- 10; # number of days into past to look for imagery, sentinel collected every 5 days (required)

post.fast <- FALSE; # Use parallel computing to perform post-processing when running processor not interactively (required)

post.skip <- TRUE; #Skip previously post-processed imagery for all farms when running processor not interactively (TRUE/FALSE - required) 

delete.leftovers <- FALSE # Clean up SentinelImages in storage location to save space (required)

####Email Out Settings####

this_hw <- "New Imagery Available on Pastures App" # Email Subject Line
email_sender <- 'Dez Schneider <dez.schneider@gmail.com>' # Name and Email address of sender
optional_bcc <- 'Dez Schneider <dschnei5@une.edu.au>' # Name and Email address of BCC recipient
eo.appname <- "send-mail-from-r" # GMail appname
# key_set(service = eo.appname, username = "pastureapp",prompt = "Enter your GMailr Application Key")
# key_set(service = paste0(eo.appname,"-secret"), username = "pastureapp",prompt = "Enter your GMailr Application Secret")
eo.key <- key_get(service = eo.appname, username = "pastureapp") # GMail application key
eo.secret <- key_get(service = paste0(eo.appname,"-secret"), username = "pastureapp") # GMail secret 

####END SCRIPT####