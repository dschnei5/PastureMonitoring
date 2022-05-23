######################################################################################
################################image-download.R######################################
######################################################################################

####User Defined Functions####
check4pw <- function(usr = "Username", ser = "Service"){
  klst <- key_list(service = ser)
  if (length(klst$service)==0) {
    con <- if (interactive()) stdin() else file('stdin');
    message(paste0("Please enter your password for ",ser,":"))
    psswrd <- scan(file=con, sep=',', nlines=1, what = 'character', quiet=TRUE);
    key_set_with_value(service = ser,username = usr ,password = psswrd);
  } else {
    klst <- klst[klst$username==usr,]
    if (length(klst$service)==0){
      con <- if (interactive()) stdin() else file('stdin');
      message(paste0("Please enter your password for ",ser,":"));
      psswrd <- scan(file=con, sep=',', nlines=1, what = 'character', quiet=TRUE);
      key_set_with_value(service = ser,username = usr ,password = psswrd);
    } else {
      psswrd <- key_get(service = ser, username = usr)
    }
  }
  return(psswrd)
};
message("check4pw - successfully loaded");
cld.connect <- function() {
  message(paste("Connecting to cloud drive:", cld.dir))
  pwd <- check4pw(usr = cld.usr, ser = cld.ser.nm)
  cmd1 <- paste0("net use ",drv.l,": \\\\",cld.dir," /user:",cld.usr," ",pwd)
  system(cmd1, wait=TRUE)
  d.dir <- paste0(drv.l,":")
};
message("cld.connect - successfully loaded");
cld.disconnect <- function() {
  message(paste("Disconnecting from cloud drive:", cld.dir))
  cmd2 <- paste0("net use ",d.dir," ", "/delete");
  system(cmd2, wait=TRUE)
};
message("cld.disconnect - successfully loaded");
create.dirs <- function(dir = "Data Directory"){
  dir.create(dir,showWarnings = FALSE, recursive = TRUE)
}
message("create.dirs - successfully loaded");
dwnld.imgs <- function(x = "tile"){
  pw <- check4pw(usr = cop.usr,ser = "Copernicus")
  paste0("Checking last ", numdaysback," for imagery with <", cld.pc,"% cloud for tile ",x)
  query01 <- paste0("wget --no-check-certificate --user=",cop.usr," --password=",pw," --auth-no-challenge --output-document=tmp//query_results.txt \"https://scihub.copernicus.eu/dhus/search?q=",x," AND producttype:S2MSI1C AND cloudcoverpercentage:[0 TO ",cld.pc,"] AND endposition:[NOW-",numdaysback,"DAYS TO NOW]&format=json\"");
  system(query01, wait = TRUE)
  info <- jsonlite::fromJSON(txt = "tmp//query_results.txt")
  numresults1 <- info$feed$`opensearch:totalResults` 
  img.dates <- info$feed$entry$summary
  img.href <- info$feed$entry$link
  message(paste("Number of cloud free images found within the last", numdaysback,"days =",numresults1))
  if (numresults1 > 0) {
    print(paste("Downloading new images for", x));
    for (i in 1:numresults1){
      print(i)
      UUID <- img.href[[i]][[1]][1];
      img.date <- strapplyc(strsplit(img.dates[i],",")[[1]][1],"[0-9]{4}-[0-9]{2}-[0-9]{2}", simplify = TRUE)
      wd <- getwd()
      new.wd <- paste0(d.dir,"/SentinelImages/T",x,"/Sentinel_",img.date)
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
          message(paste("...completed downloading",x,"image for",img.date,"moving to next image"))
        } else {
          message(paste("...completed downloading",x,"image for",img.date,"all done, moving to next tile"))
        }
      } else {
        if(i<numresults1){
          message(paste("...already had",x,"image for",img.date,"moving to next image"))
        } else {
          message(paste("...already had",x,"image for",img.date,"all done, moving to next tile"))
        }
        
      };
    }
  }
};
message("dwnld.imgs - successfully loaded")

####END SCRIPT####