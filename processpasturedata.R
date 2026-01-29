######################################################################################
##############################processpasturedata.R####################################
######################################################################################

####Welcome Message####

print("Welcome to the pasture monitoring processor");
Sys.sleep(3);

####Load Packages####
source(file = "functions/load-libraries.R");
print("Loading required packages...");
loadlibraries(pkg = list.of.packages);
rm(list.of.packages,loadlibraries);

####Initiate Processor####

tryCatchLog(
  {
    source(file = "functions/control-setup.R")
    source(file = "functions/control-pre-processing.R");
    source(file = "functions/control-setup-4-post-processing.R");
    source(file = "functions/control-post-processing.R");
  },
  error=function(cond){
    base::message("Here is the error message:");
    base::message(cond)
  },
  warning=function(cond){
    base::message("Here are the warning messages:");
    base::message(cond)
  },
  finally={
    rm(list=ls());
    gc();
    gc();
    print("Processing Completed...")
    # For Linux: using system bell or speaker-test
    if (system("command -v beep > /dev/null 2>&1", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0) {
      system("beep -f 1000 -l 500", wait=FALSE)
    } else {
      # Fallback: use printf to print the bell character
      system("printf '\\a\\a\\a'", wait=FALSE)
    }
    Sys.sleep(5)
    #quit(save = "no")
  }

)

####END SCRIPT####  
