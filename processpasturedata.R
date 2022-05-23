######################################################################################
##############################processpasturedata.R####################################
######################################################################################

####Welcome Message####

print("Welcome to the pasture monitoring processor");
Sys.sleep(3);

tryCatch(
  {
    source(file = "settings/settings.R");
    source(file = "functions/control-pre-processing.R");
    source(file = "functions/control-post-processing.R");
  },
  error=function(cond){
    message("Here is the error message:");
    message(cond)
  },
  warning=function(cond){
    message("Here are the warning messages:");
    message(cond)
  },
  finally={
    rm(list=ls());
    gc();
    gc();
    print("Processing Completed...")
    beep(sound = 3);
    Sys.sleep(5)
    quit(save = "no")
  }


)
  


####END SCRIPT####