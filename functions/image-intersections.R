######################################################################################
##############################image-intersections.R###################################
######################################################################################

####User defined functions####
test_intersection <- function(a,b,c){
  out <- tryCatch(
    {
      message("Double checking layer intersection")
      #length(crop(a,b))>0;
      #mask(a,b);
      ndvi1 <- crop(a,b);
      mask(ndvi1,c);
      return(TRUE);
    },
    error=function(cond) {
      message(paste("Shapefile doesn't intersect ndvi raster"))
      # Choose a return value in case of error
      return(FALSE)
    },
    finally={
      
    }
  )    
  return(out)
};
message("test_intersection - successfully loaded!")
test_intersection2 <- function(a,b){
  out <- tryCatch(
    {
      message("Checking layer intersection")
      #length(crop(a,b))>0;
      #mask(a,b);
      ndvi1 <- crop(a,b);
      #mask(ndvi1,c);
      message("Success - layers intersect!")
      return(TRUE);
    },
    error=function(cond) {
      message(paste("Shapefile doesn't intersect ndvi raster"))
      # Choose a return value in case of error
      return(FALSE)
    },
    finally={
      
    }
  )    
  return(out)
};
message("test_intersection2 - successfully loaded!");
done.files <- function (x) {
  
  any(grepl("ProcessingCompleted.txt",list.files(x))==TRUE)
  
};
message("done.files - successfully loaded!")

####END SCRIPT####