######################################################################################
##############################image-intersections.R###################################
######################################################################################

####User defined functions####
{
  test_intersection <- function(a,b,c){
    out <- tryCatch(
      {
        print("Double checking layer intersection")
        #length(crop(a,b))>0;
        #mask(a,b);
        ndvi1 <- crop(a,b);
        raster::mask(ndvi1,c);
        return(TRUE);
      },
      error=function(cond) {
        base::message(paste("Shapefile doesn't intersect ndvi raster"))
        # Choose a return value in case of error
        return(FALSE)
      },
      finally={
        
      }
    )    
    return(out)
  };
  print("test_intersection - successfully loaded!");
  setup.post.dirs <- function(propmeta = "Propmeta", x = "Property Name") {
    propmeta.i <- propmeta[which(x == propmeta$Property),]
    ("Setting up directories...");
    out.dir <- paste0(d.dir,propmeta.i$OutLoc);
    dir.create(paste0(out.dir,"/Maps"), showWarnings = FALSE);
    dir.create(paste0(out.dir,"/GeoJson"), showWarnings = FALSE);
    dir.create(paste0(d.dir,propmeta.i$AppFile,"/",x), showWarnings = FALSE);
    dir.create(paste0(d.dir,propmeta.i$AppFile,"/",x,"/GeoJson"), showWarnings = FALSE);
    if (!is.na(propmeta.i$AppFile2nd)) {
      dir.create(paste0(d.dir,propmeta.i$AppFile2nd,"/",x), showWarnings = FALSE);
      dir.create(paste0(d.dir,propmeta.i$AppFile2nd,"/",x,"/GeoJson"), showWarnings = FALSE);
    }
    dir.create(paste0(d.dir,"/ShinyApps",master.app,"/www/DataOut/",x), showWarnings = FALSE);
    dir.create(paste0(d.dir,"/ShinyApps",master.app,"/www/DataOut/",x,"/GeoJson"), showWarnings = FALSE);
    return(propmeta.i)
  }
  print("setup.post.dirs - successfully loaded!");
  img.nms.dtes <- function(propmeta = "Propmeta", x = "Property Name", out.dir, skip) {
    propmeta.i <- propmeta[which(x == propmeta$Property),]
    sentinel.dirs <- list.dirs(path = paste0(s.dir,"/",propmeta.i$Sentinel), recursive = FALSE);
    sentinel.dirs <- sentinel.dirs[grepl("Sentinel_",sentinel.dirs)];
    a <- nchar(sentinel.dirs[1]) - 9
    b <- nchar(sentinel.dirs[1]) * 1
    splithim <- function(q){substr(q,a,b)}; #Values change depending on directory name length
    img.dates <- ymd(sapply(sentinel.dirs, splithim, USE.NAMES = FALSE));
    rm(a,b);
    img.names <- character();
    done1 <- unlist(lapply(sentinel.dirs,done.files))
    img.dates <- img.dates[done1]
    sentinel.dirs <- sentinel.dirs[done1]
    for (k in seq_along(sentinel.dirs)){
      img.name <- list.dirs(sentinel.dirs[k], full.names = TRUE, recursive = FALSE)
      img.name <- img.name[grepl("/ready$",img.name)];
      img.name <- list.files(img.name, full.names = TRUE);
      img.name <- sub(".ovr","KILL",img.name);
      img.name <- sub(".xml", "KILL",img.name);
      img.name <- img.name[!grepl("KILL",img.name)];
      img.name <- img.name[1];
      img.names <- c(img.names,img.name);
    }; # END k Loop
    rm(k);
    img.dates <- img.dates[complete.cases(img.names)];
    img.names <- img.names[complete.cases(img.names)];
    if (skip){
      already.done <- list.files(path = paste(out.dir), pattern = ".csv", recursive = FALSE);
      already.done <- already.done[!grepl(pattern = "AllAvailableDates",already.done,ignore.case = FALSE)];
      already.done <- as.Date(unlist(strapplyc(already.done, "\\d+-\\d+-\\d+", simplify = TRUE)));
      img.names <- img.names[!img.dates %in% already.done];
      img.dates <- img.dates[!img.dates %in% already.done];
      rm(already.done);
    }
    return(list(img.names,img.dates))
  }
  print("img.nms.dtes - successfully loaded!");
  post.processor <- function(x) {
    t1 <- now()
    print(paste(now(),"- Processing of", x, "started"));
    propmeta.i <- setup.post.dirs(propmeta, x);
    out.dir <- paste0(d.dir,propmeta.i$OutLoc);
    if (!dir.exists(paste0(out.dir,"/GeoJson"))) {suppressWarnings(dir.create(paste0(out.dir,"/GeoJson"),recursive = TRUE))};
    image.names.dates <- img.nms.dtes(propmeta, x, out.dir,skip = skip);
    ####INSERT CALL TO FUNCTION FOR SELECTING CERTAIN IMAGERY ONLY HERE#### 
    img.names <- image.names.dates[[1]]
    img.dates <- image.names.dates[[2]]
    rm(image.names.dates)
    if (length(img.names) == 0) {
      base::message(paste("No new imagery to process for", x, ". Moving to next property..."));
      Sys.sleep(2);
    } else {
      base::message(paste("There are ", length(img.dates), "images to process for", x));
      boundary1 <- shps[grepl(propmeta.i$Shapefile,names(shps))][[1]];
      crs(boundary1) <- UCRS[[propmeta.i$CRS.no]];
      dta.out.full <- data.frame(aPADD_NAME = boundary1@data$PADD_NAME);
      dta.out.full$aPADD_NAME <- make.names(dta.out.full$aPADD_NAME, unique = TRUE);
      dta.out.full$aPAS_TYP <- boundary1@data$PAS_TYP;
      if (class(boundary1)[1]=="SpatialPolygonsDataFrame") {
        dta.out.full$bHECTARES <- as.numeric(raster::area(boundary1)/10000);
      };
      if (class(boundary1)[1]=="SpatialPointsDataFrame") {
        dta.out.full$bHECTARES <- NA;
        dta.out.full$bTREED_HA <- NA;
        dta.out.full$bTREED_PERC <- NA;
      };
      treemask1 <- raster(paste0(d.dir,propmeta.i$TreeMask));
      dims1 <- extent(boundary1);
      treemask1 <- crop(treemask1,dims1);
      if (class(boundary1)[1]=="SpatialPolygonsDataFrame") {
        dta.out.full$bTREED_HA <- ((extract(treemask1,boundary1, fun = sum, na.rm = TRUE))*0.0000255)[,1]; #Add up number of 10m2 255 coded pixels and convert area to hectares
        dta.out.full$bTREED_PERC <- dta.out.full$bTREED_HA/dta.out.full$bHECTARES*100;
      };
      for (i in seq_along(img.dates)){
        t2 <- now()
        print(paste("Processing", x, "Image:", i, "- Date:", img.dates[i], "- Start Time:", now()));
        ndvi <- stack(img.names[i]);
        ndvi.crp1 <- raster::intersect(extent(ndvi), dims1);
        #test_intersection <- get("test_intersection", envir = .GlobalEnv);
        if (class(boundary1)[1]=="SpatialPolygonsDataFrame") {
          ndvi.crp2 <- test_intersection(ndvi,dims1,treemask1);
        } else {ndvi.crp2 <- FALSE};
        if (length(ndvi.crp1) == 0 | ndvi.crp2 == FALSE | class(boundary1)[1]=="SpatialPointsDataFrame") {
          base::message("No overlap or it's a point file, moving on...")
          if(class(boundary1)[1]=="SpatialPointsDataFrame") {
            base::message("Ahah! it's a point file...")
            Sys.sleep(1);
            base::message("Performing spatial intersection...")
            treemask2 <- raster(paste0(d.dir,propmeta.i$TreeMask));
            treemask2 <- crop(treemask2,ndvi);
            values(treemask2) <- ifelse(values(treemask2) == 255, NA, values(ndvi));
            ndvi.masked1 <- raster::mask(ndvi,treemask2);
            x11();
            plot(treemask2);
            plot(boundary1, add = TRUE);
            crs(ndvi.masked1) <- UCRS[[propmeta.i$CRS.no]];
            dta.out <- extract(ndvi.masked1,boundary1, buffer = 10, fun = mean, na.rm = TRUE);
            suppressWarnings(rm(ndvi.masked1,ndvi,ndvi.crp1,ndvi1,treemask2));
            gc();
            gc();
            l.yeah <- dta.out;
            dta.out <- data.frame(NDVI = dta.out);
            pmods.ls <- load.pm()
            pmods.ls <- pmods.ls[!grepl("ACS211NDVI-SentinelNDVI.rda",pmods.ls)]
            dta.out[,2:(length(pmods.ls)+1)] <- dta.out$NDVI;
            colnames(dta.out) <- c("NDVI",sub(".glm",".CC_NDVI",(ls(.GlobalEnv)[grepl(".glm",ls(.GlobalEnv))])));
            glm.ls <- (ls('.GlobalEnv')[grepl(".glm",ls('.GlobalEnv'))]);
            if (class(l.yeah)!= "logical") {
              print("It's not a logical vector, predicting GDM...")
              for (k in seq_along(pmods.ls)){
                #k = 1
                #print(environment())
                base::message("doing the predicting...")
                dta.out[,(k+1)] <- predict(get(glm.ls[k]),dta.out,type='response');
                Sys.sleep(1)
                ##print(dta.out)[,k+1]
              }; #END k loop
              rm(k)};
            colnames(dta.out) <- c("NDVI",sub(".glm","",(ls(.GlobalEnv)[grepl(".glm$",ls(.GlobalEnv))])));
            dta.out$GDM <- NA
            ind.no <- match(dta.out.full$aPAS_TYP,names(dta.out))
            for(q in seq_along(ind.no)) {
              p <- ind.no[q]
              dta.out$GDM[q] <- dta.out[q,p]
            }; #END q loop
            rm(q,p,ind.no);
            dta.out$GDM_ADJ <- dta.out$GDM;
            dta.out2 <- dta.out[c(1,length(names(dta.out)))];
            colnames(dta.out2) <- c(paste0( "Sent_NDVI_",img.dates[i]),paste0("GDM_Est_",img.dates[i]));
            dta.out.full <- cbind(dta.out.full,dta.out2);
            dta.out1 <- cbind(boundary1@data$PADD_NAME,dta.out);
            colnames(dta.out1) <- c("PADD_NAME",names(dta.out))
            write.csv(dta.out1, paste0(d.dir,propmeta.i$OutLoc,"/", x, "_GDM_",img.dates[i],".csv"));
            suppressWarnings(rm(dta.out, dta.out1, dta.out2, ndvi1, treemask2,  ndvi.masked1));
            dev.off();
            gc();
            gc();
            
          }
        } else {
          print("Success!");
          ndvi1 <- crop(ndvi,dims1);
          treemask2 <- treemask1;
          ndvi.crp1 <- raster::mask(ndvi1,treemask1);
          values(treemask2) <- ifelse(values(treemask2) == 255, NA, values(ndvi.crp1));
          ndvi.masked1 <- raster::mask(ndvi.crp1,treemask2);
          x11();
          plot(ndvi.crp1);
          plot(boundary1, add = TRUE);
          Sys.sleep(2);
          plot(ndvi.masked1);
          plot(boundary1, add = TRUE);
          crs(ndvi.masked1) <- UCRS[[propmeta.i$CRS.no]];
          dta.out <- extract(ndvi.masked1,boundary1, fun = mean, na.rm = TRUE);
          rm(ndvi.masked1,ndvi,ndvi.crp1,ndvi1,treemask2);
          gc()
          gc()
          dta.out <- data.frame(NDVI = dta.out);
          pmods.ls <- load.pm()
          pmods.ls <- pmods.ls[!grepl("ACS211NDVI-SentinelNDVI.rda",pmods.ls)]
          dta.out[,2:(length(pmods.ls)+1)] <- dta.out$NDVI;
          glm.ls <- (ls('.GlobalEnv')[grepl(".glm",ls('.GlobalEnv'))]);
          colnames(dta.out) <- c("NDVI",sub(".glm",".CC_NDVI",glm.ls));
          for (k in seq_along(pmods.ls)){
            dta.out[,(k+1)] <- predict(get(glm.ls[k]),dta.out,type='response');
          }; #END k loop
          rm(k);
          colnames(dta.out) <- c("NDVI",sub(".glm","",glm.ls));
          dta.out$GDM <- NA
          ind.no <- match(dta.out.full$aPAS_TYP,names(dta.out))
          for(q in seq_along(ind.no)) {
            p <- ind.no[q]
            dta.out$GDM[q] <- dta.out[q,p]
          }; #END z loop
          rm(q,p,ind.no);
          print("Success!")
          #Adjust GDM based on % of trees in paddock and assumption that there is 50% less biomass under them#
          #dta.out$GDM_ADJ <- (((dta.out$GDM/2 * dta.out.full$bTREED_PERC) + (dta.out$GDM * (100-dta.out.full$bTREED_PERC))) / 100);
          #Adjust GDM based on % of trees in paddock and assumption that the more trees in the paddock the less biomass there is under them#
          dta.out$GDM_ADJ <- ((((dta.out$GDM*dta.out.full$bTREED_PERC/100) * dta.out.full$bTREED_PERC) + (dta.out$GDM * (100-dta.out.full$bTREED_PERC))) / 100);
          padd.out1 <- SpatialPolygonsDataFrame(boundary1,dta.out,match.ID = FALSE);
          pts.poly1 <- SpatialPointsDataFrame(boundary1,dta.out);
          dta.out2 <- dta.out[c(1,length(names(dta.out)))];
          colnames(dta.out2) <- c(paste0( "Sent_NDVI_",img.dates[i]),paste0("GDM_Est_",img.dates[i]));
          dta.out.full <- cbind(dta.out.full,dta.out2);
          dta.out1 <- cbind(boundary1@data$PADD_NAME,dta.out);
          colnames(dta.out1) <- c("PADD_NAME",names(dta.out))
          write.csv(dta.out1, paste0(d.dir,propmeta.i$OutLoc,"/", x, "_GDM_",img.dates[i],".csv"));
          suppressWarnings(rm(dta.out, dta.out1, dta.out2));
          gc();
          gc();
          dev.off();
        };
        
        print(paste("Time Elapsed =", time_length(Sys.time() - t2), "seconds"));
        Sys.sleep(1);
        suppressWarnings(rm(ndvi.crp1,ndvi.crp2,ndvi));
        gc();
        gc();
      };# END i Loop
      rm(i)
      if (skip == TRUE) {
        if(file.exists(paste0(out.dir,"/", x, "_GDM_AllAvailableDates.csv"))){
          all.data.file <- read.csv(paste0(out.dir,"/", x, "_GDM_AllAvailableDates.csv"), header = TRUE, check.names = FALSE);
        } else {
          dta.out.full <- dta.out.full[,sort(names(dta.out.full))];
          all.data.file <- dta.out.full[1:5]
        };
        dof.len <- dim(dta.out.full)[2]
        if (dof.len >= 6) {
          dta.out.full <- dta.out.full[,sort(names(dta.out.full))];
          dta.out.full <- cbind(all.data.file,dta.out.full[6:dof.len]);
          dta.out.full <- dta.out.full[,sort(names(dta.out.full))];
          natest <- function(x) {all(is.na(x))}
          col.chk <- apply(dta.out.full,2,natest);
          # table(col.chk);
          dta.out.full <- dta.out.full[,!col.chk];
          if (dim(dta.out.full)[2] != dim(all.data.file)[2]){
            write.csv(dta.out.full, paste0(d.dir,propmeta.i$OutLoc,"/",x,"_GDM_AllAvailableDates.csv"), row.names = FALSE);
            write.csv(dta.out.full, paste0(d.dir,propmeta.i$AppFile,"/",x,"/",x,"_GDM_AllAvailableDates.csv"), row.names = FALSE);
            if (!is.na(propmeta.i$AppFile2nd)) {write.csv(dta.out.full, paste0(d.dir,propmeta.i$AppFile2nd,"/",x,"/",x,"_GDM_AllAvailableDates.csv"), row.names = FALSE)};
            write.csv(dta.out.full, paste0(d.dir,"/ShinyApps",master.app,"/www/DataOut/",x,"/",x,"_GDM_AllAvailableDates.csv"), row.names = FALSE);
          }
        } else {
          base::message( "All new files contained imagery that didnt overlap with property shapefile...")
        }
        
        
        if(dof.len >= 6) {
          boundary2 <- boundary1;
          if (class(boundary1)[1]=="SpatialPointsDataFrame"){
            row.names(dta.out.full) <- boundary2@data$ID;
          } else {
            row.names(dta.out.full) <- lapply(boundary2@polygons, function(z) slot(z, "ID"));
          };
          boundary2@data <- dta.out.full;
          boundary2 <- spTransform(boundary2,CRS("+proj=longlat +datum=WGS84"));
          if (class(boundary1)[1]=="SpatialPointsDataFrame"){
            geojsonio::geojson_write(boundary2, geometry = "point", file = paste0(out.dir,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), convert_wgs84 = FALSE, crs = UCRS[[propmeta.i$CRS.no]]);
          } else {
            geojsonio::geojson_write(boundary2, geometry = "polygon", file = paste0(out.dir,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), convert_wgs84 = FALSE, crs = UCRS[[propmeta.i$CRS.no]]);
          };
          suppressWarnings(file.copy(from = paste0(out.dir,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), to = paste0(d.dir,propmeta.i$AppFile,"/",x,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), overwrite = TRUE, copy.date = TRUE));
          if (!is.na(propmeta.i$AppFile2nd)) {suppressWarnings(file.copy(from = paste0(out.dir,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), to = paste0(d.dir,propmeta.i$AppFile2nd,"/",x,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), overwrite = TRUE, copy.date = TRUE))};
          suppressWarnings(file.copy(from = paste0(out.dir,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), to = paste0(d.dir,"/ShinyApps",master.app,"/www/DataOut/",x,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), overwrite = TRUE, copy.date = TRUE));
        }
      } else {
        dta.out.full <- dta.out.full[,sort(names(dta.out.full))];
        natest <- function(x) {all(is.na(x))}
        col.chk <- apply(dta.out.full,2,natest);
        # table(col.chk);
        dta.out.full <- dta.out.full[,!col.chk];
        write.csv(dta.out.full, paste0(d.dir,propmeta.i$OutLoc,"/",x,"_GDM_AllAvailableDates.csv"), row.names = FALSE);
        write.csv(dta.out.full, paste0(d.dir,propmeta.i$AppFile,"/",x,"/",x,"_GDM_AllAvailableDates.csv"), row.names = FALSE);
        if (!is.na(propmeta.i$AppFile2nd)) {write.csv(dta.out.full, paste0(d.dir,propmeta.i$AppFile2nd,"/",x,"/",x,"_GDM_AllAvailableDates.csv"), row.names = FALSE)};
        if (!dir.exists(paste0(d.dir,"/ShinyApps",master.app,"/www/DataOut/",x))) {
          dir.create(paste0(d.dir,"/ShinyApps",master.app,"/www/DataOut/",x),recursive = TRUE)
          dir.create(paste0(d.dir,"/ShinyApps",master.app,"/www/DataOut/",x,"/GeoJson"))
        }
        write.csv(dta.out.full, paste0(d.dir,"/ShinyApps",master.app,"/www/DataOut/",x,"/",x,"_GDM_AllAvailableDates.csv"), row.names = FALSE);
        boundary2 <- boundary1;
        if (class(boundary1)[1]=="SpatialPointsDataFrame"){
          row.names(dta.out.full) <- boundary2@data$ID;
        } else {
          row.names(dta.out.full) <- lapply(boundary2@polygons, function(z) slot(z, "ID"));
        };
        boundary2@data <- dta.out.full;
        boundary2 <- spTransform(boundary2,CRS("+proj=longlat +datum=WGS84"));
        if (class(boundary1)[1]=="SpatialPointsDataFrame"){
          geojsonio::geojson_write(boundary2, geometry = "point", file = paste0(out.dir,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), convert_wgs84 = FALSE, crs = UCRS[[propmeta.i$CRS.no]]);
        } else {
          geojsonio::geojson_write(boundary2, geometry = "polygon", file = paste0(out.dir,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), convert_wgs84 = FALSE, crs = UCRS[[propmeta.i$CRS.no]]);
        };
        suppressWarnings(file.copy(from = paste0(out.dir,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), to = paste0(d.dir,propmeta.i$AppFile,"/",x,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), overwrite = TRUE, copy.date = TRUE));
        if (!is.na(propmeta.i$AppFile2nd)) {suppressWarnings(file.copy(from = paste0(out.dir,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), to = paste0(d.dir,propmeta.i$AppFile2nd,"/",x,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), overwrite = TRUE, copy.date = TRUE))};
        suppressWarnings(file.copy(from = paste0(out.dir,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), to = paste0(d.dir,"/ShinyApps",master.app,"/www/DataOut/",x,"/GeoJson/",x,"_GDM_AllAvailableDates.geojson"), overwrite = TRUE, copy.date = TRUE));
      }
      print(paste("Processing Complete... Will redeploy apps for", x,"shortly..."));
      Sys.sleep(1);
      base::message(paste("Processing end time:", Sys.time()));
      Sys.sleep(2);
      
    }
    base::message(paste("Done - Time Elapsed =", time_length(Sys.time() - t1), "seconds"));
    print("All selected properties successfully updated moving on to deploying apps")
    if (exists("dta.out.full")) {rm(dta.out.full)}
    gc();
    gc();
  }
  print("post.processor - successfully loaded!");
  select.imgs.only <- function(propmeta = "Propmeta",imndts = "image.names.dates") {
    
  };  ####NOT IMPLEMENTED YET####
  make.maps <- function() {
    colfunc <- colorRampPalette(c("#DD0000","#DA6500","#D7C900","#7FD500","#1BD200","#00D045","#00CDA4","#0094CB","#0035C8","#2700C6"));
    col.50 <- colfunc(50);
    legend_image <- as.raster(matrix(colfunc(10), ncol = 1));
    brks <- seq(0,4000,length.out = 50);
    grps <- with(padd.out1@data, base::cut(padd.out1@data$GDM_ADJ, breaks = brks, include.lowest = TRUE));
    padd.out1@data$COLOUR <- col.50[grps];
    
    img.sz.x <- dims1[2]-dims1[1];
    img.sz.y <- dims1[4]-dims1[3];
    leg.len <- 0.2 * img.sz.y;
    leg.wd <- 0.3 * leg.len;
    
    cntr.pnt.x <- mean(c(dims1[2],dims1[1]));
    cntr.pnt.y <- mean(c(dims1[4],dims1[3]));
    
    leg.loc.x1 <- cntr.pnt.x + (0.57*img.sz.x);
    leg.loc.y1 <- cntr.pnt.y - (0.68*img.sz.y);
    leg.loc.x2 <- cntr.pnt.x + (0.4*img.sz.x);
    leg.loc.y2 <- cntr.pnt.y + (0.5*img.sz.y);
    leg.loc.x3 <- dims1[1] + (0.15*img.sz.x);
    leg.loc.y3 <- dims1[3] + (0.05*img.sz.x);
    leg.loc.y4 <- dims1[3] + (1.05*img.sz.x);
    
    sca.ba.len <- round_any(img.sz.x/2,100);
    
    logo <- readPNG(paste0(wd,"/Logos/",propmeta.i$Logo));
    logo <- resize(x = logo,w = 90, filter ="bilinear");
    
    log.loc.xl <- cntr.pnt.x - (0.57*img.sz.x);
    log.loc.yt <- cntr.pnt.y + (0.68*img.sz.y);
    log.loc.xr <- (0.25 * img.sz.x) + log.loc.xl;
    log.loc.yb <- log.loc.yt - 1.25 * (((log.loc.xr-log.loc.xl)/dim(logo)[2]) * dim(logo)[1]);
    
    set.cex <- 1250/img.sz.x;
    set.cex[set.cex >= 0.8] <- 0.8; 
    
    plot(padd.out1,xlim = c(dims1[1] - 0.0025*dims1[1], dims1[2] + 0.0025*dims1[2]), ylim = c(dims1[3] - 0.00025*dims1[3], dims1[4] + 0.00025*dims1[4]), col =  padd.out1@data$COLOUR, main = paste0(x[i], " - Average GDM - ",img.dates[j]));
    points(pts.poly1, pch = 20, cex = 0.00,col = "grey8");
    text(pts.poly1,labels = paste(round(pts.poly1@data$GDM_ADJ),"kg/ha"), col = "grey8", cex = set.cex, pos = 4, offset = -0.3);
    box(which = "plot", lty = "solid");
    text(x = leg.loc.x1, y = seq(leg.loc.y1, leg.loc.y1 + leg.len, l=6), labels = paste(seq(0, 4000,l=6),"kg/ha"), cex = 0.65);
    rasterImage(legend_image, xleft = leg.loc.x2, ybottom = leg.loc.y1+leg.len, xright = leg.loc.x2+leg.wd, ytop = leg.loc.y1)
    north.arrow(xb = leg.loc.x1, yb = leg.loc.y2, len=img.sz.y*0.025, lab = "N");
    GISTools::map.scale(x = leg.loc.x3, y = dims1[3]-0.5*leg.len , len = sca.ba.len, ndivs = ceiling(sca.ba.len/1000), subdiv = 1, units = "kilometres");
    rasterImage(logo, xleft = log.loc.xl, ybottom = log.loc.yb, xright = log.loc.xr, ytop = log.loc.yt);
    Sys.sleep(3);
    dev.off();
    
    pdf(paste0(wd,propmeta.i$OutLoc,"/Maps/", x[i], "_GDM_",img.dates[j],".pdf"),width = 20, height = 25, paper = "a4");
    plot(padd.out1,xlim = c(dims1[1] - 0.0025*dims1[1], dims1[2] + 0.0025*dims1[2]), ylim = c(dims1[3] - 0.00025*dims1[3], dims1[4] + 0.00025*dims1[4]), col =  padd.out1@data$COLOUR, main = paste0(x[i], " - Average GDM - ",img.dates[j]));
    points(pts.poly1, pch = 20, cex = 0.00,col = "grey8");
    text(pts.poly1,labels = paste(round(pts.poly1@data$GDM_ADJ),"kg/ha"), col = "grey8", cex = set.cex, pos = 4, offset = -0.3);
    box(which = "plot", lty = "solid");
    text(x = leg.loc.x1, y = seq(leg.loc.y1, leg.loc.y1 + leg.len, l=6), labels = paste(seq(0, 4000,l=6),"kg/ha"), cex = 0.65);
    rasterImage(legend_image, xleft = leg.loc.x2, ybottom = leg.loc.y1+leg.len, xright = leg.loc.x2+leg.wd, ytop = leg.loc.y1)
    north.arrow(xb = leg.loc.x1, yb = leg.loc.y2, len=img.sz.y*0.025, lab = "N");
    GISTools::map.scale(x = leg.loc.x3, y = dims1[3]-0.5*leg.len , len = sca.ba.len, ndivs = ceiling(sca.ba.len/1000), subdiv = 1, units = "kilometres");
    rasterImage(logo, xleft = log.loc.xl, ybottom = log.loc.yb, xright = log.loc.xr, ytop = log.loc.yt);
    dev.off();
    
    #plot(padd.out1,xlim = c(dims1[1], dims1[2]),ylim = c(dims1[3],dims1[4]), col =  padd.out1@data$COLOUR, main = paste0(x[i], " - Average GDM - ",img.dates[j]));
    
    rm(img.sz.x,img.sz.y,leg.len,leg.loc.x1,leg.loc.x2,leg.loc.x3,leg.loc.y1,leg.loc.y2,leg.loc.y3,leg.loc.y4,leg.wd,legend_image,log.loc.xl,log.loc.xr,log.loc.yb,log.loc.yt,logo,sca.ba.len,set.cex,cntr.pnt.x,cntr.pnt.y,brks,col.50,col.chk,grps)
    gc()
    
  }; ####NOT IMPLEMENTED YET####
  postprocess.fast <- function(x) {
    no_cores <- availableCores();
    if (!is.na(max.cores)) {no_cores <- max.cores}
    c2 <- parallel::makeCluster(no_cores,outfile="tmp/post_parallel_debug_file.log")
    registerDoParallel(c2);  #8 cores works well for quad core processor (you can set it to as many cores as you like but the process of allocating task to all the individual cores you create increases the overall processing time when you add too many - there's a sweet spot)
    autoStopCluster(c2);
    getDoParWorkers();
    foreach(r = seq_along(x), .packages=c('lubridate','raster', 'rgdal','plyr','doParallel','gsubfn'), .export = c(ls(.GlobalEnv))) %dopar% {
      pn <- x[[r]]
      post.processor(pn)
    };
    stopCluster(c2);
    gc();
    gc();
  };
  print("postprocess.fast - successfully loaded!");
  md5post <- function(av.shiny.apps, MD5.check1) {
    GeoJSON.Files <- list.files(paste0(d.dir,"/DataOut"), recursive = TRUE, pattern = ".geojson", full.names = TRUE);
    a <- length(strsplit(GeoJSON.Files[1], split = "/")[[1]])-2
    GeoJSON.Files.MD5 <- md5sum(files = GeoJSON.Files);
    MD5.check2 <- data.frame(filename = GeoJSON.Files,
                             MD5Checksum2 = GeoJSON.Files.MD5,
                             stringsAsFactors = FALSE);
    MD5.check2$property2 <- gsub("_GDM_AllAvailableDates.geojson","",sapply(strsplit(MD5.check2$filename, split = "/"),"[",a));
    row.names(MD5.check2) <- 1:length(MD5.check2$filename)
    rm(a,GeoJSON.Files,GeoJSON.Files.MD5); 
    gc()
    MD5.check3 <- base::merge(MD5.check1,MD5.check2, by = "filename", all.y = TRUE )
    MD5.check4 <- MD5.check3[MD5.check3$MD5Checksum!=MD5.check3$MD5Checksum2,]
    MD5.check4$AppFile <- propmeta$AppFile[propmeta$Property  %in%  MD5.check4$property2]
    MD5.check4$AppFile <- gsub("/www/DataOut","",MD5.check4$AppFile);
    MD5.check4$AppFile2nd <- propmeta$AppFile2nd[propmeta$Property  %in%  MD5.check4$property2]
    MD5.check4$AppFile2nd <- gsub("/www/DataOut","",MD5.check4$AppFile2nd);
    return(MD5.check4)
    
  }; 
  print("md5post - successfully loaded!");
  app.update <- function(pm = "Property Metadata") {
    av.shiny.apps <- as.data.frame(cbind(AppFile = pm$AppFile,ShinyAppAccnt = pm$ShinyAppAccnt, Property = pm$Property), stringsAsFactors = FALSE);
    av.shiny.apps2 <- as.data.frame(cbind(AppFile = pm$AppFile2nd,ShinyAppAccnt = pm$ShinyAppAccnt2nd, Property = pm$Property), stringsAsFactors = FALSE);
    av.shiny.apps2 <- av.shiny.apps2[!is.na(av.shiny.apps2$AppFile),]
    av.shiny.apps <- rbind(av.shiny.apps,av.shiny.apps2);
    av.shiny.apps$AppFile <- gsub("/www/DataOut","",av.shiny.apps$AppFile);
    av.shiny.apps$AppFile <- gsub("/shinyapps/", "",av.shiny.apps$AppFile);
    master.app2 <- sub("[$/]","",master.app)
    av.shiny.apps <- av.shiny.apps[!grepl(paste0(master.app2),av.shiny.apps$AppFile),];
    if (property.nam[1] != "All") {
      av.shiny.apps <- av.shiny.apps[av.shiny.apps$Property %in% property.nam,];
    }
    av.shiny.apps <- av.shiny.apps[!duplicated(av.shiny.apps[c(1,2)]),]
    return(av.shiny.apps)
  }; 
  print("app.update - successfully loaded!");
  app.deploy <- function(MD5.check4, av.shiny.apps) {
    print("Deploying master app");
    master.app.acct <- get("master.app.acct", envir = .GlobalEnv);
    master.app <- get("master.app", envir = .GlobalEnv);
    master.app2 <- sub("[$/]","",master.app)
    avail.app.accts <- get("avail.app.accts", envir = .GlobalEnv);
    print(paste("Deploying master app:",master.app2));
    rsconnect::deployApp(paste0(d.dir,"/ShinyApps",master.app),account = master.app.acct, forceUpdate = getOption("rsconnect.force.update.apps", TRUE));
    Apps2GoUp <- c(MD5.check4$AppFile,MD5.check4$AppFile2nd)
    Apps2GoUp <- Apps2GoUp[!duplicated(Apps2GoUp)]
    Apps2GoUp <- Apps2GoUp[!grepl(paste0(master.app2),Apps2GoUp)];
    Apps2GoUp <- Apps2GoUp[!is.na(Apps2GoUp)]
    av.shiny.apps <- av.shiny.apps[paste0("/shinyapps/",av.shiny.apps$AppFile)  %in%  Apps2GoUp,]
    av.shiny.apps <- av.shiny.apps[!duplicated(av.shiny.apps$AppFile),]
    for (l in seq_along(av.shiny.apps$AppFile)){
      print(paste("Deploying app", (l+1), "of", length(av.shiny.apps$AppFile)+1));
      Sys.sleep(1);
      rsconnect::deployApp(paste0(d.dir,"/shinyapps/",av.shiny.apps$AppFile[l]),account = av.shiny.apps$ShinyAppAccnt[l], forceUpdate = getOption("rsconnect.force.update.apps", TRUE));
    };# END l loop
    return(Apps2GoUp)
  };
  print("app.deploy - successfully loaded!");
  send.mail <- function(Apps2GoUp) {
    print("Sending notification emails....");
    if (!file.exists(paste0(d.dir,"/emailout/Variables.csv"))) {
      vars <- data.frame(lastname = NA,
                         firstname = NA,
                         app_name = NA,
                         app_address = NA,
                         email_address = NA, stringsAsFactors = FALSE)
      write.csv(vars, file = paste0(d.dir,"/emailout/Variables.csv"),row.names = FALSE)
      base::message(paste0("No email data exists in ",d.dir,"/emailout/Variables.csv.  Please insert some before next run.  Put yourself as the 1st row"))
    }
    my_dat <- read_csv(paste0(d.dir,"/emailout/Variables.csv"), show_col_types = FALSE)
    #dez_dat <- my_dat[grep("Dez",my_dat$firstname),]
    dez_dat <- my_dat[1,]
    my_dat <- my_dat[tolower(paste0("/shinyapps/",unlist(lapply(strsplit(my_dat$app_address,split = "/"), "[",4)))) %in% tolower(Apps2GoUp),]
    my_dat <- rbind(dez_dat,my_dat)
    body <- "G'day, %s.

New imagery available on the Pasture Monitoring app: %s.

Enjoy!

Dez.
"
    edat <- my_dat %>%
      mutate(
        To = sprintf('%s <%s>', firstname, email_address),
        Bcc = optional_bcc,
        From = email_sender,
        Subject = sprintf('New pasture estimates available on  %s shiny app', app_name),
        body = sprintf(body, firstname, app_address)) %>%
      select(To, Bcc, From, Subject, body)
    write_csv(edat, paste0(d.dir,"/emailout/data-frame.csv"))
    emails <- edat %>%
      pmap(mime)
    str(emails, max.level = 2, list.len = 2)
    suppressWarnings(use_secret_file(paste0(d.dir,"/emailout/client_secret.json")))
    gm_auth_configure(key=eo.key,secret=eo.secret,path = paste0(d.dir,"/emailout/client_secret.json"), appname = eo.appname)
    safe_send_message <- safely(send_message)
    sent_mail <- emails %>%
      map(safe_send_message)
    saveRDS(sent_mail,
            paste(gsub("\\s+", "_", this_hw), "sent-emails.rds", sep = "_"))
    errors <- sent_mail %>%
      transpose() %>%
      .$error %>%
      map_lgl(Negate(is.null))
    base::print("Emails sent!")
  }; ####NOT IMPLEMENTED YET####
  print("send.mail - successfully loaded!");
}
####END SCRIPT####
