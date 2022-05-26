######################################################################################
##################################app-deploy.R########################################
######################################################################################

####User Defined Functions####
av.shiny.apps <- as.data.frame(cbind(AppFile = propmeta$AppFile,ShinyAppAccnt = propmeta$ShinyAppAccnt, Property = propmeta$Property), stringsAsFactors = FALSE);
av.shiny.apps2 <- as.data.frame(cbind(AppFile = propmeta$AppFile2nd,ShinyAppAccnt = propmeta$ShinyAppAccnt2nd, Property = propmeta$Property), stringsAsFactors = FALSE);
av.shiny.apps2 <- av.shiny.apps2[!is.na(av.shiny.apps2$AppFile),]
av.shiny.apps <- rbind(av.shiny.apps,av.shiny.apps2);
av.shiny.apps <- av.shiny.apps[!duplicated(av.shiny.apps[c(1,2)]),]
av.shiny.apps$AppFile <- gsub("/www/DataOut","",av.shiny.apps$AppFile);
master.app2 <- sub("[$/]","",master.app)
av.shiny.apps <- av.shiny.apps[!grepl(paste0(master.app2),av.shiny.apps$AppFile),];
if (property.nam[1] != "All") {
  av.shiny.apps <- av.shiny.apps[av.shiny.apps$Property %in% property.nam,];
}


####END SCRIPT####