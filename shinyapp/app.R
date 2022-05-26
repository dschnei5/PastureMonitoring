#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(gsubfn)
library(tidyr)
library(rgdal)
library(leaflet)
library(geojsonR)
library(dplyr)
library(lubridate)

#####Versioning####

# 0.1 - Added date selection and cumulative plotting
# 0.2 - Added the paddock total GDM
# 0.3 aka ver 0 - Added data files to app directory dropped the .3 for upload to shinyapps.io
# 0 - UNE Holdings
# 0.4 - UNE Holding Combined Map and Graph, standardised colour ramp, removed GDM decimal places, split UNE properties
# 0.5 - PARG App built on 0.4
# 0.6 - Updated to allow point geoJSOn files 

####Add prerequisites####

# properties <- list.dirs("C:\\R\\2020-01-01_Sentinel4UNE\\DataOut",recursive = FALSE, full.names = FALSE)

#################################################################################################################

# Define UI for application that draws a histogram
ui <- navbarPage("UNE PARG Pasture Monitoring App",
                 theme=shinytheme(theme = "flatly"),
                 
                 tabPanel("Sentinel Pasture Estimate Maps",uiOutput("map_content")),
                 
                 tabPanel("Sentinel Pasture Estimate Plots",uiOutput("plot_content"))
                 
)



######################################################################################################################


# Define server logic required to create plots
server <- function(input, output) {
  
  dta.in <- reactive({df <- read.csv(paste0("www/DataOut/",input$property,"/",input$property,"_GDM_AllAvailableDates.csv"),header = TRUE, check.names = FALSE)
  df <- as.data.frame(df)
  #df <- df[complete.cases(df),]
  return(df)}) # Data for Plot App
  
  dta.in2 <- reactive({df <- rgdal::readOGR(paste0("www/DataOut/",input$property2,"/GeoJson/",input$property2,"_GDM_AllAvailableDates.geojson"))
  df <- df
  return(df)}) # Data for Map App
  
  output$paddocks <- renderUI({
    
    selectInput("paddocks2", "Select a Paddock: ", choices = sort(dta.in()$aPADD_NAME, decreasing = FALSE), multiple = TRUE)
    
  })
  
  output$gdmcheck <- renderUI({
    if(input$ndvi == "GDM"){
      checkboxInput(inputId = "switch.1",label = "Display GDM as paddock total in kg", value = FALSE)
    } else {
      if(input$ndvi == "ACC") {
        checkboxInput(inputId = "switch.2",label = "Display accumulation as stacked yearly plot - (Not implemented yet)", value = FALSE)
      } else {NULL}
    }
    
  })
  
  output$accnote <- renderText({
    if (is.null(input$paddocks2) | input$ndvi != "ACC") {
      return(NULL)
    } else {
      "NOTE: the accumulation (ACC) plot provides an accumulation of green pasture (on a dry matter basis) but does not reflect the current level because removal by grazing, and pasture decay are not considered" 
    }
  })
  
  output$parg <- renderImage({
    return(list(
      src = "www/parg.png",
      contentType = "image/png",
      alt = "PARG Logo Missing"
    ))
    
  }, deleteFile = FALSE)
  
  output$parg2 <- renderImage({
    return(list(
      src = "www/parg.png",
      contentType = "image/png",
      alt = "PARG Logo Missing"
    ))
    
  }, deleteFile = FALSE)
  
  output$smart <- renderImage({
    return(list(
      src = "www/logo2.png",
      contentType = "image/png",
      alt = "Logo Missing",
      height = 150
    ))
    
  }, deleteFile = FALSE)
  
  output$smart2 <- renderImage({
    return(list(
      src = "www/logo2.png",
      contentType = "image/png",
      alt = "Logo Missing",
      height = 150
    ))
    
  }, deleteFile = FALSE)
  
  output$mostrecentest <- renderText({
    if (is.null(input$paddocks2) | input$ndvi == "ACC") {
      return(NULL)
    } else {
      "The most recent estimates for selected paddocks are: " 
    }
  })
  
  output$mostrectab <- renderTable({
    
    if (is.null(input$paddocks2) | input$ndvi == "ACC") {
      return(NULL)
    } else if (input$ndvi == "NDVI") {
      
      dta.in2 <- as.data.frame(t(dta.in()[-c(1:3)]))
      colnames(dta.in2) <- dta.in()$aPADD_NAME
      dta.in2 <- dta.in2[grepl(input$ndvi,(row.names(dta.in2))),]
      dta.in2$DATE <- as.Date(unlist(strapplyc(rownames(dta.in2), "\\d+-\\d+-\\d+", simplify = TRUE)))
      #padds2plot <- function (x) { dta.in2[,grep(x,names(dta.in2))] }
      padds2plot <- function (x) { dta.in2[,grep(paste0("^",x,"$"),names(dta.in2))] }
      pads <- as.character(input$paddocks2)
      dta.in3 <- cbind(Date = dta.in2$DATE,as.data.frame(sapply(pads,padds2plot)))
      dta.in3 <- dta.in3[complete.cases(dta.in3), ]
      dta.in3 <- dta.in3[which.max(dta.in3$Date), ]
      dta.in3$Date <- format(dta.in3$Date,'%d %b %Y')
      return(dta.in3)
      
      #dta.in <- read.csv(paste0("www\\DataOut\\",input$property,"\\",input$property,"_GDM_AllAvailableDates.csv"),header = TRUE, check.names = FALSE)
      
    } else {
      if (class(dta.in2()) == "SpatialPolygonsDataFrame") {
           if (input$ndvi == "GDM" & input$switch.1) {
             # dta.in <- read.csv(paste0("www\\DataOut\\",input$property,"\\",input$property,"_GDM_AllAvailableDates.csv"),header = TRUE, check.names = FALSE)
             descript.dta <- dta.in()[1:3]
             dta.in2 <- as.data.frame(t(dta.in()[-c(1:3)]))
             colnames(dta.in2) <- dta.in()$aPADD_NAME
             dta.in2 <- dta.in2[grepl(input$ndvi,(row.names(dta.in2))),]
             dta.in2$DATE <- as.Date(unlist(strapplyc(rownames(dta.in2), "\\d+-\\d+-\\d+", simplify = TRUE)))
             a <- (dim(dta.in2)[2] - 1) * 1
             dta.in2[1:a] <- as.data.frame(as.matrix(dta.in2[1:a]) %*% diag(descript.dta$bHECTARES))
             #padds2plot <- function (x) { dta.in2[,grep(x,names(dta.in2))] }
             padds2plot <- function (x) { dta.in2[,grep(paste0("^",x,"$"),names(dta.in2))] }
             pads <- as.character(input$paddocks2)
             dta.in3 <- cbind(Date = dta.in2$DATE,as.data.frame(sapply(pads,padds2plot)))
             dta.in3 <- dta.in3[complete.cases(dta.in3), ]
             dta.in3 <- dta.in3[which.max(dta.in3$Date), ]
             dta.in3$Date <- format(dta.in3$Date,'%d %b %Y')
             return(dta.in3)
           } else {
             dta.in2 <- as.data.frame(t(dta.in()[-c(1:3)]))
             colnames(dta.in2) <- dta.in()$aPADD_NAME
             dta.in2 <- dta.in2[grepl(input$ndvi,(row.names(dta.in2))),]
             dta.in2$DATE <- as.Date(unlist(strapplyc(rownames(dta.in2), "\\d+-\\d+-\\d+", simplify = TRUE)))
             #padds2plot <- function (x) { dta.in2[,grep(x,names(dta.in2))] }
             padds2plot <- function (x) { dta.in2[,grep(paste0("^",x,"$"),names(dta.in2))] }
             pads <- as.character(input$paddocks2)
             dta.in3 <- cbind(Date = dta.in2$DATE,as.data.frame(sapply(pads,padds2plot)))
             dta.in3 <- dta.in3[complete.cases(dta.in3), ]
             dta.in3 <- dta.in3[which.max(dta.in3$Date), ]
             dta.in3$Date <- format(dta.in3$Date,'%d %b %Y')
             return(dta.in3)
           }
      } else {
        if (input$ndvi == "GDM" & input$switch.1) {
          # dta.in <- read.csv(paste0("www\\DataOut\\",input$property,"\\",input$property,"_GDM_AllAvailableDates.csv"),header = TRUE, check.names = FALSE)
          descript.dta <- dta.in()[1:2]
          descript.dta$bHECTARES <- 1
          dta.in2 <- as.data.frame(t(dta.in()[-c(1:2)]))
          colnames(dta.in2) <- dta.in()$aPADD_NAME
          dta.in2 <- dta.in2[grepl(input$ndvi,(row.names(dta.in2))),]
          dta.in2$DATE <- as.Date(unlist(strapplyc(rownames(dta.in2), "\\d+-\\d+-\\d+", simplify = TRUE)))
          a <- (dim(dta.in2)[2] - 1) * 1
          dta.in2[1:a] <- as.data.frame(as.matrix(dta.in2[1:a]) %*% diag(descript.dta$bHECTARES))
          #padds2plot <- function (x) { dta.in2[,grep(x,names(dta.in2))] }
          padds2plot <- function (x) { dta.in2[,grep(paste0("^",x,"$"),names(dta.in2))] }
          pads <- as.character(input$paddocks2)
          dta.in3 <- cbind(Date = dta.in2$DATE,as.data.frame(sapply(pads,padds2plot)))
          dta.in3 <- dta.in3[complete.cases(dta.in3), ]
          dta.in3 <- dta.in3[which.max(dta.in3$Date), ]
          dta.in3$Date <- format(dta.in3$Date,'%d %b %Y')
          return(dta.in3)
        } else {
          dta.in2 <- as.data.frame(t(dta.in()[-c(1:3)]))
          colnames(dta.in2) <- dta.in()$aPADD_NAME
          dta.in2 <- dta.in2[grepl(input$ndvi,(row.names(dta.in2))),]
          dta.in2$DATE <- as.Date(unlist(strapplyc(rownames(dta.in2), "\\d+-\\d+-\\d+", simplify = TRUE)))
          #padds2plot <- function (x) { dta.in2[,grep(x,names(dta.in2))] }
          padds2plot <- function (x) { dta.in2[,grep(paste0("^",x,"$"),names(dta.in2))] }
          pads <- as.character(input$paddocks2)
          dta.in3 <- cbind(Date = dta.in2$DATE,as.data.frame(sapply(pads,padds2plot)))
          dta.in3 <- dta.in3[complete.cases(dta.in3), ]
          dta.in3 <- dta.in3[which.max(dta.in3$Date), ]
          dta.in3$Date <- format(dta.in3$Date,'%d %b %Y')
          return(dta.in3)
        }
        
      }
      
    }
    
  }, align = 'c')
  
  output$pasturePlot <- renderPlot({
    
    if (is.null(input$paddocks2)) {
      return(NULL)
    } else {
      
      #dta.in <- read.csv(paste0("www\\DataOut\\",input$property,"\\",input$property,"_GDM_AllAvailableDates.csv"),header = TRUE, check.names = FALSE)
      dta.in2 <- as.data.frame(t(dta.in()[-c(1:3)]))
      colnames(dta.in2) <- dta.in()$aPADD_NAME
      descript.dta <- dta.in()[1:3]
      
      if (input$ndvi != "ACC") {
        dta.in2 <- dta.in2[grepl(input$ndvi,(row.names(dta.in2))),]
        dta.in2$DATE <- as.Date(unlist(strapplyc(rownames(dta.in2), "\\d+-\\d+-\\d+", simplify = TRUE)))
        dta.in2 <- dta.in2[dta.in2$DATE >= input$date1 & dta.in2$DATE <= input$date2,]
        if (input$ndvi != "GDM") {
          #padds2plot <- function (x) { dta.in2[,grep(x,names(dta.in2))] }
          padds2plot <- function (x) { dta.in2[,grep(paste0("^",x,"$"),names(dta.in2))] }
          pads <- as.character(input$paddocks2)
          dta.in3 <- cbind(Date = dta.in2$DATE,as.data.frame(sapply(pads,padds2plot)))
          dta.in3 <- dta.in3[complete.cases(dta.in3), ]
          a <- dim(dta.in3)[2] * 1
          dta.in4 <- gather(dta.in3[-c(1)])
          dta.in4$index <- match(dta.in4$key,names(dta.in3))
          dta.in4$Date <- dta.in3$Date
          ggplot(data = dta.in4[dta.in4$index %in% c(2:a),],
                 aes(x = Date, y = value, colour = key)) +
            geom_point(size = 3) +
            geom_line(size = 1) +
            theme_light() +
            xlab("Observation Date") +
            ylab(input$ndvi) +
            scale_x_date(date_breaks = "3 month", date_labels = ("%b-%y")) +
            theme(
              text = element_text(size = 20),
              axis.text.x = element_text(angle = 60, hjust = 1)) +
            labs(colour = "Paddock")
        } else {
          dta.in2 <- dta.in2[grepl(input$ndvi,(row.names(dta.in2))),]
          dta.in2$DATE <- as.Date(unlist(strapplyc(rownames(dta.in2), "\\d+-\\d+-\\d+", simplify = TRUE)))
          dta.in2 <- dta.in2[dta.in2$DATE >= input$date1 & dta.in2$DATE <= input$date2,]
          if (input$switch.1) {
            a <- (dim(dta.in2)[2] - 1) * 1
            dta.in2[1:a] <- as.data.frame(as.matrix(dta.in2[1:a]) %*% diag(descript.dta$bHECTARES))
            #padds2plot <- function (x) { dta.in2[,grep(x,names(dta.in2))] }
            padds2plot <- function (x) { dta.in2[,grep(paste0("^",x,"$"),names(dta.in2))] }
            pads <- as.character(input$paddocks2)
            dta.in3 <- cbind(Date = dta.in2$DATE,as.data.frame(sapply(pads,padds2plot)))
            dta.in3 <- dta.in3[complete.cases(dta.in3), ]
            a <- dim(dta.in3)[2] * 1
            dta.in4 <- gather(dta.in3[-c(1)])
            dta.in4$index <- match(dta.in4$key,names(dta.in3))
            dta.in4$Date <- dta.in3$Date
            ggplot(data = dta.in4[dta.in4$index %in% c(2:a),],
                   aes(x = Date, y = value, colour = key)) +
              geom_point(size = 3) +
              geom_line(size = 1) +
              theme_light() +
              xlab("Observation Date") +
              ylab(input$ndvi) +
              scale_x_date(date_breaks = "3 month", date_labels = ("%b-%y")) +
              theme(
                text = element_text(size = 20),
                axis.text.x = element_text(angle = 60, hjust = 1)) +
              labs(colour = "Paddock")
          } else {
            #padds2plot <- function (x) { dta.in2[,grep(x,names(dta.in2))] }
            padds2plot <- function (x) { dta.in2[,grep(paste0("^",x,"$"),names(dta.in2))] }
            pads <- as.character(input$paddocks2)
            dta.in3 <- cbind(Date = dta.in2$DATE,as.data.frame(sapply(pads,padds2plot)))
            dta.in3 <- dta.in3[complete.cases(dta.in3), ]
            a <- dim(dta.in3)[2] * 1
            dta.in4 <- gather(dta.in3[-c(1)])
            dta.in4$index <- match(dta.in4$key,names(dta.in3))
            dta.in4$Date <- dta.in3$Date
            ggplot(data = dta.in4[dta.in4$index %in% c(2:a),],
                   aes(x = Date, y = value, colour = key)) +
              geom_point(size = 3) +
              geom_line(size = 1) +
              theme_light() +
              xlab("Observation Date") +
              ylab(input$ndvi) +
              scale_x_date(date_breaks = "3 month", date_labels = ("%b-%y")) +
              theme(
                text = element_text(size = 20),
                axis.text.x = element_text(angle = 60, hjust = 1)) +
              labs(colour = "Paddock")
          }
        }
      } else {
        if (!input$switch.2) {
          dta.in2 <- dta.in2[grepl("GDM",(row.names(dta.in2))),]
          dta.in2 <- dta.in2[, colSums(is.na(dta.in2)) != nrow(dta.in2)]
          dta.in2$DATE <- as.Date(unlist(strapplyc(rownames(dta.in2), "\\d+-\\d+-\\d+", simplify = TRUE)))
          dta.in2 <- dta.in2[dta.in2$DATE >= input$date1 & dta.in2$DATE <= input$date2,]
          #dta.in2 <- dta.in2[dta.in2$DATE >= as.Date("2018-01-01") & dta.in2$DATE <= as.Date("2019-01-01"),]
          dta.in2$NUMDAYS <- dta.in2$DATE - input$date1
          #dta.in2$NUMDAYS <- dta.in2$DATE - as.Date("2018-01-01")
          dta.in2$NUMDAYS <- c(as.numeric(dta.in2$NUMDAYS[1]),as.numeric(diff(dta.in2$NUMDAYS)))
          
          getacc <- function(x) {
            x = "dta.in2"
            dta <- get(x)
            a <- dim(dta)[2] - 2
            b <- seq_along(names(dta[1:a]))
            dta.out <- as.data.frame(sapply(dta[,b],diff))
            dta.out <- rbind(0,dta.out)
            pgr <- dta.out/dta$NUMDAYS
            dta.out[dta.out <= 0] <- 0
            dta.out <- cumsum(dta.out)
            dta[1:a] <- dta.out
            return(dta)
          }
          
          dta.in2 <- getacc(dta.in2)
          #padds2plot <- function (x) { dta.in2[,grep(x,names(dta.in2))] }
          padds2plot <- function (x) { dta.in2[,grep(paste0("^",x,"$"),names(dta.in2))] }
          #pads <- c("Top","Valley")
          pads <- as.character(input$paddocks2)
          dta.in3 <- cbind(Date = dta.in2$DATE,as.data.frame(sapply(pads,padds2plot)))
          dta.in3 <- dta.in3[complete.cases(dta.in3), ]
          a <- dim(dta.in3)[2] * 1
          dta.in4 <- gather(dta.in3[-c(1)])
          dta.in4$index <- match(dta.in4$key,names(dta.in3))
          dta.in4$Date <- dta.in3$Date
          ggplot(data = dta.in4[dta.in4$index %in% c(2:a),],
                 aes(x = Date, y = value, colour = key)) +
            geom_point(size = 3) +
            geom_line(size = 1) +
            theme_light() +
            xlab("Observation Date") +
            ylab("Accumulated GDM (kg/ha)") +
            scale_x_date(date_breaks = "1 month", date_labels = ("%b-%y")) +
            theme(
              text = element_text(size = 20),
              axis.text.x = element_text(angle = 60, hjust = 1)) +
            labs(colour = "Paddock")
        } else {
          NULL
        }
      }
    }
    
  })
  
  output$datadate <- renderUI({
    
    a <- sort(names(dta.in2()@data[grep(input$raworproc,names(dta.in2()@data))]),decreasing = TRUE)
    # varSelectInput("datadate2", "Select Date and Layer: ", dta.in()@data[-c(1:3)], multiple = FALSE)
    selectInput("datadate2", "Select Date and Layer: ",a, multiple = FALSE)
  })
  
  output$result <- renderTable({
    if (length(dta.in2()@data) > 0){
      dta.t <- as.data.frame(dta.in2()@data %>% dplyr::select(!!input$datadate2))
      dta.t$padd.nam <- dta.in2()@data %>% dplyr::select("aPADD_NAME")
      
      
      if (input$property2 != "SMARTfarms" & class(dta.in2()) == "SpatialPolygonsDataFrame" & grepl("GDM",names(dta.t))[1]) {
        dta.t$pad.size <- dta.in2()@data %>% dplyr::select("bHECTARES")
        names(dta.t) <- c("gdm","padd.nam","pad.size")
        dta.t$gdm.tot <- dta.t$gdm * dta.t$pad.size
        dta.out <- as.data.frame(input$property2)
        dta.out$Property.GDM <- as.character(round(sum(dta.t$gdm.tot,na.rm = TRUE)/sum(dta.t$pad.size,na.rm = TRUE),0))
        dta.out$Date <- as.character(as.Date(substr(input$datadate2,start = 9,stop = 19), format = "%Y.%m.%d"))
        colnames(dta.out) <- c("Property","Property Average GDM (kg/ha)","Image Date")
        return(dta.out)
        
      } else if (input$property2 == "SMARTfarms" & grepl("GDM",names(dta.t))[1]){
        dta.t$pad.size <- dta.in2()@data %>% dplyr::select("bHECTARES")
        names(dta.t) <- c("gdm","padd.nam","pad.size")
        dta.t$gdm.tot <- dta.t$gdm * dta.t$pad.size
        property.nams <- c("Clarkes_Farm","Kirby","Laureldale","Maxwelton","Newholme","Toombs","Trevenna","Tullimba")
        dta.out <- data.frame();
        for (i in seq_along(property.nams)){
          #print(i)
          #i = 2
          a <- property.nams[i]
          dta.out1 <- data.frame(a, stringsAsFactors = FALSE)
          dta.out1$Property.GDM <- as.character(round(sum(dta.t$gdm.tot[grep(a,unlist(dta.t$padd.nam)),],na.rm = TRUE) / (sum(dta.t$pad.size[grep(a,unlist(dta.t$padd.nam)),],na.rm = TRUE)),0))
          dta.out1$Date <- as.character(as.Date(substr(input$datadate2,start = 9,stop = 19), format = "%Y.%m.%d"))
          colnames(dta.out1) <- c("Property","Property Average GDM (kg/ha)","Image Date")
          dta.out <- rbind(dta.out,dta.out1)
          rm(dta.out1,a)
        }
        return(dta.out)
      } else if (class(dta.in2()) == "SpatialPointsDataFrame" & grepl("GDM",names(dta.t))[1]){
        names(dta.t) <- c("gdm","padd.nam")
        dta.t$gdm.tot <- dta.t$gdm
        dta.out <- as.data.frame(input$property2)
        #dta.out$Property.GDM <- as.character(round(sum(dta.t$gdm.tot,na.rm = TRUE)/sum(dta.t$pad.size,na.rm = TRUE),0))
        dta.out$Property.GDM <- mean(dta.t$gdm.tot, na.rm = TRUE);
        dta.out$Date <- as.character(as.Date(substr(input$datadate2,start = 9,stop = 19), format = "%Y.%m.%d"))
        colnames(dta.out) <- c("Property","Property Average GDM (kg/ha)","Image Date")
        return(dta.out)
      } else {NULL}
    } else {NULL}
  }, align = "c")
  
  output$mapPlot <- leaflet::renderLeaflet({
    if (is.null(input$datadate2)) {
      return(NULL)
    } else if (length(dta.in2()@data) > 0 & class(dta.in2()) == "SpatialPolygonsDataFrame"){
      colfunc <- colorRampPalette(c("#DD0000","#DA6500","#D7C900","#7FD500","#1BD200","#00D045","#00CDA4","#0094CB","#0035C8","#2700C6"));
      col.50 <- colfunc(50);
      pal <- colorNumeric(col.50, NULL); #"viridis", "magma", "inferno", or "plasma"
      dta <- dta.in2()@data %>% dplyr::select(!!input$datadate2)
      if (grepl("GDM",names(dta))){
        dta <- round(dta[[1]])
        pnam <- dta.in2()@data$aPADD_NAME
        
        leaflet(dta.in2()) %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%
          addPolygons(stroke = TRUE, smoothFactor = 0.3, color = "darkgrey", weight = 2, fillOpacity = 0.5, fillColor = ~pal(dta), label = ~paste(pnam, paste(dta,"kg/ha"), sep = "\n")) %>%
          addLegend(pal = pal, values = ~dta, opacity = 1.0, title = as.character(input$datadate2))
      } else {
        dta <- round(dta[[1]],4)
        pnam <- dta.in2()@data$aPADD_NAME
        
        leaflet(dta.in2()) %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%
          addPolygons(stroke = TRUE, smoothFactor = 0.3, color = "darkgrey", weight = 2, fillOpacity = 0.5, fillColor = ~pal(dta), label = ~paste(pnam, dta, sep = "\n")) %>%
          addLegend(pal = pal, values = ~dta, opacity = 1.0, title = as.character(input$datadate2), position = "bottomright", na.label = NULL)
      }
    } else if (length(dta.in2()@data) > 0 & class(dta.in2()) == "SpatialPointsDataFrame") {
      colfunc <- colorRampPalette(c("#DD0000","#DA6500","#D7C900","#7FD500","#1BD200","#00D045","#00CDA4","#0094CB","#0035C8","#2700C6"));
      col.50 <- colfunc(50);
      pal <- colorNumeric(col.50, NULL); #"viridis", "magma", "inferno", or "plasma"
      dta <- dta.in2()@data %>% dplyr::select(!!input$datadate2)
      if (grepl("GDM",names(dta))){
        dta <- round(dta[[1]])
        pnam <- dta.in2()@data$aPADD_NAME
        
        leaflet(dta.in2()) %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%
          addCircleMarkers(stroke = TRUE, color = "darkgrey", weight = 2, fillOpacity = 0.5, fillColor = ~pal(dta), label = ~paste(pnam, paste(dta,"kg/ha"), sep = "\n")) %>%
          addLegend(pal = pal, values = ~dta, opacity = 1.0, title = as.character(input$datadate2))
      } else {
        dta <- round(dta[[1]],4)
        pnam <- dta.in2()@data$aPADD_NAME
        
        leaflet(dta.in2()) %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%
          addCircleMarkers(stroke = TRUE, color = "darkgrey", weight = 2, fillOpacity = 0.5, fillColor = ~pal(dta), label = ~paste(pnam, dta, sep = "\n")) %>%
          addLegend(pal = pal, values = ~dta, opacity = 1.0, title = as.character(input$datadate2), position = "bottomright", na.label = NULL)
      }
    } else {NULL}
  })
  
  output$plot_content <- renderUI({sidebarLayout(
    sidebarPanel(
      
      imageOutput("smart", inline = TRUE),
      
      selectInput(
        "property", "Select a Property: ", list.dirs("www/DataOut",recursive = FALSE, full.names = FALSE), selected = "SMARTfarms"
      ),
      
      uiOutput("paddocks"),
      
      selectInput(
        "ndvi", "NDVI, GDM, GDM Accumulation Plot? ", c("NDVI","GDM","ACC"), selected = "GDM"
      ),
      
      dateInput("date1", "Start Date: ", value = "2017-04-01"),
      dateInput("date2", "End Date: ", value = Sys.Date()),
      
      textOutput("mostrecentest"),
      
      tableOutput("mostrectab"),
      
      uiOutput("gdmcheck"),
      
      textOutput("accnote"),
      
      imageOutput("parg", inline = TRUE)
      
    ),
    
    mainPanel(
      plotOutput("pasturePlot", height = "800px")
    )
  )
  })
  
  output$map_content <- renderUI({sidebarLayout(
    sidebarPanel(
      imageOutput("smart2", inline = TRUE),
      
      selectInput(
        "property2", "Select a Property: ", list.dirs("www/DataOut",recursive = FALSE, full.names = FALSE), selected = "SMARTfarms"
        ),
      
      selectInput(
        "raworproc", "Display GDM or NDVI: ", c("GDM","NDVI")
      ),
      
      uiOutput("datadate"),
      
      # varSelectInput(
      #   "datadate", "Select Date and Layer: ", dta.in@data[-c(1:10)], multiple = FALSE
      # ),
      
      imageOutput("parg2", inline = TRUE
      ),
      
      tableOutput("result"
      )
      
      #    tableOutput("data")
      #    
    ),
    
    mainPanel(
      leaflet::leafletOutput("mapPlot", height = 900)
    )
  )
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)

# rsconnect::deployApp('C:/R/SentinelPastureProcessing/ShinyApps/GDMPlotting_ver5',account = 'parg')

