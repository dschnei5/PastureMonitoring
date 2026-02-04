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
library(dplyr)
library(lubridate)
library(sf)       # Replacement for rgdal
library(leaflet)  # For mapping

#####Versioning####

# 0.1 - Added date selection and cumulative plotting
# 0.2 - Added the paddock total GDM
# 0.3 aka ver 0 - Added data files to app directory dropped the .3 for upload to shinyapps.io
# 0 - UNE Holdings
# 0.4 - UNE Holding Combined Map and Graph, standardised colour ramp, removed GDM decimal places, split UNE properties
# 0.5 - PARG App built on 0.4
# 0.6 - Updated to allow point geoJSOn files 
# 0.7 - Reimplemented mapping using sf and leaflet
# 0.8 - Fixed atomic vector error in single-paddock/single-date scenarios
# 0.9 - Removed CSV dependency, app now uses only GeoJSON files for both plots and maps

#################################################################################################################

# Define UI for application that draws a histogram
ui <- navbarPage("UNE PARG Pasture Monitoring App",
                 theme=shinytheme(theme = "flatly"),
                 
                 tabPanel("Sentinel Pasture Estimate Maps",uiOutput("map_content")),
                 
                 tabPanel("Sentinel Pasture Estimate Plots",uiOutput("plot_content"))
                 
)



######################################################################################################################


# Define server logic required to create plots
server <- function(input, output, session) {
  
  # Reactive value to sync property selection between tabs
  selected_property <- reactiveVal("SMARTfarms")
  
  # When property2 changes on map tab, update the reactive value and plots tab property
  observeEvent(input$property2, {
    selected_property(input$property2)
    updateSelectInput(session, "property", selected = input$property2)
  })
  
  # When property changes on plots tab, update the reactive value and map tab property
  observeEvent(input$property, {
    selected_property(input$property)
    updateSelectInput(session, "property2", selected = input$property)
  })
  
  # Data for Plot and Map Apps (GeoJSON via sf - single data source)
  dta.in <- reactive({
    geojson_file <- paste0("www/DataOut/",selected_property(),"/GeoJson/",selected_property(),"_GDM_AllAvailableDates.geojson")
    if(file.exists(geojson_file)) {
      # Read using sf (quietly)
      tryCatch({
        data <- st_read(geojson_file, quiet = TRUE)
        # Drop geometry for plot operations, convert to regular dataframe
        st_drop_geometry(data)
      }, error = function(e) {
        return(NULL)
      })
    } else {
      NULL
    }
  }) 
  
  # Data for Map App (GeoJSON via sf - keeps geometry)
  dta.in2 <- reactive({
    geojson_file <- paste0("www/DataOut/",input$property2,"/GeoJson/",input$property2,"_GDM_AllAvailableDates.geojson")
    if(file.exists(geojson_file)) {
      # Read using sf (quietly)
      tryCatch({
        st_read(geojson_file, quiet = TRUE)
      }, error = function(e) {
        return(NULL)
      })
    } else {
      NULL
    }
  }) 
  
  output$paddocks <- renderUI({
    req(dta.in())
    selectInput("paddocks2", "Select a Paddock: ", choices = sort(dta.in()$aPADD_NAME, decreasing = FALSE), multiple = TRUE)
  })
  
  output$gdmcheck <- renderUI({
    if(input$ndvi == "GDM"){
      checkboxInput(inputId = "switch.1",label = "Display GDM as paddock total in kg", value = FALSE)
    } else {
      if(input$ndvi == "ACC") {
        checkboxInput(inputId = "switch.2",label = "Display accumulation as stacked yearly plot", value = FALSE)
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
    req(dta.in())
    
    if (is.null(input$paddocks2) | input$ndvi == "ACC") {
      return(NULL)
    } else if (input$ndvi == "NDVI") {
      
      dta.in2 <- as.data.frame(t(dta.in()[-c(1:3)]))
      colnames(dta.in2) <- dta.in()$aPADD_NAME
      # FIX: Added drop = FALSE to prevent atomic vector error
      dta.in2 <- dta.in2[grepl(input$ndvi,(row.names(dta.in2))), , drop = FALSE]
      dta.in2$DATE <- as.Date(unlist(strapplyc(rownames(dta.in2), "\\d+-\\d+-\\d+", simplify = TRUE)))
      padds2plot <- function (x) { dta.in2[,grep(paste0("^",x,"$"),names(dta.in2))] }
      pads <- as.character(input$paddocks2)
      dta.in3 <- cbind(Date = dta.in2$DATE,as.data.frame(sapply(pads,padds2plot)))
      dta.in3 <- dta.in3[complete.cases(dta.in3), ]
      if(nrow(dta.in3) > 0) {
        dta.in3 <- dta.in3[which.max(dta.in3$Date), ]
        dta.in3$Date <- format(dta.in3$Date,'%d %b %Y')
        return(dta.in3)
      } else { return(NULL) }
      
    } else {
      # GDM Logic
      # Determine cols to drop based on NDVI selection and switch
      cols_to_drop <- if(input$ndvi == "GDM") 2 else 3 
      
      dta.in2 <- as.data.frame(t(dta.in()[-c(1:cols_to_drop)]))
      colnames(dta.in2) <- dta.in()$aPADD_NAME
      
      # FIX: Added drop = FALSE to prevent atomic vector error
      dta.in2 <- dta.in2[grepl(input$ndvi,(row.names(dta.in2))), , drop = FALSE]
      dta.in2$DATE <- as.Date(unlist(strapplyc(rownames(dta.in2), "\\d+-\\d+-\\d+", simplify = TRUE)))
      
      if (input$ndvi == "GDM" & isTRUE(input$switch.1)) {
        # FIX: Changed subset to 1:3 to ensure bHECTARES (col 3) is included
        descript.dta <- dta.in()[, 1:3] 
        # Calculate Total GDM (kg) = GDM (kg/ha) * Area (ha)
        val_cols <- (dim(dta.in2)[2] - 1)
        if(val_cols > 0){
          # Transpose/Matrix math adjustment
          dta.in2[1:val_cols] <- as.data.frame(as.matrix(dta.in2[1:val_cols]) %*% diag(descript.dta$bHECTARES))
        }
      }
      
      padds2plot <- function (x) { dta.in2[,grep(paste0("^",x,"$"),names(dta.in2))] }
      pads <- as.character(input$paddocks2)
      dta.in3 <- cbind(Date = dta.in2$DATE,as.data.frame(sapply(pads,padds2plot)))
      dta.in3 <- dta.in3[complete.cases(dta.in3), ]
      
      if(nrow(dta.in3) > 0) {
        dta.in3 <- dta.in3[which.max(dta.in3$Date), ]
        dta.in3$Date <- format(dta.in3$Date,'%d %b %Y')
        return(dta.in3)
      } else { return(NULL) }
    }
  }, align = 'c')
  
  output$pasturePlot <- renderPlot({
    req(dta.in())
    
    if (is.null(input$paddocks2)) {
      return(NULL)
    } else {
      if (input$ndvi == "GDM") {
        dta.in2 <- as.data.frame(t(dta.in()[-c(1:2)]))
        # FIX: Ensure bHECTARES is available (col 3)
        descript.dta <- dta.in()[, 1:3]
      } else {
        dta.in2 <- as.data.frame(t(dta.in()[-c(1:3)]))
        descript.dta <- dta.in()[, 1:3]
      }
      colnames(dta.in2) <- dta.in()$aPADD_NAME
      
      if (input$ndvi != "ACC") {
        # FIX: Added drop = FALSE
        dta.in2 <- dta.in2[grepl(input$ndvi,(row.names(dta.in2))), , drop = FALSE]
        dta.in2$DATE <- as.Date(unlist(strapplyc(rownames(dta.in2), "\\d+-\\d+-\\d+", simplify = TRUE)))
        dta.in2 <- dta.in2[dta.in2$DATE >= input$date1 & dta.in2$DATE <= input$date2, , drop = FALSE]
        
        if (input$ndvi == "GDM" & isTRUE(input$switch.1)) {
          # Calculate Total
          val_cols <- (dim(dta.in2)[2] - 1)
          if(val_cols > 0) {
            dta.in2[1:val_cols] <- as.data.frame(as.matrix(dta.in2[1:val_cols]) %*% diag(descript.dta$bHECTARES))
          }
        }
        
        padds2plot <- function (x) { dta.in2[,grep(paste0("^",x,"$"),names(dta.in2))] }
        pads <- as.character(input$paddocks2)
        dta.in3 <- cbind(Date = dta.in2$DATE,as.data.frame(sapply(pads,padds2plot)))
        dta.in3 <- dta.in3[complete.cases(dta.in3), ]
        
        if(nrow(dta.in3) > 0){
          a <- dim(dta.in3)[2] * 1
          dta.in4 <- gather(dta.in3[-c(1)])
          dta.in4$index <- match(dta.in4$key,names(dta.in3))
          dta.in4$Date <- dta.in3$Date
          ggplot(data = dta.in4[dta.in4$index %in% c(2:a),],
                 aes(x = Date, y = value, colour = key)) +
            geom_point(size = 3) +
            geom_line(linewidth = 1) +
            theme_light() +
            xlab("Observation Date") +
            ylab(input$ndvi) +
            scale_x_date(date_breaks = "3 month", date_labels = ("%b-%y")) +
            theme(
              text = element_text(size = 20),
              axis.text.x = element_text(angle = 60, hjust = 1)) +
            labs(colour = "Paddock")
        }
      } else {
        # ACCUMULATION LOGIC
        if (!input$switch.2) {
          # Standard Cumulative Plot
          # FIX: Added drop = FALSE
          dta.in2 <- dta.in2[grepl("GDM",(row.names(dta.in2))), , drop = FALSE]
          dta.in2 <- dta.in2[, colSums(is.na(dta.in2)) != nrow(dta.in2), drop = FALSE]
          dta.in2$DATE <- as.Date(unlist(strapplyc(rownames(dta.in2), "\\d+-\\d+-\\d+", simplify = TRUE)))
          dta.in2 <- dta.in2[dta.in2$DATE >= input$date1 & dta.in2$DATE <= input$date2, , drop = FALSE]
          
          if(nrow(dta.in2) > 0) {
            dta.in2$NUMDAYS <- dta.in2$DATE - input$date1
            dta.in2$NUMDAYS <- c(as.numeric(dta.in2$NUMDAYS[1]),as.numeric(diff(dta.in2$NUMDAYS)))
            
            getacc <- function(x) {
              dta <- x
              a <- dim(dta)[2] - 2
              b <- seq_along(names(dta[1:a]))
              dta.out <- as.data.frame(sapply(dta[,b],diff))
              dta.out <- rbind(0,dta.out)
              # pgr <- dta.out/dta$NUMDAYS 
              dta.out[dta.out <= 0] <- 0
              dta.out <- cumsum(dta.out)
              dta[1:a] <- dta.out
              return(dta)
            }
            
            dta.in2 <- getacc(dta.in2)
            padds2plot <- function (x) { dta.in2[,grep(paste0("^",x,"$"),names(dta.in2))] }
            pads <- as.character(input$paddocks2)
            dta.in3 <- cbind(Date = dta.in2$DATE,as.data.frame(sapply(pads,padds2plot)))
            dta.in3 <- dta.in3[complete.cases(dta.in3), ]
            
            if(nrow(dta.in3) > 0) {
              a <- dim(dta.in3)[2] * 1
              dta.in4 <- gather(dta.in3[-c(1)])
              dta.in4$index <- match(dta.in4$key,names(dta.in3))
              dta.in4$Date <- dta.in3$Date
              ggplot(data = dta.in4[dta.in4$index %in% c(2:a),],
                     aes(x = Date, y = value, colour = key)) +
                geom_point(size = 3) +
                geom_line(linewidth = 1) +
                theme_light() +
                xlab("Observation Date") +
                ylab("Accumulated GDM (kg/ha)") +
                scale_x_date(date_breaks = "1 month", date_labels = ("%b-%y")) +
                theme(
                  text = element_text(size = 20),
                  axis.text.x = element_text(angle = 60, hjust = 1)) +
                labs(colour = "Paddock")
            }
          }
        } else {
          # Stacked Yearly Plot (New Logic)
          # FIX: Added drop = FALSE
          dta.in2 <- dta.in2[grepl("GDM",(row.names(dta.in2))), , drop = FALSE]
          dta.in2 <- dta.in2[, colSums(is.na(dta.in2)) != nrow(dta.in2), drop = FALSE]
          dta.in2$DATE <- as.Date(unlist(strapplyc(rownames(dta.in2), "\\d+-\\d+-\\d+", simplify = TRUE)))
          dta.in2 <- dta.in2[dta.in2$DATE >= input$date1 & dta.in2$DATE <= input$date2, , drop = FALSE]
          
          if(nrow(dta.in2) > 0) {
            dta.in2$NUMDAYS <- dta.in2$DATE - input$date1
            dta.in2$NUMDAYS <- c(as.numeric(dta.in2$NUMDAYS[1]),as.numeric(diff(dta.in2$NUMDAYS)))
            
            getacc <- function(x) {
              dta <- x
              a <- dim(dta)[2] - 2
              b <- seq_along(names(dta[1:a]))
              dta.out <- as.data.frame(sapply(dta[,b],diff))
              dta.out <- rbind(0,dta.out)
              dta.out[dta.out <= 0] <- 0
              dta.out <- cumsum(dta.out)
              dta[1:a] <- dta.out
              return(dta)
            }
            
            dta.in2 <- getacc(dta.in2)
            padds2plot <- function (x) { dta.in2[,grep(paste0("^",x,"$"),names(dta.in2))] }
            pads <- as.character(input$paddocks2)
            dta.in3 <- cbind(Date = dta.in2$DATE,as.data.frame(sapply(pads,padds2plot)))
            dta.in3$Year <- factor(year(dta.in3$Date))
            
            all_years <- unique(dta.in3$Year)
            plot_data <- data.frame()
            
            for (curr_year in all_years) {
              year_data <- dta.in3[year(dta.in3$Date) == as.numeric(as.character(curr_year)), ]
              paddocks_in_year <- names(year_data)[names(year_data) %in% pads]
              
              for (paddock_col in paddocks_in_year) {
                paddock_values <- year_data[[paddock_col]]
                if(length(paddock_values) > 0) {
                  dates_in_year <- year_data$Date
                  days_of_year <- yday(dates_in_year)
                  first_value <- paddock_values[1]
                  paddock_values_relative <- paddock_values - first_value
                  
                  year_padd_data <- data.frame(
                    DayOfYear = c(1, days_of_year), 
                    value = c(0, paddock_values_relative), 
                    key = paddock_col,
                    Year = curr_year
                  )
                  plot_data <- rbind(plot_data, year_padd_data)
                }
              }
            }
            
            if(nrow(plot_data) > 0) {
              plot_data <- plot_data[!is.na(plot_data$value), ]
              ggplot(data = plot_data,
                     aes(x = DayOfYear, y = value, colour = Year, linetype = key)) +
                geom_point(size = 2) +
                geom_line(linewidth = 1, na.rm = TRUE) +
                theme_light() +
                xlab("Day of Year") +
                ylab("Accumulated GDM (kg/ha)") +
                scale_x_continuous(breaks = seq(0, 365, by = 30)) +
                theme(
                  text = element_text(size = 20),
                  axis.text.x = element_text(angle = 60, hjust = 1)) +
                labs(colour = "Year", linetype = "Paddock")
            }
          }
        }
      }
    }
  })
  
  output$datadate <- renderUI({
    req(dta.in2())
    # Note: sf objects behave like data.frames, so names() works directly. No @data needed.
    # Filter for GDM or NDVI columns based on input$raworproc
    cols <- names(dta.in2())
    date_cols <- cols[grep(input$raworproc, cols)]
    a <- sort(date_cols, decreasing = TRUE)
    selectInput("datadate2", "Select Date and Layer: ", a, multiple = FALSE)
  })
  
  output$result <- renderTable({
    req(dta.in2(), input$datadate2)
    
    # sf object handling - drop geometry for table calc if preferred, or just subset
    # dta.t will contain the GDM/NDVI values
    if (length(dta.in2()) > 0){
      
      # Select specific column + PADD_NAME + HECTARES
      dta.t <- dta.in2() %>% dplyr::select(all_of(input$datadate2), aPADD_NAME, bHECTARES)
      # sf keeps geometry, we can drop it for table stats
      dta.t <- st_drop_geometry(dta.t)
      
      # Rename columns for easier access
      # Note: input$datadate2 is the value column
      names(dta.t)[names(dta.t) == input$datadate2] <- "gdm"
      names(dta.t)[names(dta.t) == "aPADD_NAME"] <- "padd.nam"
      names(dta.t)[names(dta.t) == "bHECTARES"] <- "pad.size"

      # Logic for different property types
      is_polygon <- any(grepl("POLYGON", st_geometry_type(dta.in2(), by_geometry = FALSE)))
      is_point <- any(grepl("POINT", st_geometry_type(dta.in2(), by_geometry = FALSE)))
      
      if (input$property2 != "SMARTfarms" & is_polygon & grepl("GDM", input$datadate2)) {
        dta.t$gdm.tot <- dta.t$gdm * dta.t$pad.size
        dta.out <- as.data.frame(input$property2)
        dta.out$Property.GDM <- as.character(round(sum(dta.t$gdm.tot, na.rm = TRUE)/sum(dta.t$pad.size, na.rm = TRUE), 0))
        dta.out$Date <- as.character(as.Date(substr(input$datadate2, start = 9, stop = 19), format = "%Y.%m.%d"))
        colnames(dta.out) <- c("Property", "Property Average GDM (kg/ha)", "Image Date")
        return(dta.out)
        
      } else if (input$property2 == "SMARTfarms" & grepl("GDM", input$datadate2)){
        dta.t$gdm.tot <- dta.t$gdm * dta.t$pad.size
        property.nams <- c("Clarkes_Farm","Kirby","Laureldale","Maxwelton","Newholme","Toombs","Trevenna","Tullimba")
        dta.out <- data.frame()
        for (i in seq_along(property.nams)){
          a <- property.nams[i]
          # Filter by property name match
          sub_data <- dta.t[grep(a, dta.t$padd.nam), ]
          if(nrow(sub_data) > 0) {
            val <- round(sum(sub_data$gdm.tot, na.rm = TRUE) / sum(sub_data$pad.size, na.rm = TRUE), 0)
          } else {
            val <- NA
          }
          dta.out1 <- data.frame(Property = a, 
                                 Property.GDM = as.character(val),
                                 Date = as.character(as.Date(substr(input$datadate2,start = 9,stop = 19), format = "%Y.%m.%d")),
                                 stringsAsFactors = FALSE)
          colnames(dta.out1) <- c("Property","Property Average GDM (kg/ha)","Image Date")
          dta.out <- rbind(dta.out,dta.out1)
        }
        return(dta.out)
        
      } else if (is_point & grepl("GDM", input$datadate2)){
        dta.out <- as.data.frame(input$property2)
        dta.out$Property.GDM <- mean(dta.t$gdm, na.rm = TRUE)
        dta.out$Date <- as.character(as.Date(substr(input$datadate2,start = 9,stop = 19), format = "%Y.%m.%d"))
        colnames(dta.out) <- c("Property","Property Average GDM (kg/ha)","Image Date")
        return(dta.out)
      } else {NULL}
    } else {NULL}
  }, align = "c")
  
  # MAP IMPLEMENTATION USING LEAFLET
  output$mapPlot <- renderLeaflet({
    req(dta.in2(), input$datadate2)
    
    data <- dta.in2()
    # Check if selected column exists
    if (!input$datadate2 %in% names(data)) return(NULL)
    
    # Extract values for palette
    vals <- data[[input$datadate2]]
    
    # Create color palette (Same as original)
    colfunc <- colorRampPalette(c("#DD0000","#DA6500","#D7C900","#7FD500","#1BD200","#00DD00","#00BB00","#008800","#006600","#003300"))
    col.50 <- colfunc(50)
    pal <- colorNumeric(col.50, domain = vals, na.color = "transparent")
    
    # Determine labels
    if (grepl("GDM", input$datadate2)) {
      lbls <- paste(data$aPADD_NAME, paste(round(vals, 0), "kg/ha"), sep = "\n")
    } else {
      lbls <- paste(data$aPADD_NAME, round(vals, 4), sep = "\n")
    }
    
    # Initialize Map
    map <- leaflet(data) %>%
      addProviderTiles(providers$Esri.WorldImagery)
    
    # Add layers based on geometry type
    geom_type <- st_geometry_type(data, by_geometry = FALSE)
    
    if (any(grepl("POLYGON", geom_type))) {
      map <- map %>%
        addPolygons(
          fillColor = ~pal(vals),
          weight = 2,
          opacity = 1,
          color = "darkgrey",
          dashArray = "3",
          fillOpacity = 0.5,
          label = lbls,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE)
        )
    } else if (any(grepl("POINT", geom_type))) {
      map <- map %>%
        addCircleMarkers(
          fillColor = ~pal(vals),
          radius = 8,
          stroke = TRUE,
          color = "darkgrey",
          weight = 2,
          fillOpacity = 0.8,
          label = lbls
        )
    }
    
    # Add Legend
    map %>% addLegend(pal = pal, values = vals, opacity = 1.0, title = as.character(input$datadate2), position = "bottomright")
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
      
      imageOutput("parg2", inline = TRUE
      ),
      
      tableOutput("result"
      )
    ),
    
    mainPanel(
      leafletOutput("mapPlot", height = 900)
    )
  )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)