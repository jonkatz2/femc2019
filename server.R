options(stringsAsFactors=FALSE)
#roejanwaterchem <- read.csv('data/RoeJan_WaterChem.csv')
#roejanwaterchem["Date_Sampled"] <- as.Date(roejanwaterchem[,"Date_Sampled"], format="%m/%d/%Y")
#sawkillwaterchem <- read.csv('data/Sawkill_WaterChem.csv')
#sawkillwaterchem["Date"] <- as.Date(sawkillwaterchem[,"Date"], format="%m/%d/%Y")
#nutrients <- read.csv('data/Nutrients.csv')

source("R/getData.R")
source("apikey.R")

shinyServer(
	function(input,output,session) {
	    navigation <- reactiveValues(
	        currentview = "mapview",
	        currentmetric = "chemistry"
	    )
	    
	    # Change the buttons to table/chart options
	    observeEvent(input$chartview_open, navigation$currentview <- "chartview")
	    observeEvent(input$tableview_open, navigation$currentview <- "tableview")
	    observeEvent(input$mapview_open, navigation$currentview <- "mapview")
	    
	    # Change the buttons to chem/nutrient/landcover options
	    observeEvent(input$chemistry_open, navigation$currentmetric <- "chemistry", ignoreInit=TRUE)
	    observeEvent(input$nutrients_open, navigation$currentmetric <- "nutrients", ignoreInit=TRUE)
	    observeEvent(input$landcover_open, navigation$currentmetric <- "landuse", ignoreInit=TRUE)
	    
	    # The secondary buttons are created server-side
	    observe({
	        currentview <- navigation$currentview
	        if(currentview %in% c("tableview", "chartview")) {
	            metric <- navigation$currentmetric
	            clicked <- c("redbutton", "redbutton", "redbutton")
	            clicked[c("chemistry", "nutrients", "landuse") == metric] <- "redbutton clicked"
	            output$secondarybuttons <- renderUI(list(
                    column(4, class="leftalign", actionButton("chemistry_open", "Chemistry Data", class=clicked[1])),
                    column(4, class="text-center", actionButton("nutrients_open", "Nutrients Data", class=clicked[2])),
                    column(4, class="rightalign", actionButton("landcover_open", "Land Cover Data", class=clicked[3])),
                    tags$script('
                        $("#chemistry_open").click(function() { clicked(event); });
                        $("#nutrients_open").click(function(event) { clicked(event); });
                        $("#landcover_open").click(function(event) { clicked(event); });'
                    )
                ))
	        } else output$secondarybuttons <- NULL
	    })
	    
	    # get the saw kill locations
	    getSawkillLocations <- reactive({
	        dat <- getLocationData("SawKill", apikey=apikey)
            dat <- locsToSHP(dat)
            dat
	    })
	    
	    # create a map
	    output$mapcontent <- renderLeaflet({
	        dat <- getSawkillLocations()
	        crds <- unname(rowMeans(bbox(dat)))
	        m <- leaflet()
            m <- addTiles(m)
#            m <- addMarkers(m, lng=174.768, lat=-36.852, popup="The birthplace of R")
            m <- addMarkers(m, data = dat)
            m <- setView(m, lat = crds[2], lng = crds[1], zoom = 12)
            m
        })	    
	    
	    
	    # Update site and year selector
	    observe({
	        view <- navigation$currentview
	        if(view == "mapview") {
	            location <- input$location
                if("SawKill" == location) {
                    dat <- getSawkillData()
                    sites <- unique(dat$Site)
                } else if("RoeJan" == location) {
                    dat <- getRoeJanData()
                    sites <- unique(dat$Site_Name)
                }
                updateSelectInput(session, "site", choices=setNames(nm=sites), selected=input$site)
                year <- unique(dat$Year)
                updateSelectInput(session, "year", choices=setNames(nm=year), selected=input$year)
	        }
	    })
	    
	    # Nutrients only apply to saw kill
	    observe({
	        metric <- navigation$currentmetric
	        input$location
	        if(metric == "nutrients") updateRadioButtons(session, "location", selected="SawKill")
	    })
	    
	    # get the saw kill table
	    getSawkillData <- reactive({
	        dat <- getData("SawKill", apikey=apikey)
            dat["Date"] <- as.Date(dat[,"Date"])
            dat
	    })
	    
	    # get the roe jan table
	    getRoeJanData <- reactive({
	        dat <- getData("roejan", apikey=apikey)
	        dat["Date_Sampled"] <- as.Date(dat[,"Date_Sampled"])
	        dat
	    })
	    
	    # get the saw kill nutrient table
	    getNutrientData <- reactive({
	        dat <- getData("nutrients", apikey=apikey)
            dat
	    })
	    
	    # Filter the saw kill table
	    filteredSawKillChem <- reactive({
	        dat <- getSawkillData()
	        
	        daterange <- input$filterdaterange
	        site <- input$site
	        year <- input$year
	        tempMin <- input$tempMin
	        tempMax <- input$tempMax
	        conductivityMin <- input$conductivityMin
	        conductivityMax <- input$conductivityMax
	        turbidityMin <- input$turbidityMin
	        turbidityMax <- input$turbidityMax
	        
	        sitefilter <- yearfilter <- tempminfilter <- tempmaxfilter <- condminfilter <- condmaxfilter <- turbminfilter <- turbmaxfilter <- character()
	        if(length(site)) sitefilter <- paste0("dat$Site %in% c(", paste(site, collapse=", "), ")")
	        if(length(year)) yearfilter <- paste0("dat$Year %in% c(", paste(year, collapse=", "), ")")
	        datefilter <- paste0("dat$Date >= as.Date('", daterange[1], "') & ", "dat$Date <= as.Date('", daterange[2], "')")
	        
	        if(!is.na(tempMin)) tempminfilter <- paste0("dat$Temperature_C >= ", tempMin)
	        if(!is.na(tempMax)) tempmaxfilter <- paste0("dat$Temperature_C <= ", tempMax)
	        if(!is.na(conductivityMin)) condminfilter <- paste0("dat$UncompensatedConductivity_C >= ", conductivityMin)
	        if(!is.na(conductivityMax)) condmaxfilter <- paste0("dat$UncompensatedConductivity_C <= ", conductivityMax)
	        if(!is.na(turbidityMin)) turbminfilter <- paste0("dat$Turbidity_NTU >= ", turbidityMin)
	        if(!is.na(turbidityMax)) turbmaxfilter <- paste0("dat$Turbidity_NTU <= ", turbidityMax)
            
            if(length(sitefilter)) sitefilter <- paste(sitefilter, "& !is.na(dat$Site)")
            if(length(tempminfilter)) tempminfilter <- paste(tempminfilter, "& !is.na(dat$Temperature_C)")
            if(length(tempmaxfilter)) tempmaxfilter <- paste(tempmaxfilter, "& !is.na(dat$Temperature_C)")
            if(length(condminfilter)) condminfilter <- paste(condminfilter, "& !is.na(dat$UncompensatedConductivity_C)")
            if(length(condmaxfilter)) condmaxfilter <- paste(condmaxfilter, "& !is.na(dat$UncompensatedConductivity_C)")
            if(length(turbminfilter)) turbminfilter <- paste(turbminfilter, "& !is.na(dat$Turbidity_NTU)")
            if(length(turbmaxfilter)) turbmaxfilter <- paste(turbmaxfilter, "& !is.na(dat$Turbidity_NTU)")
            if(length(datefilter)) datefilter <- paste(datefilter, "& !is.na(dat$Site)")
            # Remove missing values
	        allfilters <- c(sitefilter, yearfilter, datefilter, tempminfilter, tempmaxfilter, condminfilter, condmaxfilter, turbminfilter, turbmaxfilter)
	        allfilters <- allfilters[unlist(lapply(allfilters, function(x) length(x) > 0))]
            if(length(allfilters)) {
                allfilters <- paste(allfilters, collapse=" & ")	 
                stmt <- paste0("dat[", allfilters, ",]")
                eval(parse(text=stmt))
            }       
	    })
	    
	    # Filter the roe jan table
	    filteredRoeJanChem <- reactive({
	        dat <- getRoeJanData()	        
	        
	        daterange <- input$filterdaterange
	        site <- input$site
	        year <- input$year
	        tempMin <- input$tempMin
	        tempMax <- input$tempMax
	        conductivityMin <- input$conductivityMin
	        conductivityMax <- input$conductivityMax
	        turbidityMin <- input$turbidityMin
	        turbidityMax <- input$turbidityMax
	        
	        sitefilter <- yearfilter <- tempminfilter <- tempmaxfilter <- condminfilter <- condmaxfilter <- turbminfilter <- turbmaxfilter <- character()
	        if(length(site)) sitefilter <- paste0("dat$Site %in% c(", paste(site, collapse=", "), ")")
	        
	        if(length(year)) yearfilter <- paste0("dat$Year %in% c(", paste(year, collapse=", "), ")")
	        datefilter <- paste0("dat$Date_Sampled >= as.Date('", daterange[1], "') & ", "dat$Date_Sampled <= as.Date('", daterange[2], "')")
	        
	        if(!is.na(tempMin)) tempminfilter <- paste0("dat$Temperature_Celsius >= ", tempMin)
	        if(!is.na(tempMax)) tempmaxfilter <- paste0("dat$Temperature_Celsius <= ", tempMax)
	        if(!is.na(conductivityMin)) condminfilter <- paste0("dat$Conductivity_uS >= ", conductivityMin)
	        if(!is.na(conductivityMax)) condmaxfilter <- paste0("dat$Conductivity_uS <= ", conductivityMax)
	        if(!is.na(turbidityMin)) turbminfilter <- paste0("dat$Turbidity_NTU_Edited >= ", turbidityMin)
	        if(!is.na(turbidityMax)) turbmaxfilter <- paste0("dat$Turbidity_NTU_Edited <= ", turbidityMax)
            # Remove missing values
            if(length(sitefilter)) sitefilter <- paste(sitefilter, "& !is.na(dat$Site)")
            if(length(tempminfilter)) tempminfilter <- paste(tempminfilter, "& !is.na(dat$Temperature_Celsius)")
            if(length(tempmaxfilter)) tempmaxfilter <- paste(tempmaxfilter, "& !is.na(dat$Temperature_Celsius)")
            if(length(condminfilter)) condminfilter <- paste(condminfilter, "& !is.na(dat$Conductivity_uS)")
            if(length(condmaxfilter)) condmaxfilter <- paste(condmaxfilter, "& !is.na(dat$Conductivity_uS)")
            if(length(turbminfilter)) turbminfilter <- paste(turbminfilter, "& !is.na(dat$Turbidity_NTU_Edited)")
            if(length(turbmaxfilter)) turbmaxfilter <- paste(turbmaxfilter, "& !is.na(dat$Turbidity_NTU_Edited)")
            if(length(datefilter)) datefilter <- paste(datefilter, "& !is.na(dat$Date_Sampled)")
            
	        allfilters <- c(sitefilter, yearfilter, datefilter, tempminfilter, tempmaxfilter, condminfilter, condmaxfilter, turbminfilter, turbmaxfilter)
	        allfilters <- allfilters[unlist(lapply(allfilters, function(x) length(x) > 0))]
            if(length(allfilters)) {
                allfilters <- paste(allfilters, collapse=" & ")	 
                stmt <- paste0("dat[", allfilters, ",]")
                eval(parse(text=stmt))
            }       
	    })
        
        
        # Filter the nutrient table
	    filteredNutrients <- reactive({
	        dat <- getNutrientData()
	        daterange <- input$filterdaterange
	        site <- input$site
	        year <- input$year
	        
	        sitefilter <- yearfilter <- character()
	        if(length(site)) sitefilter <- paste0("dat$Site %in% c(", paste(site, collapse=", "), ")")
	        
	        if(length(year)) yearfilter <- paste0("dat$Year %in% c(", paste(year, collapse=", "), ")")
	        datefilter <- paste0("dat$DateSampled >= as.Date('", daterange[1], "') & ", "dat$DateSampled <= as.Date('", daterange[2], "')")
	        # Remove missing values
            if(length(sitefilter)) sitefilter <- paste(sitefilter, "& !is.na(dat$Site)")
	        
	        allfilters <- c(sitefilter, yearfilter, datefilter)
	        allfilters <- allfilters[unlist(lapply(allfilters, function(x) length(x) > 0))]
            if(length(allfilters)) {
                allfilters <- paste(allfilters, collapse=" & ")	 
                stmt <- paste0("dat[", allfilters, ",]")
                eval(parse(text=stmt))
            }   
        })
	    
	    # Identify the table that is on screen
	    subsetTable <- reactive({
	        location <- input$location
	        metric <- navigation$currentmetric
	        if(metric == "chemistry") {
                if("SawKill" == location) dat <- filteredSawKillChem()
                else if("RoeJan" == location) dat <- filteredRoeJanChem()
            } else if(metric == "nutrients") {
                dat <- filteredNutrients()
            } else if(metric == "landuse") {
                dat <- NULL
            } else dat <- NULL
            list(data=dat, location=location, metric=metric)
	    })
	    
	    # Identify the dataset that is on screen
	    fullTable <- reactive({
	        location <- input$location
	        metric <- navigation$currentmetric
	        if(metric == "chemistry") {
                if("SawKill" == location) dat <- getSawkillData()
                else if("RoeJan" == location) dat <- getRoeJanData()
            } else if(metric == "nutrients") {
                dat <- getNutrientData()
            } else if(metric == "landuse") {
                dat <- NULL
            } else dat <- NULL
            list(data=dat, location=location, metric=metric)
	    })
	    
	    # Show the table
	    output$tablecontent <- DT::renderDataTable({
	        dat <- subsetTable()
	        dat$data
	    })
	    
	    # download the full table
	    output$downloadfulltable <- downloadHandler(
	        filename=function() {
	            dat <- fullTable()
	            paste0(dat$location, "_",  dat$metric, "_FullData.csv")
            },
            content <- function(file){
	            dat <- fullTable()
                dat <- dat$data
                write.csv(dat, file, row.names=FALSE)
            },
            contentType="text/csv"
	    )
	    
	    # download the partial table
	    output$downloadtableselection <- downloadHandler(
	        filename=function() {
	            dat <- subsetTable()
	            paste0(dat$location, "_",  dat$metric, "_DataSubset.csv")
            },
            content <- function(file){
	            dat <- subsetTable()
                dat <- dat$data
                write.csv(dat, file, row.names=FALSE)
            },
	        contentType="text/csv"
	    )
	}
)
