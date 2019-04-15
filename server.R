options(stringsAsFactors=FALSE)

if(!length(suppressMessages(webshot:::find_phantom()))) webshot::install_phantomjs()

source("R/getData.R")
source("apikey.R")

#columnNames <- read.csv('data/columnNames.csv')

columnNames <- makeColNameTab(apikey)


shinyServer(
	function(input,output,session) {
	    	    
	    navigation <- reactiveValues(
	        currentview = "mapview",
	        currentmap = "sites",
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
	            output$secondarybuttons <- renderUI(div(id="metricbuttons",
                    column(4, class="leftalign", actionButton("chemistry_open", "Chemistry Data", class=clicked[1])),
                    column(4, class="text-center", actionButton("nutrients_open", "Nutrients Data", class=clicked[2])),
                    column(4, class="rightalign", actionButton("landcover_open", "Land Cover Data", class=clicked[3])),
                    tags$script('
                        $("#chemistry_open").click(function(event) { clickedmetric(event); });
                        $("#nutrients_open").click(function(event) { clickedmetric(event); });
                        $("#landcover_open").click(function(event) { clickedmetric(event); });'
                    )
                ))
	        } else {
	            map <- navigation$currentmap
	            clicked <- c("redbutton", "redbutton")
	            clicked[c("sites", "basins") == map] <- "redbutton clicked"
	            output$secondarybuttons <- renderUI(div(id="mapbuttons",
                    column(6, class="text-center", actionButton("sites_open", "Plots", class=clicked[1])),
                    column(6, class="text-center", actionButton("basins_open", "Sub Basins", class=clicked[2])),
                    tags$script('
                        $("#sites_open").click(function(event) { clickedmap(event); });
                        $("#basins_open").click(function(event) { clickedmap(event); });'
                    )
                ))
	        }
	    })
	    
	    # Change the buttons to table/chart options
	    observeEvent(input$sites_open, navigation$currentmap <- "sites", ignoreInit=TRUE)
	    observeEvent(input$basins_open, navigation$currentmap <- "basins", ignoreInit=TRUE)
	    
	    # get the saw kill locations
	    getSawkillLocations <- reactive({
	        dat <- getLocationData("SawKill", apikey=apikey)
            dat <- locsToSHP(dat)
            dat
	    })
	    
	    # Download site location data
	    output$downloadsitedata <- downloadHandler(
	        file=function() {
	            "Sawkill_RoeJan_sitedata.csv"
	        },
	        content=function(file) {
	            dat_s <- getSawkillLocations()
	            write.csv(dat_s@data, file, row.names=FALSE)
	        },
	        contentType="text/csv"
	    )
	    
	    # Make the map of sawkill points
	    sitesmap <- reactive({
	        dat <- getSawkillLocations()
	        crds <- unname(rowMeans(bbox(dat)))
            
            ## Spelling error: Longtiude!!
	        content <- paste0("<b>", dat@data[,'Site_Name'], "</b><br/>", "Site: ", dat@data[,'Site_Numbe'], "&nbsp; &nbsp; &nbsp; (", dat@data[,'Latitude'], ", ", dat@data[,'Longtiude'], ")<br/>", dat@data[,'Site_Descr'], '<div style=text-align:center;><button class="btn btn-default action-button redbutton narrowbutton" onclick="gotosite(\'', dat@data[,'Site_Numbe'], '\')" type="button">View Data</button></div>')
            labels <- dat@data[,"Site_Numbe"]
            direction <- rep(c("top", "left", "bottom", "right"), ceiling(length(labels)/4))
	        m <- leaflet(dat)
            m <- addTiles(m)
            for(i in 1:nrow(dat))
            m <- addCircleMarkers(m, 
                lng = as.numeric(dat@data[i, "Longtiude"]), 
                lat = as.numeric(dat@data[i,"Latitude"]), 
                popup = content[i], 
                label = labels[i], 
                labelOptions = labelOptions(noHide = TRUE, className="leaflet-label-small", direction=direction[i]), 
                options=pathOptions(className="leaflet-marker-hover")
            )
            m <- setView(m, lat = crds[2], lng = crds[1], zoom = 12)
            m
	    })
	    
	    # read the saw kill basins
	    getSawkillBasins <- reactive({
	        dat <-readOGR("data/Subbasins_revised", "Subbasins_revised")
	        dat <- spTransform(dat, CRS("+init=epsg:4326"))
	        dat
	    })
	    
	    # Make the map of sawkill subbasins
	    basinsmap <- reactive({
	        dat <-getSawkillBasins()
	        crds <- unname(rowMeans(bbox(dat)))
            colorpal1 <- brewer.pal(7, "RdYlBu")
            colorpal2 <- brewer.pal(7, "PRGn")
            colorpal3 <- rev(brewer.pal(6, "BrBG"))
            colorpal <- c(colorpal1, colorpal2, colorpal3)
	        landuse <- getSawkillLandUseData()
            lupct <- colnames(landuse)
            lupct <- lupct[grepl("pct$", lupct, ignore.case=TRUE)]
	        ludat <- unlist(sapply(dat@data[,'Letter_ID'], function(x) {
	            letterid <- paste0("<tr><th>Basin Letter ID</th><th>", x, "</th></tr>")
	            watershed <- paste0("<tr><td>Watershed</td><td>", dat@data[dat@data[,'Letter_ID'] == x, 'WSHED3'], "</td></tr>")
	            acres <- paste0("<tr><td>Acres</td><td>", landuse[landuse['sub_basin'] == x, 'Acres'], "</td></tr>")
	            otherstat <- unlist(lapply(lupct, function(y) {
	                paste0("<tr><td>", strsplit(y, "_")[[1]][1], "</td><td>", paste0(landuse[landuse['sub_basin'] == x, y], '%'), "</td></tr>")
	            }))
	            paste0("<table class='watershedstats'>", letterid, watershed, acres, paste0(otherstat, collapse=""), "</table>", collapse="")
	        }))
	        content <- paste0(ludat, '<div style=text-align:center;><button class="btn btn-default action-button redbutton narrowbutton" onclick="gotobasin(\'', dat@data[,'Letter_ID'], '\')" type="button">View Data</button></div>')
#	        content <- paste0("<b>Basin Letter ID: ", dat@data[,'Letter_ID'], "</b><br/>", "Watershed: ", dat@data[,'WSHED3'], "<br/>Acres: ", dat@data[,'Acres'], '<div style=text-align:center;><button class="btn btn-default action-button redbutton narrowbutton" onclick="gotobasin(\'', dat@data[,'Letter_ID'], '\')" type="button">View Data</button></div>')
            labels <- dat@data[,"Letter_ID"]
#            direction <- rep(c("top", "left", "bottom", "right"), ceiling(length(labels)/4))
	        m <- leaflet(dat)
            m <- addTiles(m)
            for(i in 1:nrow(dat))
            m <- addPolygons(m,
                data = dat[i, ],
                color = "#444444", 
                weight = 1, 
                smoothFactor = 0.5,
                opacity = 1, 
                fillOpacity = 0.5,
                fillColor = colorpal[i],
                highlightOptions = highlightOptions(
                    color = "white", 
                    weight = 2,
                    bringToFront = TRUE
                ),
                popup = content[i],
                label = labels[i]
            )
            m <- addLabelOnlyMarkers(m, 
                lng=coordinates(dat)[,1],
                lat=coordinates(dat)[,2],
                labelOptions = labelOptions(
                    noHide = TRUE, 
                    className="leaflet-label-hidden",
                    direction="top"
                ),
                label = labels
            )
            m <- setView(m, lat = crds[2], lng = crds[1], zoom = 12)
            m
	    })

	    # display the map
	    output$mapcontent <- renderLeaflet({
	        currentmap <- navigation$currentmap
	        if(currentmap == "sites") map <- sitesmap()
	        else map <- basinsmap()
	        map
        })	    
        
        # Go to the site selected from the map
        observeEvent(input$gotosite, {
            location <- input$location
            site <- input$gotosite
            metric <- navigation$currentmetric
            if(metric == "chemistry") {
                if(location == "SawKill") dat <- getSawKillChemData()
                else if(location == "RoeJan") dat <- getRoeJanChemData()
            } else if(metric == "nutrients") {
                if(location == "SawKill") dat <- getSawKillNutrientData()
                else if(location == "RoeJan") dat <- getRoeJanNutrientData()
            } else if(metric == "landuse") {
                if(location == "SawKill") dat <- getSawKillLandUseData()
                else if(location == "RoeJan") dat <- getRoeLandUseData()
            }
            sites <- unique(dat[,columnName('site', location, "chemistry", columnNames)])
#            updateRadioButtons(session, 'location', selected=loc)
            updateSelectizeInput(session, 'site', choices=setNames(nm=sites), selected=site)
        }, ignoreInit=TRUE)
        
        
        
        
        # Update the filters with sawkill-only filters
        observeEvent(input$location, {
            session$sendCustomMessage("togglesawkillonlyfilters", message=list(location=input$location))
        })
	    
	    # toggle between chemistry and nutrient filters
	    observeEvent(navigation$currentmetric, {
            session$sendCustomMessage("togglenutrientfilters", message=list(type=navigation$currentmetric))
        }, ignoreInit=TRUE)
	    
	    # Download site map as image
	    output$downloadmapimage <- downloadHandler(
	        file=function() {
	            "Sawkill_RoeJan_sites.png"
	        },
	        content=function(file) {
	            currentmap <- navigation$currentmap
	            if(currentmap == "sites") map <- sitesmap()
	            else map <- basinsmap()
	            map
                mapview::mapshot(map, file = file)
	        },
	        contentType="image/png"
	    )
	    
	    # Download site map as zipped shapefile
	    output$downloadmapshapefile <- downloadHandler(
	        file=function() {
	            paste0(input$location, "_", navigation$currentmap, ".zip")
	        },
	        content=function(file) {
	            location <- input$location
	            currentmap <- navigation$currentmap
                if(location == "SawKill") {
                    if(currentmap == "sites") map <- getSawkillLocations()
                    else map <- getSawkillBasins()
                } else {
                    if(currentmap == "sites") map <- getRoejanLocations()
                    else map <- getRoejanBasins()
                }
	            rgdal::writeOGR(map, paste0(location, currentmap), paste0(location, currentmap), driver="ESRI Shapefile", overwrite_layer=TRUE, delete_dsn=TRUE)
	            zip(file, paste0(location, currentmap))
	        },
	        contentType="application/zip"
	    )
	    
	    # Update site and year selector
	    observe({
	        view <- navigation$currentview
            if(view == "tableview") {
                location <- input$location
                metric <- navigation$currentmetric
                if(metric == "chemistry") {
                    if(location == "SawKill") dat <- getSawKillChemData()
                    else if(location == "RoeJan") dat <- getRoeJanChemData()
                } else if(metric == "nutrients") {
                    if(location == "SawKill") dat <- getSawKillNutrientData()
                    else if(location == "RoeJan") dat <- getRoeJanNutrientData()
                } else if(metric == "landuse") {
                    if(location == "SawKill") dat <- getSawKillLandUseData()
                    else if(location == "RoeJan") dat <- getRoeLandUseData()
                }
                if(length(dat)) {
                    sites <- unique(dat[,columnName('site', location, metric, columnNames)])
                    updateSelectInput(session, "site", choices=setNames(nm=sites), selected=input$site)
                    year <- unique(dat[,columnName('year', location, metric, columnNames)])
                    updateSelectInput(session, "year", choices=setNames(nm=year), selected=input$year)
                }
            }
	    }, priority=1000) # execute before the from-map site 
	    
#	    # Nutrients only apply to saw kill
#	    observe({
#	        metric <- navigation$currentmetric
#	        input$location
#	        if(metric == "nutrients") updateRadioButtons(session, "location", selected="SawKill")
#	    })
	    
	    # get the saw kill table from FEMC
	    getSawKillChemData <- reactive({
	        dat <- getData("sawkill", "chemistry", apikey=apikey)
	        metric <- navigation$currentmetric
	        datecol <- columnName('date', input$location, metric, columnNames)
	        dat[,datecol] <- as.Date(dat[,datecol])
#	        cat("SawKill\n")
#            print(colnames(dat))
            dat
	    })
	    
	    # get the roe jan table from FEMC
	    getRoeJanChemData <- reactive({
	        dat <- getData("roejan", "chemistry", apikey=apikey)
	        metric <- navigation$currentmetric
	        datecol <- columnName('date', input$location, metric, columnNames)
	        dat[,datecol] <- as.Date(dat[,datecol])
#	        cat("roejan\n")
#            print(colnames(dat))
	        dat
	    })
	    
	    # get the saw kill nutrient table from FEMC
	    getSawKillNutrientData <- reactive({
	        dat <- getData("sawkill", "nutrients", apikey=apikey)
	        metric <- navigation$currentmetric
	        datecol <- columnName('date', input$location, metric, columnNames)
	        dat[,datecol] <- as.Date(dat[,datecol])
#	        cat("nutrients\n")
#            print(colnames(dat))
            dat
	    })
	    
	    # get the roe jan nutrient table from FEMC
	    getRoeJanNutrientData <- reactive({
#	        dat <- getData("roejan", "nutrients", apikey=apikey)
##	        cat("nutrients\n")
##            print(colnames(dat))
#            dat
            NULL
	    })

	    # get the saw kill land use table from FEMC
	    getSawkillLandUseData <- reactive({
	        dat <- read.csv("data/LandUseData_Cleaned.csv", stringsAsFactors=FALSE)
#	        cat("land use\n")
#            print(colnames(dat))
            dat
	    })
	    
	    # get the saw kill subbasin land use table from FEMC
	    getSawkillLandUseDataBySubbasin <- reactive({
	        dat <- read.csv("data/SawKillSubbasin_MasterMonitoring_Data_cleaned.csv", stringsAsFactors=FALSE)
#	        cat("land use\n")
#            print(colnames(dat))
            dat
	    })
	    
#	    # get the saw kill land use table from FEMC
#	    getSawkillLandUseData <- reactive({
#	        dat <- getData("sawkill", "landuse", apikey=apikey)
##	        cat("land use\n")
##            print(colnames(dat))
#            dat
#	    })
#	    
#	    # get the saw kill land use table from FEMC
#	    getRoeJanLandUseData <- reactive({
#	        dat <- getData("roejan", "landuse", apikey=apikey)
##	        cat("land use\n")
##            print(colnames(dat))
#            dat
#	    })
	    
	    # Filter the saw kill chemistry table
	    filteredSawKillChem <- reactive({
	        dat <- getSawKillChemData()
	        metric <- navigation$currentmetric
	        location <- input$location
	        daterange <- input$filterdaterange
	        site <- input$site
	        year <- input$year
	        tempMin <- input$tempMin
	        tempMax <- input$tempMax
	        conductivityMin <- input$conductivityMin
	        conductivityMax <- input$conductivityMax
	        turbidityMin <- input$turbidityMin
	        turbidityMax <- input$turbidityMax
	        ecoliMin <- input$ecoliMin
	        ecoliMax <- input$ecoliMax
	        enterococciMin <- input$enterococciMin
	        enterococciMax <- input$enterococciMax
	        totcoliformMin <- input$totcoliformMin
	        totcoliformMax <- input$totcoliformMax
	        
	        sitefilter <- yearfilter <- tempminfilter <- tempmaxfilter <- condminfilter <- condmaxfilter <- turbminfilter <- turbmaxfilter <- ecoliminfilter <- ecolimaxfilter <- enterocminfilter <- enterocmaxfilter <- totcoliformminfilter <- totcoliformmaxfilter <- character()
	        
	        if(length(site)) sitefilter <- paste0("dat[,'", columnName('site', location, metric, columnNames), "'] %in% c(", paste(site, collapse=", "), ")")
	        
	        if(length(year)) yearfilter <- paste0("dat[,'", columnName('year', location, metric, columnNames), "'] %in% c(", paste(year, collapse=", "), ")")
	        datefilter <- paste0("dat[,'", columnName('date', location, metric, columnNames), "'] >= as.Date('", daterange[1], "') & ", "dat[,'", columnName('date', location, metric, columnNames), "'] <= as.Date('", daterange[2], "')")
	        
	        if(!is.na(tempMin)) tempminfilter <- paste0("dat[,'", columnName('temperature', location, metric, columnNames), "'] >= ", tempMin)
	        if(!is.na(tempMax)) tempmaxfilter <- paste0("dat[,'", columnName('temperature', location, metric, columnNames), "'] <= ", tempMax)
	        if(!is.na(conductivityMin)) condminfilter <- paste0("dat[,'", columnName('conductivity', location, metric, columnNames), "'] >= ", conductivityMin)
	        if(!is.na(conductivityMax)) condmaxfilter <- paste0("dat[,'", columnName('conductivity', location, metric, columnNames), "'] <= ", conductivityMax)
	        if(!is.na(turbidityMin)) turbminfilter <- paste0("dat[,'", columnName('turbidity', location, metric, columnNames), "'] >= ", turbidityMin)
	        if(!is.na(turbidityMax)) turbmaxfilter <- paste0("dat[,'", columnName('turbidity', location, metric, columnNames), "'] <= ", turbidityMax)
	        
	        if(!is.na(ecoliMin)) ecoliminfilter <- paste0("dat[,'", columnName('ecoli', location, metric, columnNames), "'] >= ", ecoliMin)
	        if(!is.na(ecoliMax)) ecolimaxfilter <- paste0("dat[,'", columnName('ecoli', location, metric, columnNames), "'] <= ", ecoliMax)
	        if(!is.na(enterococciMin)) enterocminfilter <- paste0("dat[,'", columnName('enterococc', location, metric, columnNames), "'] >= ", enterococciMin)
	        if(!is.na(enterococciMax)) enterocmaxfilter <- paste0("dat[,'", columnName('enterococc', location, metric, columnNames), "'] <= ", enterococciMax)
	        if(!is.na(totcoliformMin)) totcoliformminfilter <- paste0("dat[,'", columnName('coliform', location, metric, columnNames), "'] >= ", totcoliformMin)
	        if(!is.na(totcoliformMax)) totcoliformmaxfilter <- paste0("dat[,'", columnName('coliform', location, metric, columnNames), "'] <= ", totcoliformMax)
	        
            # Remove missing values
            if(length(sitefilter)) sitefilter <- paste0(sitefilter, " & !is.na(dat[,'", columnName('site', location, metric, columnNames), "'])")
            if(length(tempminfilter)) tempminfilter <- paste0(tempminfilter, " & !is.na(dat[,'", columnName('temperature', location, metric, columnNames), "'])")
            if(length(tempmaxfilter)) tempmaxfilter <- paste0(tempmaxfilter, " & !is.na(dat[,'", columnName('temperature', location, metric, columnNames), "'])")
            if(length(condminfilter)) condminfilter <- paste0(condminfilter, " & !is.na(dat[,'", columnName('conductivity', location, metric, columnNames), "'])")
            if(length(condmaxfilter)) condmaxfilter <- paste0(condmaxfilter, " & !is.na(dat[,'", columnName('conductivity', location, metric, columnNames), "'])")
            if(length(turbminfilter)) turbminfilter <- paste0(turbminfilter, " & !is.na(dat[,'", columnName('turbidity', location, metric, columnNames), "'])")
            if(length(turbmaxfilter)) turbmaxfilter <- paste0(turbmaxfilter, " & !is.na(dat[,'", columnName('turbidity', location, metric, columnNames), "'])")
            if(length(datefilter)) datefilter <- paste0(datefilter, " & !is.na(dat[,'", columnName('date', location, metric, columnNames), "'])")
            
	        if(length(ecoliminfilter)) ecoliminfilter <- paste0(" & !is.na(dat[,'", columnName('ecoli', location, metric, columnNames), "'])")
	        if(length(ecolimaxfilter)) ecolimaxfilter <- paste0(" & !is.na(dat[,'", columnName('ecoli', location, metric, columnNames), "'])")
	        if(length(enterocminfilter)) enterocminfilter <- paste0(" & !is.na(dat[,'", columnName('enteroc', location, metric, columnNames), "'])")
	        if(length(enterocmaxfilter)) enterocmaxfilter <- paste0(" & !is.na(dat[,'", columnName('enteroc', location, metric, columnNames), "'])")
	        if(length(totcoliformminfilter)) totcoliformminfilter <- paste0(" & !is.na(dat[,'", columnName('coliform', location, metric, columnNames), "'])")
	        if(length(totcoliformmaxfilter)) totcoliformmaxfilter <- paste0(" & !is.na(dat[,'", columnName('coliform', location, metric, columnNames), "'])")

	        allfilters <- c(sitefilter, yearfilter, datefilter, tempminfilter, tempmaxfilter, condminfilter, condmaxfilter, turbminfilter, turbmaxfilter, ecoliminfilter, ecolimaxfilter, enterocminfilter, enterocmaxfilter, totcoliformminfilter, totcoliformmaxfilter, ecoliminfilter, ecolimaxfilter, enterocminfilter, enterocmaxfilter, totcoliformminfilter, totcoliformmaxfilter)
	        
	        allfilters <- allfilters[unlist(lapply(allfilters, function(x) length(x) > 0))]
            if(length(allfilters)) {
                allfilters <- paste(allfilters, collapse=" & ")	 
                stmt <- paste0("dat[", allfilters, ",]")
                eval(parse(text=stmt))
            }       
	    })
	    
	    # Filter the roe jan chemistry table
	    filteredRoeJanChem <- reactive({
	        dat <- getRoeJanChemData()
	        metric <- navigation$currentmetric
	        location <- input$location	        
	        
	        daterange <- input$filterdaterange
	        site <- input$site
	        year <- input$year
	        tempMin <- input$tempMin
	        tempMax <- input$tempMax
	        conductivityMin <- input$conductivityMin
	        conductivityMax <- input$conductivityMax
	        turbidityMin <- input$turbidityMin
	        turbidityMax <- input$turbidityMax
	        enterococciMin <- input$enterococciMin
	        enterococciMax <- input$enterococciMax
	        
	        sitefilter <- yearfilter <- tempminfilter <- tempmaxfilter <- condminfilter <- condmaxfilter <- turbminfilter <- turbmaxfilter <- enterocminfilter <- enterocmaxfilter <- character()
	        
	        if(length(site)) sitefilter <- paste0("dat[,'", columnName('site', location, metric, columnNames), "'] %in% c('", paste0(site, collapse="', '"), "')")
	        
	        if(length(year)) yearfilter <- paste0("dat[,'", columnName('year', location, metric, columnNames), "'] %in% c(", paste(year, collapse=", "), ")")
	        datefilter <- paste0("dat[,'", columnName('date', location, metric, columnNames), "'] >= as.Date('", daterange[1], "') & ", "dat[,'", columnName('date', location, metric, columnNames), "'] <= as.Date('", daterange[2], "')")
	        
	        if(!is.na(tempMin)) tempminfilter <- paste0("dat[,'", columnName('temperature', location, metric, columnNames), "'] >= ", tempMin)
	        if(!is.na(tempMax)) tempmaxfilter <- paste0("dat[,'", columnName('temperature', location, metric, columnNames), "'] <= ", tempMax)
	        if(!is.na(conductivityMin)) condminfilter <- paste0("dat[,'", columnName('conductivity', location, metric, columnNames), "'] >= ", conductivityMin)
	        if(!is.na(conductivityMax)) condmaxfilter <- paste0("dat[,'", columnName('conductivity', location, metric, columnNames), "'] <= ", conductivityMax)
	        if(!is.na(turbidityMin)) turbminfilter <- paste0("dat[,'", columnName('turbidity', location, metric, columnNames), "'] >= ", turbidityMin)
	        if(!is.na(turbidityMax)) turbmaxfilter <- paste0("dat[,'", columnName('turbidity', location, metric, columnNames), "'] <= ", turbidityMax)
            
	        if(!is.na(enterococciMin)) enterocminfilter <- paste0("dat[,'", columnName('enteroc', location, metric, columnNames), "'] >= ", enterococciMin)
	        if(!is.na(enterococciMax)) enterocmaxfilter <- paste0("dat[,'", columnName('enteroc', location, metric, columnNames), "'] <= ", enterococciMax)
	        
            # Remove missing values
            if(length(sitefilter)) sitefilter <- paste0(sitefilter, " & !is.na(dat[,'", columnName('site', location, metric, columnNames), "'])")
            if(length(tempminfilter)) tempminfilter <- paste0(tempminfilter, " & !is.na(dat[,'", columnName('temperature', location, metric, columnNames), "'])")
            if(length(tempmaxfilter)) tempmaxfilter <- paste0(tempmaxfilter, " & !is.na(dat[,'", columnName('temperature', location, metric, columnNames), "'])")
            if(length(condminfilter)) condminfilter <- paste0(condminfilter, " & !is.na(dat[,'", columnName('conductivity', location, metric, columnNames), "'])")
            if(length(condmaxfilter)) condmaxfilter <- paste0(condmaxfilter, " & !is.na(dat[,'", columnName('conductivity', location, metric, columnNames), "'])")
            if(length(turbminfilter)) turbminfilter <- paste0(turbminfilter, " & !is.na(dat[,'", columnName('turbidity', location, metric, columnNames), "'])")
            if(length(turbmaxfilter)) turbmaxfilter <- paste0(turbmaxfilter, " & !is.na(dat[,'", columnName('turbidity', location, metric, columnNames), "'])")
            if(length(datefilter)) datefilter <- paste0(datefilter, " & !is.na(dat[,'", columnName('date', location, metric, columnNames), "'])")
            
	        if(length(enterocminfilter)) enterocminfilter <- paste0(" & !is.na(dat[,'", columnName('enteroc', location, metric, columnNames), "'])")
	        if(length(enterocmaxfilter)) enterocmaxfilter <- paste0(" & !is.na(dat[,'", columnName('enteroc', location, metric, columnNames), "'])")

	        allfilters <- c(sitefilter, yearfilter, datefilter, tempminfilter, tempmaxfilter, condminfilter, condmaxfilter, turbminfilter, turbmaxfilter, enterocminfilter, enterocmaxfilter)
	        
	        allfilters <- allfilters[unlist(lapply(allfilters, function(x) length(x) > 0))]
            if(length(allfilters)) {
                allfilters <- paste(allfilters, collapse=" & ")	 
                stmt <- paste0("dat[", allfilters, ",]")
                eval(parse(text=stmt))
            }       
	    })
        
        
        # Filter the saw kill nutrient table
	    filteredSawKillNutrients <- reactive({
	        dat <- getSawKillNutrientData()
	        metric <- navigation$currentmetric
	        location <- input$location
	        
	        daterange <- input$filterdaterange
	        site <- input$site
	        year <- input$year
	        ammoniumMin <- input$ammoniumMin
	        ammoniumMax <- input$ammoniumMax
	        nitrateMin <- input$nitrateMin
	        nitrateMax <- input$nitrateMax
	        phosphateMin <- input$phosphateMin
	        phosphateMax <- input$phosphateMax
	        totalNMin <- input$totalNMin
	        totalNMax <- input$totalNMax
	        totalPMin <- input$totalPMin
	        totalPMax <- input$totalPMax
	        	        
	        sitefilter <- yearfilter <- ammoniumMinfilter <- ammoniumMaxfilter <- nitrateMinfilter <- nitrateMaxfilter <- phosphateMinfilter <- phosphateMaxfilter <- totalNMinfilter <- totalNMaxfilter <- totalPMinfilter <- totalPMaxfilter <- character()
	        
	        if(length(site)) sitefilter <- paste0("dat[,'", columnName('site', location, metric, columnNames), "'] %in% c(", paste(site, collapse=", "), ")")
	        
	        if(length(year)) yearfilter <- paste0("dat[,'", columnName('year', location, metric, columnNames), "'] %in% c(", paste(year, collapse=", "), ")")
	        
	        datefilter <- paste0("dat[,'", columnName('date', location, metric, columnNames), "'] >= as.Date('", daterange[1], "') & ", "dat[,'", columnName('date', location, metric, columnNames), "'] <= as.Date('", daterange[2], "')")
	        
	        if(!is.na(ammoniumMin)) ammoniumMinfilter <- paste0("dat[,'", columnName('ammonium', location, metric, columnNames), "'] >= ", ammoniumMin)
	        if(!is.na(ammoniumMax)) ammoniumMaxfilter <- paste0("dat[,'", columnName('ammonium', location, metric, columnNames), "'] <= ", ammoniumMax)
	        if(!is.na(nitrateMin)) nitrateMinfilter <- paste0("dat[,'", columnName('nitrate', location, metric, columnNames), "'] >= ", nitrateMin)
	        if(!is.na(nitrateMax)) nitrateMaxfilter <- paste0("dat[,'", columnName('nitrate', location, metric, columnNames), "'] <= ", nitrateMax)
	        if(!is.na(phosphateMin)) phosphateMinfilter <- paste0("dat[,'", columnName('phosphate', location, metric, columnNames), "'] >= ", phosphateMin)
	        if(!is.na(phosphateMax)) phosphateMaxfilter <- paste0("dat[,'", columnName('phosphate', location, metric, columnNames), "'] <= ", phosphateMax)
	        if(!is.na(totalNMin)) totalNMinfilter <- paste0("dat[,'", columnName('nitrogen', location, metric, columnNames), "'] >= ", totalNMin)
	        if(!is.na(totalNMax)) totalNMaxfilter <- paste0("dat[,'", columnName('nitrogen', location, metric, columnNames), "'] <= ", totalNMax)
	        if(!is.na(totalPMin)) totalPMinfilter <- paste0("dat[,'", columnName('phosphorus', location, metric, columnNames), "'] >= ", totalPMin)
	        if(!is.na(totalPMax)) totalPMaxfilter <- paste0("dat[,'", columnName('phosphorus', location, metric, columnNames), "'] <= ", totalPMax)
	        
	        
	        # Remove missing values
            if(length(sitefilter)) sitefilter <- paste0(sitefilter, " & !is.na(dat[,'", columnName('site', location, metric, columnNames), "'])")
            
            if(length(ammoniumMinfilter)) ammoniumMinfilter <- paste0(ammoniumMinfilter, " & !is.na(dat[,'", columnName('ammonium', location, metric, columnNames), "'])")
            if(length(ammoniumMaxfilter)) ammoniumMaxfilter <- paste0(ammoniumMaxfilter, " & !is.na(dat[,'", columnName('ammonium', location, metric, columnNames), "'])")
            if(length(nitrateMin)) nitrateMin <- paste0(nitrateMin, " & !is.na(dat[,'", columnName('nitrate', location, metric, columnNames), "'])")
            if(length(nitrateMax)) nitrateMax <- paste0(nitrateMax, " & !is.na(dat[,'", columnName('nitrate', location, metric, columnNames), "'])")
            if(length(phosphateMin)) phosphateMin <- paste0(phosphateMin, " & !is.na(dat[,'", columnName('phosphate', location, metric, columnNames), "'])")
            if(length(phosphateMax)) phosphateMax <- paste0(phosphateMax, " & !is.na(dat[,'", columnName('phosphate', location, metric, columnNames), "'])")
            if(length(totalNMin)) totalNMin <- paste0(totalNMin, " & !is.na(dat[,'", columnName('nitrogen', location, metric, columnNames), "'])")
            if(length(totalNMax)) totalNMax <- paste0(totalNMax, " & !is.na(dat[,'", columnName('nitrogen', location, metric, columnNames), "'])")
            if(length(totalPMin)) totalPMin <- paste0(totalPMin, " & !is.na(dat[,'", columnName('phosphorus', location, metric, columnNames), "'])")
            if(length(totalPMax)) totalPMax <- paste0(totalPMax, " & !is.na(dat[,'", columnName('phosphorus', location, metric, columnNames), "'])")
	        
	        allfilters <- c(sitefilter, yearfilter, datefilter, ammoniumMinfilter, ammoniumMaxfilter, nitrateMinfilter, nitrateMaxfilter, phosphateMinfilter, phosphateMaxfilter, totalNMinfilter, totalNMaxfilter, totalPMinfilter, totalPMaxfilter)
	        
	        allfilters <- allfilters[unlist(lapply(allfilters, function(x) length(x) > 0))]
            if(length(allfilters)) {
                allfilters <- paste(allfilters, collapse=" & ")	 
                stmt <- paste0("dat[", allfilters, ",]")
                eval(parse(text=stmt))
            }   
        })
        
        
        # Filter the saw kill land use table
	    filteredSawKillLandUse <- reactive({
	        dat <- getSawKillLandUseData()
	        dat
#	        metric <- navigation$currentmetric
#	        location <- input$location
#	        
#	        daterange <- input$filterdaterange
#	        site <- input$site
#	        year <- input$year
#	        ammoniumMin <- input$ammoniumMin
#	        ammoniumMax <- input$ammoniumMax
#	        nitrateMin <- input$nitrateMin
#	        nitrateMax <- input$nitrateMax
#	        phosphateMin <- input$phosphateMin
#	        phosphateMax <- input$phosphateMax
#	        totalNMin <- input$totalNMin
#	        totalNMax <- input$totalNMax
#	        totalPMin <- input$totalPMin
#	        totalPMax <- input$totalPMax
#	        	        
#	        sitefilter <- yearfilter <- ammoniumMinfilter <- ammoniumMaxfilter <- nitrateMinfilter <- nitrateMaxfilter <- phosphateMinfilter <- phosphateMaxfilter <- totalNMinfilter <- totalNMaxfilter <- totalPMinfilter <- totalPMaxfilter <- character()
#	        
#	        if(length(site)) sitefilter <- paste0("dat[,'", columnName('site', location, metric, columnNames), "'] %in% c(", paste(site, collapse=", "), ")")
#	        
#	        if(length(year)) yearfilter <- paste0("dat[,'", columnName('year', location, metric, columnNames), "'] %in% c(", paste(year, collapse=", "), ")")
#	        
#	        datefilter <- paste0("dat[,'", columnName('date', location, metric, columnNames), "'] >= as.Date('", daterange[1], "') & ", "dat[,'", columnName('date', location, metric, columnNames), "'] <= as.Date('", daterange[2], "')")
#	        
#	        if(!is.na(ammoniumMin)) ammoniumMinfilter <- paste0("dat[,'", columnName('ammonium', location, metric, columnNames), "'] >= ", ammoniumMin)
#	        if(!is.na(ammoniumMax)) ammoniumMaxfilter <- paste0("dat[,'", columnName('ammonium', location, metric, columnNames), "'] <= ", ammoniumMax)
#	        if(!is.na(nitrateMin)) nitrateMinfilter <- paste0("dat[,'", columnName('nitrate', location, metric, columnNames), "'] >= ", nitrateMin)
#	        if(!is.na(nitrateMax)) nitrateMaxfilter <- paste0("dat[,'", columnName('nitrate', location, metric, columnNames), "'] <= ", nitrateMax)
#	        if(!is.na(phosphateMin)) phosphateMinfilter <- paste0("dat[,'", columnName('phosphate', location, metric, columnNames), "'] >= ", phosphateMin)
#	        if(!is.na(phosphateMax)) phosphateMaxfilter <- paste0("dat[,'", columnName('phosphate', location, metric, columnNames), "'] <= ", phosphateMax)
#	        if(!is.na(totalNMin)) totalNMinfilter <- paste0("dat[,'", columnName('nitrogen', location, metric, columnNames), "'] >= ", totalNMin)
#	        if(!is.na(totalNMax)) totalNMaxfilter <- paste0("dat[,'", columnName('nitrogen', location, metric, columnNames), "'] <= ", totalNMax)
#	        if(!is.na(totalPMin)) totalPMinfilter <- paste0("dat[,'", columnName('phosphorus', location, metric, columnNames), "'] >= ", totalPMin)
#	        if(!is.na(totalPMax)) totalPMaxfilter <- paste0("dat[,'", columnName('phosphorus', location, metric, columnNames), "'] <= ", totalPMax)
#	        
#	        
#	        # Remove missing values
#            if(length(sitefilter)) sitefilter <- paste0(sitefilter, " & !is.na(dat[,'", columnName('site', location, metric, columnNames), "'])")
#            
#            if(length(ammoniumMinfilter)) ammoniumMinfilter <- paste0(ammoniumMinfilter, " & !is.na(dat[,'", columnName('ammonium', location, metric, columnNames), "'])")
#            if(length(ammoniumMaxfilter)) ammoniumMaxfilter <- paste0(ammoniumMaxfilter, " & !is.na(dat[,'", columnName('ammonium', location, metric, columnNames), "'])")
#            if(length(nitrateMin)) nitrateMin <- paste0(nitrateMin, " & !is.na(dat[,'", columnName('nitrate', location, metric, columnNames), "'])")
#            if(length(nitrateMax)) nitrateMax <- paste0(nitrateMax, " & !is.na(dat[,'", columnName('nitrate', location, metric, columnNames), "'])")
#            if(length(phosphateMin)) phosphateMin <- paste0(phosphateMin, " & !is.na(dat[,'", columnName('phosphate', location, metric, columnNames), "'])")
#            if(length(phosphateMax)) phosphateMax <- paste0(phosphateMax, " & !is.na(dat[,'", columnName('phosphate', location, metric, columnNames), "'])")
#            if(length(totalNMin)) totalNMin <- paste0(totalNMin, " & !is.na(dat[,'", columnName('nitrogen', location, metric, columnNames), "'])")
#            if(length(totalNMax)) totalNMax <- paste0(totalNMax, " & !is.na(dat[,'", columnName('nitrogen', location, metric, columnNames), "'])")
#            if(length(totalPMin)) totalPMin <- paste0(totalPMin, " & !is.na(dat[,'", columnName('phosphorus', location, metric, columnNames), "'])")
#            if(length(totalPMax)) totalPMax <- paste0(totalPMax, " & !is.na(dat[,'", columnName('phosphorus', location, metric, columnNames), "'])")
#	        
#	        allfilters <- c(sitefilter, yearfilter, datefilter, ammoniumMinfilter, ammoniumMaxfilter, nitrateMinfilter, nitrateMaxfilter, phosphateMinfilter, phosphateMaxfilter, totalNMinfilter, totalNMaxfilter, totalPMinfilter, totalPMaxfilter)
#	        
#	        allfilters <- allfilters[unlist(lapply(allfilters, function(x) length(x) > 0))]
#            if(length(allfilters)) {
#                allfilters <- paste(allfilters, collapse=" & ")	 
#                stmt <- paste0("dat[", allfilters, ",]")
#                eval(parse(text=stmt))
#            }   
        })
	    
	    # Identify the table that is on screen
	    subsetTable <- reactive({
	        location <- input$location
	        metric <- navigation$currentmetric
	        if(metric == "chemistry") {
                if(location == "SawKill") dat <- filteredSawKillChem()
                else if(location == "RoeJan") dat <- filteredRoeJanChem()
            } else if(metric == "nutrients") {
                if(location == "SawKill") dat <- filteredSawKillNutrients()
                else if(location == "RoeJan") dat <- filteredRoeJanNutrients()
            } else if(metric == "landuse") {
                if(location == "SawKill") dat <- filteredSawKillLandUse()
                else if(location == "RoeJan") dat <- filteredRoeLandUse()
            }
            list(data=dat, location=location, metric=metric)
	    })
	    
	    # Identify the dataset that is on screen
	    fullTable <- reactive({
	        location <- input$location
	        metric <- navigation$currentmetric
	        if(location == "SawKill") datfun <- switch(metric, chemistry=getSawKillChemData, nutrients=getSawKillNutrientData)
            else if(location == "RoeJan") datfun <- switch(metric, chemistry=getRoeJanChemData)
	        
	        dat <- do.call(datfun, list())
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
	    
	    # yaxis options are location specific
	    observe({
	        metric <- navigation$currentmetric
            location <- input$location
            if(length(location)) {
	            if(metric == "chemistry") {
	                if(location == "SawKill") choices <- c(Temperature="temperature", Conductivity="conductivity", Turbidity="turbidity", `E. coli Concentration`="ecoli", `Enterococci Concentration`="enterococc", `Total Coliform Concentration`="coliform")
                    else if(location == "RoeJan") choices <- c(Temperature="temperature", Conductivity="conductivity", Turbidity="turbidity", `Enterococci Concentration`="enterococc")
                } else if(metric == "nutrients") {
	                if(location == "SawKill") choices <- c(Ammonium="ammonium", Nitrate="nitrate", Phosphate="phosphate", `Total Nitrogen`="nitrogen", `Total Phosphorus`="phosphorus")
                    else if(location == "RoeJan") choices <- c()
                } else if(metric == "landuse") {
                    choices <- c()
                }
                # Update the yaxis selector
                updateSelectInput(session, "yaxis", choices=choices, selected=if(input$yaxis != "") input$yaxis else choices[1])
                # Update the statistics selector
                updateCheckboxGroupInput(session, "tabularstatsattribute", choices=choices, selected=input$tabularstatsattribute)
            }         
	    })
	    
	    # Make a chart
	    output$chartcontent <- renderPlot({
	        source("server/makePlot.R", local=TRUE)
	    })
	    
	    # Download chart
	    output$downloadchart <- downloadHandler(
	        filename=function() {
	            dat <- subsetTable()
	            paste0(dat$location, "_",  input$yaxis, ".png")
	        },
	        content=function(file) {
	            png(file, width=1000, height=800)
	            source("server/makePlot.R", local=TRUE)
	            dev.off()
	        },
	        contentType="image/png"
	    )
	    
	    
	    # Make stats table
	    statstable <- reactive({
#	        metric <- navigation$currentmetric
#            location <- input$location
#	        dat <- fullTable()
	        dat <- subsetTable()
	        metric <- dat$metric
            location <- dat$location
	        dat <- dat$data
#	        daterange <- input$statisticsdaterange
	        daterange <- input$filterdaterange
	        sites <- na.omit(unique(dat[,columnName('site', location, metric, columnNames)]))
	        sites <- sites[sites != "NA"]
	        dat <- dat[dat[,columnName('date', location, metric, columnNames)] >= as.Date(daterange[1]) & dat[,columnName('date', location, metric, columnNames)] <= as.Date(daterange[2]) & !is.na(dat[,columnName('date', location, metric, columnNames)]), ]
	        allrows <- nrow(dat)
	        filters <- input$tabularstatsattribute 
	        if(length(filters)) {
	            columnvals <- unlist(sapply(filters, function(x) columnName(x, location, metric, columnNames)), use.names=FALSE)
                stats <- lapply(columnvals, function(x) {
                    if(length(sites) > 1) {
                        allx <- dat[, x]
                        allsites <- makeStatsDF(allx)
                        allsites <- t(allsites)
                        colnames(allsites) <- x
                        allsites <- data.frame(Site="All", Metric=rownames(allsites), allsites)
                    }
                    sitestats <- lapply(sites, function(y) {
                        sitex <- dat[dat[,columnName('site', location, metric, columnNames)] == y, x]
                        site.tmp <- makeStatsDF(sitex)
                        site.tmp <- t(site.tmp)
                        colnames(site.tmp) <- x
                        data.frame(Site=y, Metric=rownames(site.tmp), site.tmp)
                    })
                    sitestats <- do.call(rbind.data.frame, sitestats)
                    if(length(sites) > 1) rbind(allsites, sitestats)
                    else sitestats
                })
                sitemetric <- stats[[1]][,c("Site", "Metric")]
                stats <- lapply(stats, function(x) x[,3, drop=FALSE])
                stats <- do.call(data.frame, stats)
                stats <- data.frame(sitemetric, stats)
                rownames(stats) <- 1:nrow(stats)
                list(data=stats, location=location, metric=metric)
            }
	    })
	    
	    # Show stats table
	    output$statsmodalcontent <- DT::renderDataTable({
	        dat <- statstable()
	        dat$data
	    }, rownames=FALSE)
	    
	    output$downloadstats <- downloadHandler(
	        filename=function() {
	            dat <- statstable()
	            paste0(dat$location, "_",  dat$metric, "_stats.csv")
	        },
	        content=function(file) {
	            dat <- statstable()
	            dat <- dat$data
	            write.csv(dat, file, row.names=FALSE)
	        },
	        contentType="text/csv"
	    )	    
	}
)
