options(stringsAsFactors=FALSE)
# shinyapps.io requires us to install phantomjs for each session
if(!length(suppressMessages(webshot:::find_phantom()))) webshot::install_phantomjs()

# all external functions
source("R/getData.R")
# the apikey is in a separate file that can be listed in .gitignore
source("apikey.R")

# metadata is retrieved for each session, used to look up column names
metadata <- downloadMetadata(apikey)
columnNames <- makeColNameTab(metadata)
## error codes are hardcoded in the rmErr functions, could be dynamically sourced
#errorCodes <- makeErrTab(metadata)
#print(errorCodes)


shinyServer(
	function(input,output,session) {
	    # manage UI states	    
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
	        clickedbasin <- input$gotobasin
	        # the rendered UI depends on whether navigation is via tooltip or primary button
            if(length(clickedbasin)) {
                # User clicked a "View Data" button in a leaflet tooltip
                isolate(navigation$currentmetric <- "landuse")
                clicked <- c("redbutton", "redbutton", "redbutton clicked")
                output$secondarybuttons <- renderUI(div(id="metricbuttons",
                    column(4, class="leftalign", actionButton("chemistry_open", "Water Quality Data", class=clicked[1])),
                    column(4, class="text-center", actionButton("nutrients_open", "Nutrients Data", class=clicked[2])),
                    column(4, class="rightalign", actionButton("landcover_open", "Land Cover Data", class=clicked[3])),
                    tags$script('
                        $("#chemistry_open").click(function(event) { clickedmetric(event); });
                        $("#nutrients_open").click(function(event) { clickedmetric(event); });
                        $("#landcover_open").click(function(event) { clickedmetric(event); });'
                    )
                ))
            } else {
	            currentview <- navigation$currentview
	            if(currentview %in% c("tableview", "chartview")) {
                    # User clicked the "Table View" or "Chart View" button
	                metric <- navigation$currentmetric
	                clicked <- c("redbutton", "redbutton", "redbutton")
	                clicked[c("chemistry", "nutrients", "landuse") == metric] <- "redbutton clicked"
	                if(input$location == "RoeJan") {
	                    clicked[2:3] <- c(paste(clicked[2], "sawkillonly hidden"), paste(clicked[3], "sawkillonly hidden"))
                    } else if(currentview == "chartview") {
                        clicked[3] <- paste(clicked[3], "hidden")
                    }
	                output$secondarybuttons <- renderUI(div(id="metricbuttons",
                        column(4, class="leftalign", actionButton("chemistry_open", "Water Quality Data", class=clicked[1])),
                        column(4, class="text-center", actionButton("nutrients_open", "Nutrients Data", class=clicked[2])),
                        column(4, class="rightalign", actionButton("landcover_open", "Land Cover Data", class=clicked[3])),
                        tags$script('
                            $("#chemistry_open").click(function(event) { clickedmetric(event); });
                            $("#nutrients_open").click(function(event) { clickedmetric(event); });
                            $("#landcover_open").click(function(event) { clickedmetric(event); });'
                        )
                    ))
	            } else {
	                # User clicked the "Study Sites" button
	                output$secondarybuttons <- renderUI(div())
#                    map <- navigation$currentmap
#                    clicked <- c("redbutton", "redbutton")
#                    clicked[c("sites", "basins") == map] <- "redbutton clicked"
#                    if(input$location == "RoeJan") clicked[2] <- paste0(clicked[2], "sawkillonly hidden")
#                    output$secondarybuttons <- renderUI(div(id="mapbuttons",
#                        column(6, class="text-center", actionButton("sites_open", "Plots", class=clicked[1])),
#                        column(6, class="text-center", actionButton("basins_open", "Sub Basins", class=clicked[2])),
#                        tags$script('
#                            $("#sites_open").click(function(event) { clickedmap(event); });
#                            $("#basins_open").click(function(event) { clickedmap(event); });'
#                        )
#                    ))
	            }
	        }
	    })
	    
#	    # Change the buttons to table/chart options
#	    observeEvent(input$sites_open, navigation$currentmap <- "sites", ignoreInit=TRUE)
#	    observeEvent(input$basins_open, navigation$currentmap <- "basins", ignoreInit=TRUE)
	    
	    # get the saw kill locations
	    getSawkillLocations <- reactive({
	        dat <- getLocationData("SawKill", apikey=apikey)
            dat <- toPoints(dat, "SawKill")
            content <- paste0("<b>", dat@data[,'Site_Name'], "</b><br/>", "Site: ", dat@data[,'Site_Numbe'], "&nbsp; &nbsp; &nbsp; (", dat@data[,'Latitude'], ", ", dat@data[,'Longitude'], ")<br/>", dat@data[,'Site_Descr'], '<div style=text-align:center;><button class="btn btn-default action-button redbutton narrowbutton" onclick="gotosite(\'', dat@data[,'Site_Numbe'], '\')" type="button">View Data</button></div>')
            labels <- as.character(dat@data[,"Site_Numbe"])
            coords <- coordinates(dat)
            colnames(coords) <- c("Longitude", "Latitude")
	        list(points=dat, content=content, labels=labels, coords=coords, zoom=12)
	    })
	    
	    # get the roe jan locations
	    getRoeJanLocations <- reactive({
	        dat <- getLocationData("RoeJan", apikey=apikey)
            dat <- toPoints(dat, "RoeJan")
	        popup <- dat@data[,'PopupInfo']
	        popup[is.na(popup)] <- ""
	        
	        content <- paste0("<b>", dat@data[,'Name'], "</b><br/>", "Site: ", dat@data[,'SiteName'], "&nbsp; &nbsp; &nbsp; (", dat@data[,'Latitude'], ", ", dat@data[,'Longitude'], ")<br/>", popup, '<div style=text-align:center;><button class="btn btn-default action-button redbutton narrowbutton" onclick="gotosite(\'', dat@data[,'SiteName'], '\')" type="button">View Data</button></div>')
            labels <- as.character(dat@data[,"SiteName"])
            coords <- coordinates(dat)
            colnames(coords) <- c("Longitude", "Latitude")
	        list(points=dat, content=content, labels=labels, coords=coords, zoom=11)
	    })
	    
	    # get threshold data 
	    getThresholdData <- reactive({
	        getThresholds(apikey)
	    })
	        
	    # Identify the location map to show on screen
	    currentLocationSites <- reactive({
	        location <- input$location
            if(location == "SawKill") dat <- getSawkillLocations()
            else if(location == "RoeJan") dat <- getRoeJanLocations()
            dat
	    })
	    
	    # Download site location data
	    output$downloadsitedata <- downloadHandler(
	        file=function() {
	            paste0(input$location, "_sitedata.csv")
	        },
	        content=function(file) {
	            dat_s <- currentLocationSites()
	            dat_s <- dat_s$points
	            write.csv(dat_s@data, file, row.names=FALSE)
	        },
	        contentType="text/csv"
	    )
	    
	    # read the saw kill basins
	    getSawkillBasins <- reactive({
	        dat <- getLocationData("SawKill", "basins", apikey=apikey)
	        dat <- toPolygons(dat)
	        dat
	    })
	    	    
	    # Make the map of sawkill points
	    sitesmap <- reactive({
	        sites <- currentLocationSites()
	        sitescontent <- sites$content
	        siteslabels <- sites$labels
	        sitescoords <- sites$coords
	        siteszoom <- sites$zoom
	        sites <- sites$points
	        crds <- unname(rowMeans(bbox(sites)))
            direction <- rep(c("top", "left", "bottom", "right"), ceiling(length(labels)/4))
	        m <- leaflet(sites)
            m <- addTiles(m)
            for(i in 1:nrow(sites))
            m <- addCircleMarkers(m, 
                lng = as.numeric(sitescoords[i, "Longitude"]), 
                lat = as.numeric(sitescoords[i,"Latitude"]), 
                popup = sitescontent[i], 
                label = siteslabels[i], 
                labelOptions = labelOptions(noHide = TRUE, className="leaflet-label-small", direction=direction[i]), 
                options=pathOptions(className="leaflet-marker-hover")
            )
            m <- setView(m, lat = crds[2], lng = crds[1], zoom = siteszoom)
            m
	    })
	    
	    # Make the map of sawkill subbasins
	    basinsmap <- reactive({
	        basins <- getSawkillBasins()
	        crds <- unname(rowMeans(bbox(basins)))
            colorpal1 <- brewer.pal(7, "RdYlBu")
            colorpal2 <- brewer.pal(7, "PRGn")
            colorpal3 <- rev(brewer.pal(6, "BrBG"))
            colorpal <- c(colorpal1, colorpal2, colorpal3)
	        landuse <- getSawKillLandUseData()
            lupct <- colnames(landuse)
            lupct <- lupct[grepl("pct$", lupct, ignore.case=TRUE)]
	        ludat <- unlist(sapply(basins@data[,'Letter_ID'], function(x) {
	            letterid <- paste0("<tr><th>Basin Letter ID</th><th>", x, "</th></tr>")
	            watershed <- paste0("<tr><td>Watershed</td><td>", basins@data[basins@data[,'Letter_ID'] == x, 'WSHED3'], "</td></tr>")
	            acres <- paste0("<tr><td>Acres</td><td>", landuse[landuse['sub_basin'] == x, 'Acres'], "</td></tr>")
	            otherstat <- unlist(lapply(lupct, function(y) {
	                paste0("<tr><td>", strsplit(y, "_")[[1]][1], "</td><td>", paste0(landuse[landuse['sub_basin'] == x, y], '%'), "</td></tr>")
	            }))
	            paste0("<table class='watershedstats'>", letterid, watershed, acres, paste0(otherstat, collapse=""), "</table>", collapse="")
	        }))
	        basinscontent <- paste0(ludat, '<div style=text-align:center;><button class="btn btn-default action-button redbutton narrowbutton" onclick="gotobasin(\'', basins@data[,'Letter_ID'], '\')" type="button">View Data</button></div>')
            basinslabels <- basins@data[,"Letter_ID"]

	        m <- leaflet(basins)
            m <- addTiles(m)
            for(i in 1:nrow(basins))
            m <- addPolygons(m,
                data = basins[i, ],
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
                popup = basinscontent[i],
                label = basinslabels[i]
            )
            m <- addLabelOnlyMarkers(m, 
                lng=coordinates(basins)[,1],
                lat=coordinates(basins)[,2],
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

        # one map with both basins and study sites
	    onemap <- reactive({
	        # Sites
	        sites <- currentLocationSites()
	        sitescontent <- sites$content
	        siteslabels <- sites$labels
	        sitescoords <- sites$coords
	        siteszoom <- sites$zoom
	        sites <- sites$points
	        crds <- unname(rowMeans(bbox(sites)))
            direction <- rep(c("top", "left", "bottom", "right"), ceiling(length(labels)/4))
	        # Basins
	        basins <- getSawkillBasins()
	        crds <- unname(rowMeans(bbox(basins)))
            colorpal1 <- brewer.pal(7, "RdYlBu")
            colorpal2 <- brewer.pal(7, "PRGn")
            colorpal3 <- rev(brewer.pal(6, "BrBG"))
            colorpal <- c(colorpal1, colorpal2, colorpal3)
	        landuse <- getSawKillLandUseData()
            lupct <- colnames(landuse)
            lupct <- lupct[grepl("pct$", lupct, ignore.case=TRUE)]
	        ludat <- unlist(sapply(basins@data[,'Letter_ID'], function(x) {
	            letterid <- paste0("<tr><th>Basin Letter ID</th><th>", x, "</th></tr>")
	            watershed <- paste0("<tr><td>Watershed</td><td>", basins@data[basins@data[,'Letter_ID'] == x, 'WSHED3'], "</td></tr>")
	            acres <- paste0("<tr><td>Acres</td><td>", landuse[landuse['sub_basin'] == x, 'Acres'], "</td></tr>")
	            otherstat <- unlist(lapply(lupct, function(y) {
	                paste0("<tr><td>", strsplit(y, "_")[[1]][1], "</td><td>", paste0(landuse[landuse['sub_basin'] == x, y], '%'), "</td></tr>")
	            }))
	            paste0("<table class='watershedstats'>", letterid, watershed, acres, paste0(otherstat, collapse=""), "</table>", collapse="")
	        }))
	        basinscontent <- paste0(ludat, '<div style=text-align:center;><button class="btn btn-default action-button redbutton narrowbutton" onclick="gotobasin(\'', basins@data[,'Letter_ID'], '\')" type="button">View Data</button></div>')
            basinslabels <- basins@data[,"Letter_ID"]
            # Make map
	        m <- leaflet(basins)
            m <- addTiles(m)
            for(i in 1:nrow(basins))
            m <- addPolygons(m,
                data = basins[i, ],
                color = "#444444", 
                weight = 1, 
                smoothFactor = 0.5,
                opacity = 1, 
                fillOpacity = 0.5,
                fillColor = colorpal[i],
                highlightOptions = highlightOptions(
                    color = "white", 
                    weight = 2
                ),
                popup = basinscontent[i],
                label = basinslabels[i],
                group = "Basins"
            )
            m <- addLabelOnlyMarkers(m, 
                lng=coordinates(basins)[,1],
                lat=coordinates(basins)[,2],
                labelOptions = labelOptions(
                    noHide = TRUE, 
                    className="leaflet-label-hidden",
                    direction="top"
                ),
                label = basinslabels,
                group = "Basins"
            )
            for(i in 1:nrow(sites))
            m <- addCircleMarkers(m, 
                lng = as.numeric(sitescoords[i, "Longitude"]), 
                lat = as.numeric(sitescoords[i,"Latitude"]), 
                popup = sitescontent[i], 
                label = siteslabels[i], 
                labelOptions = labelOptions(noHide = TRUE, className="leaflet-label-small", direction=direction[i]), 
                options = pathOptions(className="leaflet-marker-hover"),
                group = "Study Sites"
            )
            m <- setView(m, lat = crds[2], lng = crds[1], zoom = 12)
            m <- addLayersControl(m, 
                overlayGroups = c("Basins", "Study Sites"),
                options = layersControlOptions(collapsed = FALSE)
            )
            m
	    })

	    # display the map
	    output$mapcontent <- renderLeaflet({
#	        currentmap <- navigation$currentmap
#	        if(currentmap == "sites") map <- sitesmap()
#	        else map <- basinsmap()
            if(input$location == "SawKill") map <- onemap()
            else map <- sitesmap()
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
                if(location == "SawKill") dat <- getSawKillLandUseDataBySubbasin()
                else if(location == "RoeJan") dat <- getRoeLandUseData()
            }
            sites <- unique(dat[,columnName('site', location, "chemistry", columnNames)])
            updateSelectizeInput(session, 'site', choices=setNames(nm=sites), selected=site)
        }, ignoreInit=TRUE)
        
        
        # Go to the basin selected from the map
        observeEvent(input$gotobasin, {
            basin <- input$gotobasin
            metric <- navigation$currentmetric
            dat <- getSawKillLandUseDataBySubbasin()
            basins <- unique(dat[,"sub_basin"])
            updateSelectizeInput(session, 'subbasin', choices=setNames(nm=basins), selected=basin)
        }, ignoreInit=TRUE)
        
        # When moving between table view (landuse metric) and chart view, switch off landuse
        observe({
            currentview <- navigation$currentview
            currentmetric <- navigation$currentmetric
            if(currentview != "tableview" && currentmetric == "landuse") navigation$currentmetric <- "chemistry"
        })
        
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
	            paste0(input$location, "_sites.png")
	        },
	        content=function(file) {
#	            currentmap <- navigation$currentmap
#	            if(currentmap == "sites") map <- sitesmap()
#	            else map <- basinsmap()
                if(input$location == "SawKill") map <- onemap()
                else map <- sitesmap()
	            map
                mapview::mapshot(map, file = file)
	        },
	        contentType="image/png"
	    )
	    
	    # Download site map as zipped shapefile
	    output$downloadmapshapefile <- downloadHandler(
	        file=function() {
	            paste0(input$location, "_studysites.zip")
	        },
	        content=function(file) {
	            location <- input$location
	            if(location == "SawKill") {
                    sitesmap <- getSawkillLocations()
                    basinsmap <- getSawkillBasins()
                    rgdal::writeOGR(sitesmap$points, paste0(location, "_studysites"), paste0(location, "_studysites"), driver="ESRI Shapefile", overwrite_layer=TRUE, delete_dsn=TRUE)
                    rgdal::writeOGR(basinsmap, paste0(location, "_studysites"), paste0(location, "_basins"), driver="ESRI Shapefile", overwrite_layer=TRUE, delete_dsn=FALSE)
                } else {
                    map <- getRoeJanLocations()
                    rgdal::writeOGR(map$points, paste0(location, "_studysites"), paste0(location, "_studysites"), driver="ESRI Shapefile", overwrite_layer=TRUE, delete_dsn=TRUE)
                }
#	            currentmap <- navigation$currentmap
#                if(location == "SawKill") {
#                    if(currentmap == "sites") map <- getSawkillLocations()
#                    else map <- getSawkillBasins()
#                } else {
#                    if(currentmap == "sites") map <- getRoeJanLocations()
#                    else map <- getRoejanBasins()
#                }
#	            rgdal::writeOGR(map, paste0(location, currentmap), paste0(location, currentmap), driver="ESRI Shapefile", overwrite_layer=TRUE, delete_dsn=TRUE)
	            zip(file, paste0(location, "_studysites"))
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
                    if(location == "SawKill") dat <- getSawKillLandUseDataBySubbasin()
                    else if(location == "RoeJan") dat <- getRoeLandUseData()
                }
                if(length(dat)) {
                    sites <- unique(dat[,columnName('site', location, metric, columnNames)])
                    updateSelectInput(session, "site", choices=setNames(nm=sites), selected=isolate(input$site))
                    year <- unique(dat[,columnName('year', location, metric, columnNames)])
                    updateSelectInput(session, "year", choices=setNames(nm=year), selected=isolate(input$year))
                }
            }
	    }, priority=1000) # execute before the from-map site
	    
	    
	    # Update the sub-basin selector
	    observe({
	        dat <- getSawKillLandUseDataBySubbasin()
            if(is.data.frame(dat)) {
                subbasins <- unique(dat$sub_basin)
                updateSelectizeInput(session, "subbasin", choices=setNames(nm=subbasins), selected=isolate(input$subbasin))
            }
        }, priority=1000) 
	    	    
	    # get the saw kill table from FEMC
	    getSawKillChemData <- reactive({
	        dat <- getData("sawkill", "chemistry", apikey=apikey)
	        metric <- navigation$currentmetric
	        datecol <- columnName('date', input$location, metric, columnNames)
	        dat[,datecol] <- as.Date(dat[,datecol])
            dat
	    })
	    
	    # get the roe jan table from FEMC
	    getRoeJanChemData <- reactive({
	        dat <- getData("roejan", "chemistry", apikey=apikey)
	        metric <- navigation$currentmetric
	        datecol <- columnName('date', input$location, metric, columnNames)
	        dat[,datecol] <- as.Date(dat[,datecol])
	        dat
	    })
	    
	    # get the saw kill nutrient table from FEMC
	    getSawKillNutrientData <- reactive({
	        dat <- getData("sawkill", "nutrients", apikey=apikey)
	        metric <- navigation$currentmetric
	        datecol <- columnName('date', input$location, metric, columnNames)
	        dat[,datecol] <- as.Date(dat[,datecol])
            dat
	    })
	    
	    # This is never called?
	    # get the roe jan nutrient table from FEMC
	    getRoeJanNutrientData <- reactive({
            NULL
	    })

	    # get the saw kill land use table 
	    ## LOCAL FILE ##
	    getSawKillLandUseData <- reactive({
	        dat <- read.csv("data/LandUseData_Cleaned.csv", stringsAsFactors=FALSE)
            dat
	    })
	    
	    # get the saw kill subbasin land use table from FEMC
	    ## LOCAL FILE ##
	    getSawKillLandUseDataBySubbasin <- reactive({
	        dat <- read.csv("data/SawKillSubbasin_MasterMonitoring_Data_cleaned.csv", stringsAsFactors=FALSE)
	        dat$date <- as.Date(dat$date, format="%m/%d/%Y")
            dat
	    })
	    
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
	        # Set empty filter placeholders
	        sitefilter <- yearfilter <- tempminfilter <- tempmaxfilter <- condminfilter <- condmaxfilter <- turbminfilter <- turbmaxfilter <- ecoliminfilter <- ecolimaxfilter <- enterocminfilter <- enterocmaxfilter <- totcoliformminfilter <- totcoliformmaxfilter <- character()
	        # update placeholders with user selections
	        if(length(site)) sitefilter <- paste0("dat[,'", columnName('site', location, metric, columnNames), "'] %in% c('", paste(site, collapse="', '"), "')")
	        
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
	        
            # Remove missing (NA) values
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
            # Combine all filter statements & evaluate
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
	        # Gather user selections from UI
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
	        # Set empty filter placeholders
	        sitefilter <- yearfilter <- tempminfilter <- tempmaxfilter <- condminfilter <- condmaxfilter <- turbminfilter <- turbmaxfilter <- enterocminfilter <- enterocmaxfilter <- character()
	        
	        # update placeholders with user selections
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
	        # Gather user selections from UI
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
	        # Set empty filter placeholders
	        sitefilter <- yearfilter <- ammoniumMinfilter <- ammoniumMaxfilter <- nitrateMinfilter <- nitrateMaxfilter <- phosphateMinfilter <- phosphateMaxfilter <- totalNMinfilter <- totalNMaxfilter <- totalPMinfilter <- totalPMaxfilter <- character()
	        # update placeholders with user selections
	        if(length(site)) sitefilter <- paste0("dat[,'", columnName('site', location, metric, columnNames), "'] %in% c('", paste(site, collapse="', '"), "')")
	        
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
	        dat <- getSawKillLandUseDataBySubbasin()
	        # Gather user selections from UI
	        daterange <- input$filterdaterange
	        site <- input$site
	        year <- input$year
	        subbasin <- input$subbasin

	        # Set empty filter placeholders
	        sitefilter <- yearfilter <- subbasinfilter <- character()
	        # update placeholders with user selections
	        if(length(site)) sitefilter <- paste0("dat[,'SiteID'] %in% c('", paste(site, collapse="', '"), "')")
	        
	        if(length(year)) yearfilter <- paste0("dat[,'year'] %in% c(", paste(year, collapse=", "), ")")
	        
	        datefilter <- paste0("dat[,'date'] >= as.Date('", daterange[1], "') & ", "dat[,'date'] <= as.Date('", daterange[2], "')")

	        if(length(subbasin)) subbasinfilter <- paste0("dat[,'sub_basin'] %in% c('", paste0(subbasin, collapse="', '"), "')")
	        
	        
	        # Remove missing values
            if(length(sitefilter)) sitefilter <- paste0(sitefilter, " & !is.na(dat[,'SiteID'])")
            
            if(length(subbasinfilter)) subbasinfilter <- paste0(subbasinfilter, " & !is.na(dat[,'sub_basin'])")
	        
	        allfilters <- c(sitefilter, yearfilter, datefilter, subbasinfilter)

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
	        if(location == "SawKill") datfun <- switch(metric, chemistry=getSawKillChemData, nutrients=getSawKillNutrientData, landuse=getSawKillLandUseData)
            else if(location == "RoeJan") datfun <- switch(metric, chemistry=getRoeJanChemData)
	        
	        dat <- do.call(datfun, list())
            list(data=dat, location=location, metric=metric)
	    })
	    
	    # Show the table
	    output$tablecontent <- DT::renderDataTable({
	        dat <- subsetTable()
	        dat$data
	    }, rownames=FALSE)
	    
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
                currentyaxis <- input$yaxis
                if(currentyaxis != "") {
                    if(currentyaxis %in% choices) {
                        selected <- currentyaxis
                    } else selected <- choices[1]
                } else selected <- choices[1]
                # Update the yaxis selector
                updateSelectInput(session, "yaxis", choices=choices, selected=selected)
                # Update the statistics selector
                updateCheckboxGroupInput(session, "tabularstatsattribute", choices=choices, selected=input$tabularstatsattribute)
            }         
	    })
	    
	    # Prep data for plotting
	    plotData <- reactive({
	        dat <- subsetTable()
            dat <- dat$data
            location <- tolower(input$location)
            metric <- navigation$currentmetric
            if(length(dat)) {
                yvar <- input$yaxis
                yax <- columnName(yvar, location, metric, columnNames)
                if(is.na(yax)) yax <- ""
                # X axis is hardcoded
                xax <- columnName("date", location, metric, columnNames) 
                if(is.na(xax)) xax <- ""
                dat <- dat[order(dat[,xax]),]
                spl <- columnName(input$split, location, metric, columnNames)
                type <- input$graphtype
                # UI label is "Remove Errors"
                remove999 <- input$remove999
                if(all(c(yax, xax) != "")) {
                    if(remove999) dat <- dat[notErr(dat[,yax]),]
                }
                
                threshdat <- getThresholdData()
                rw <- which(grepl(yvar, threshdat[,'fieldName'], ignore.case=TRUE))[1]
                if(!is.na(rw)) {
                    thresh <- threshdat[rw, 'threshold']
                    unit <- threshdat[rw, 'unit']
                } else unit <- thresh <- NA
                list(data=dat, yax=yax, xax=xax, thresh=thresh, unit=unit)
            }
	    })
	    
	    
	    # Plot the value by site
	    output$sitelines <- renderD3({
            dat <- plotData()
            yax <- dat$yax
            xax <- dat$xax
            thresh <- dat$thresh
            unit <- dat$unit
            dat <- dat$data
            location <- tolower(input$location)
            metric <- navigation$currentmetric
            if(yax != "") {
                dat <- dat[c(xax, columnName("site", location, metric, columnNames), yax)]
                names(dat) <- c("date", "site", "value")
                dat <- na.omit(dat)
                opts <- list(ylab=yax, threshold=thresh, unit=unit, loc=location, metric=yax)
                opts <- opts[!unlist(lapply(opts, is.na))]
                opts <- 
                r2d3(
                    data = dat, 
                    script = "www/r2d3/siteplot.js", 
                    options = opts
                )
            }
	    })
	    
	    # Plot the value by year
	    output$yearlines <- renderD3({
            dat <- plotData()
            yax <- dat$yax
            xax <- dat$xax
            thresh <- dat$thresh
            unit <- dat$unit
            dat <- dat$data
            location <- tolower(input$location)
            metric <- navigation$currentmetric
            dateinfo <- as.POSIXlt(dat[,columnName("date", location, metric, columnNames)])
            dat$day <- dateinfo$yday
            dat$year <- dateinfo$year + 1900
            if(yax != "") {
                dat <- dat[c("year", columnName("site", location, metric, columnNames), yax, "day")]
                names(dat) <- c("year", "site", "value", "day")
                dat <- na.omit(dat)
                opts <- list(ylab=yax, threshold=thresh, unit=unit, loc=location, metric=yax)
                opts <- opts[!unlist(lapply(opts, is.na))]
                r2d3(
                    data = dat, 
                    script = "www/r2d3/yearplot.js", 
                    options = opts
                )
            }
	    })
	    
	    # Show each d3 plot in its own container
	    output$variableD3 <- renderUI({
	        if(input$split == "year") {
	            d3Output("yearlines", height="500px")
	        } else {
	            d3Output("sitelines", height="500px")
	        }
	    })

        # Threshold data below the plot
        output$thresholddefinition <- renderText({
            yax <- input$yaxis
            def <- switch(yax,
                enterococc = "Enterococci 60 CFU- The EPA recommends that Enterococci levels within surface waters used for recreational purposes, such as swimming, should not exceed 60 colony forming units (cfu) per 100mL of water.",
                temperature = "Temperature 20 degrees Celsius- Temperature affects the oxygen content of the water, the rate of photosynthesis by aquatic plants, the metabolic rates of aquatic organisms, and the sensitivity of organisms. We use the recommended temperature for brook trout, 20 degrees Celsius, as our indicator for water quality.",
                conductivity = "Uncompensated Conductivity 500 uS- The EPA recommendation for freshwater streams is to not exceed 500 uS to maintain a healthy ecosystem for freshwater organisms.", 
                ecoli = "E. coli 190 CFU- The EPA recommends that E. coli levels within surface waters used for recreational purposes, such as swimming, should not exceed 190 colony forming units (cfu) per 100mL of water.",
                turbidity = "Turbidity 10 NTU- The degree to which turbidity affects aquatic systems and organisms depends on both the amount of sediment in the water and how long the turbidity lasts, 10 NTU is when fish start to show signs of stress."
            )
        })       
        
#       # This was the native R (PNG) plot and downloader
#	    # Make a chart
#	    output$chartcontent <- renderPlot({
#	        source("server/makePlot.R", local=TRUE)
#	    })
	    
#	    # Download chart
#	    output$downloadchart <- downloadHandler(
#	        filename=function() {
#	            dat <- subsetTable()
#	            paste0(dat$location, "_",  input$yaxis, ".png")
#	        },
#	        content=function(file) {
#	            png(file, width=1000, height=800)
#	            source("server/makePlot.R", local=TRUE)
#	            dev.off()
#	        },
#	        contentType="image/png"
#	    )
	    
	    
	    # Make stats table
	    statstable <- reactive({
	        dat <- subsetTable()
	        metric <- dat$metric
            location <- dat$location
	        dat <- dat$data
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
