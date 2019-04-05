options(stringsAsFactors=FALSE)
#roejanwaterchem <- read.csv('data/RoeJan_WaterChem.csv')
#roejanwaterchem["Date_Sampled"] <- as.Date(roejanwaterchem[,"Date_Sampled"], format="%m/%d/%Y")
#sawkillwaterchem <- read.csv('data/Sawkill_WaterChem.csv')
#sawkillwaterchem["Date"] <- as.Date(sawkillwaterchem[,"Date"], format="%m/%d/%Y")
#nutrients <- read.csv('data/Nutrients.csv')

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
	    observeEvent(input$sites_open, navigation$currentmap <- "sites")
	    observeEvent(input$basins_open, navigation$currentmap <- "basins")
	    
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
	    
	    # Make the map of sawkill subbasins
	    basinsmap <- reactive({
	        dat <-readOGR("data/Subbasins_revised", "Subbasins_revised")
	        dat <- spTransform(dat, CRS("+init=epsg:4326"))
	        crds <- unname(rowMeans(bbox(dat)))
            colorpal1 <- brewer.pal(7, "RdYlBu")
            colorpal2 <- brewer.pal(7, "PRGn")
            colorpal3 <- rev(brewer.pal(6, "BrBG"))
            colorpal <- c(colorpal1, colorpal2, colorpal3)
	        content <- paste0("<b>Basin Letter ID: ", dat@data[,'Letter_ID'], "</b><br/>", "Watershed: ", dat@data[,'WSHED3'], "<br/>Acres: ", dat@data[,'Acres'], '<div style=text-align:center;><button class="btn btn-default action-button redbutton narrowbutton" onclick="gotobasin(\'', dat@data[,'Letter_ID'], '\')" type="button">View Data</button></div>')
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
            dat <- getSawKillChemData()
            sites <- unique(dat[,columnName('site', "SawKill", "chemistry", columnNames)])
            site <- input$gotosite
            loc <- site %in% sites
            if(loc) loc <- "SawKill"
            else {
                loc <- "RoeJan"
                dat <- getRoeJanChemData()
                sites <- unique(dat[,columnName('site', "RoeJan", "chemistry", columnNames)])
            }
            updateRadioButtons(session, 'location', selected=loc)
            updateSelectizeInput(session, 'site', choices=setNames(nm=sites), selected=site)
        }, ignoreInit=TRUE)
        
        
        
        
        # Update the filters with sawkill-only filters
        observeEvent(input$location, {
            session$sendCustomMessage("togglesawkillonlyfilters", message=list(location=input$location))
        }, ignoreInit=TRUE)
	    
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
	            "Sawkill_RoeJan_sitemap.zip"
	        },
	        content=function(file) {
#	            if(file.exists("location")) unlink("location", recursive=TRUE)
	            sawkill <- getSawkillLocations()
	            rgdal::writeOGR(sawkill, "location", "sawkill", driver="ESRI Shapefile", overwrite_layer=TRUE, delete_dsn=TRUE)
	            zip(file, "location")
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
                    if("SawKill" == location) dat <- getSawKillChemData()
                    else if("RoeJan" == location) dat <- getRoeJanChemData()
                } else if(metric == "nutrients") {
                    if("SawKill" == location) dat <- getSawKillNutrientData()
                    else if("RoeJan" == location) dat <- getRoeJanNutrientData()
                } else if(metric == "landuse") {
                    dat <- NULL
                }
                if(length(dat)) {
                    sites <- unique(dat[,columnName('site', location, metric, columnNames)])
                    updateSelectInput(session, "site", choices=setNames(nm=sites), selected=input$site)
                    year <- unique(dat[,columnName('year', location, metric, columnNames)])
                    updateSelectInput(session, "year", choices=setNames(nm=year), selected=input$year)
                }
            }
	    }, priority=1000) # execute before the from-map site updater
	    
	    # Nutrients only apply to saw kill
	    observe({
	        metric <- navigation$currentmetric
	        input$location
	        if(metric == "nutrients") updateRadioButtons(session, "location", selected="SawKill")
	    })
	    
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
	        
	        if(length(site)) sitefilter <- paste0("dat[,'", columnName('site', location, metric, columnNames), "'] %in% c(", paste(site, collapse=", "), ")")
	        
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
	    filteredNutrients <- reactive({
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
	        if("SawKill" == location) datfun <- switch(metric, chemistry=getSawKillChemData, nutrients=getSawKillNutrientData)
            else if("RoeJan" == location) datfun <- switch(metric, chemistry=getRoeJanChemData)
	        
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
	        if(metric == "chemistry") {
	            if("SawKill" == location) choices <- c(Temperature="temp", Conductivity="cond", Turbidity="turb", `E. coli Concentration`="ecoli", `Enterococci Concentration`="enteroc", `Total Coliform Concentration`="totcoliform")
                else if("RoeJan" == location) choices <- c(Temperature="temp", Conductivity="cond", Turbidity="turb", `Enterococci Concentration`="enteroc")
            } else if(metric == "nutrients") {
	            if("SawKill" == location) choices <- c(Ammonium="ammonium", Nitrate="nitrate", Phosphate="phosphate", `Total Nitrogen`="totalN", `Total Phosphorus`="TotalP")
                else if("RoeJan" == location) choices <- c()
            } else if(metric == "landuse") {
                choices <- c()
            }
            updateSelectInput(session, "yaxis", choices=choices, selected=if(input$yaxis != "") input$yaxis else choices[1])            
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
	}
)
