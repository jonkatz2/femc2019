function(req) {
    # If a query string is provided (/?loc=RoeJan), set location
    qry <- shiny::parseQueryString(req$QUERY_STRING)
    if(length(qry)) {
        loc.tmp <- qry$loc
        loc.tmp <- tolower(loc.tmp)
        loc.tmp <- match.arg(loc.tmp, choices=c("sawkill", "roejan"))
        loc.tmp <- switch(loc.tmp, sawkill="SawKill", roejan="RoeJan")
        location <- loc.tmp
    } else location <- c("SawKill", "RoeJan")

    fluidPage(
        singleton(tags$head(
            tags$link(rel="stylesheet", type="text/css", href="css/custom.css"),
            tags$script(src="js/custom.js"),
            tags$script(src="js/messageHandlers.js"),
            tags$style('
                .line {
                    stroke-width: 2;
                    fill: none;
                }'
            )
        )),
        # Statistics modal
        bsModal(
            id='statsmodal',
            title = div(
                span(style="float:left;", 
                    downloadButton("downloadstats")
                ),
                span(style="float:right;line-height:2em;",
                    p(class="statistics-help", `data-trigger`="hover click", `data-toggle`="tooltip", title="A measure of central tendency used by NYS DEC and the US EPA to assess water quality. The geometric mean involves taking the nth root of the product of the data values, where n is the number of data values.", `data-container`="body", `data-placement`="bottom", "Geometric Mean"),
                    p(class="statistics-help", `data-trigger`="hover click", `data-toggle`="tooltip", title="The arithmetic mean. The sum of data values divided by the number of data values.", `data-container`="body", `data-placement`="bottom", "Mean"),
                    p(class="statistics-help", `data-trigger`="hover click", `data-toggle`="tooltip", title="The lowest data point recorded.", `data-container`="body", `data-placement`="bottom", "Minimum"),
                    p(class="statistics-help", `data-trigger`="hover click", `data-toggle`="tooltip", title="The highest data point recorded.", `data-container`="body", `data-placement`="bottom", "Maximum"),
                    p(class="statistics-help", `data-trigger`="hover click", `data-toggle`="tooltip", title="Most often occurring data value.", `data-container`="body", `data-placement`="bottom", "Mode"),
                    p(class="statistics-help", `data-trigger`="hover click", `data-toggle`="tooltip", title="The number of data values used to compute statistics.", `data-container`="body", `data-placement`="bottom", "Samples"),
                    tags$br(),
                    p(class="statistics-help", `data-trigger`="hover click", `data-toggle`="tooltip", title="The number of data values before removing errors and NA values.", `data-container`="body", `data-placement`="bottom", "Data Rows"),
                    p(class="statistics-help", `data-trigger`="hover click", `data-toggle`="tooltip", title="Number of error codes removed. Error codes typically indicate measured values below detectable limit or above quantifiable limit.", `data-container`="body", `data-placement`="bottom", "Errors Removed"),
                    p(class="statistics-help", `data-trigger`="hover click", `data-toggle`="tooltip", title="Number of missing (NA) values removed. Missing values are typically due to adverse environmental or sampling conditions at the time of collection.", `data-container`="body", `data-placement`="bottom", "NA Removed")
                ),
                span(style="font-weight:bold;float:right;line-height:2em;padding: 5px 8px 0px 0px;font-size:0.75em;", "Definitions:")
            ),
            trigger="test1",
            DT::dataTableOutput("statsmodalcontent"), 
            size='large'
        ),
        # Location-specific title
        if(length(location) == 1) {
            h3(class="text-center", switch(location, SawKill="Saw Kill Data", RoeJan="Roe Jan Kill Data"))
        } else {
            NULL
        },
        div(class="navigation-control",
            div(id="primarybuttons",
                column(4, class="leftalign", actionButton("mapview_open", "Study Sites", class="redbutton clicked")),
                column(4, class="text-center", actionButton("tableview_open", "Table View", class="redbutton")),
                column(4, class="rightalign", actionButton("chartview_open", "Chart View", class="redbutton")),
                tags$script('
                    $("#mapview_open").click(function(event) { showmap(event); });
                    $("#tableview_open").click(function(event) { showtable(event); });
                    $("#chartview_open").click(function(event) { showchart(event); });'
                )
            ),
            uiOutput("secondarybuttons")
        ),
        tags$hr(id="divider"),
        # Study sites
        div(id="mapcontainer", # HAS STYLING
            leaflet::leafletOutput("mapcontent", height = 600),
            div(id="mapdownloadoptions",
                downloadButton("downloadsitedata", "Download Site Data", class="downloadbutton"),
                div(id="moremapoptions",
                    actionButton("showmapdownloadoptions", "Download Map", icon=icon("caret-right"), class="downloadbutton", onclick="togglecaret(event);"),
                    conditionalPanel("input.showmapdownloadoptions % 2",
                        downloadButton("downloadmapimage", "Download Image", class="downloadbutton stack"),
                        downloadButton("downloadmapshapefile", "Download Shapefile", class="downloadbutton stack")
                    )
                )
            )
        ),
        # Table view
        div(id="tablecontainer", class="hidden", 
            column(8, class="flushleft, overflow-x-scroll getwide",
                downloadButton("downloadfulltable", "Download Full Data", class="downloadbutton"),
                downloadButton("downloadtableselection", "Download Data Subset", class="downloadbutton"),
                DT::dataTableOutput("tablecontent")
            ),
            column(4, class="nopadright",
                div(class="rightalign",
                    actionButton("filterfield_open", "Study Sites", class="redbutton float-right clicked"),
                    tags$script('
                        $("#filterfield_open").click(function(event) { showfilters(event) });'
                    ),
                    div(id="filterfieldbox", class="float-right",
                        # Date
                        div(class="filtergroup", 
                            p(class="font-bold", icon("caret-right"), "Date", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                dateRangeInput("filterdaterange", NULL, start="1901-01-01", width="100%")
                            )
                        ),
                        # Year
                        div(class="filtergroup",
                            p(class="font-bold", icon("caret-right"), "Year", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                selectizeInput("year", NULL, choices=list(), multiple=TRUE, options = list(plugins=list('remove_button')), width="100%")
                            )
                        ),
                        # Location
                        div(class="filtergroup", style=if(length(location) == 1) {"display:none;"} else "",
                            p(class="font-bold", icon("caret-right"), "Location", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                radioButtons("location", NULL, choices=setNames(nm=location), width="100%")
                            )
                        ),
                        # Site
                        div(class="filtergroup",
                            p(class="font-bold", icon("caret-right"), "Site", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                selectizeInput("site", NULL, choices=list(), multiple=TRUE, options = list(plugins=list('remove_button')), width="100%")
                            )
                        ),
                        # Temperature
                        div(class="filtergroup overflow-hidden chemistry",
                            p(class="font-bold", icon("caret-right"), "Temperature", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                span(class="numericleft",
                                    numericInput("tempMin", "Min", value=NA, min=-100, max=100, step=0.1)
                                ),
                                span(class="numericright",
                                    numericInput("tempMax", "Max", value=NA, min=-100, max=100, step=0.1)
                                )
                            )
                        ),
                        # Conductivity
                        div(class="filtergroup overflow-hidden chemistry",
                            p(class="font-bold", icon("caret-right"), "Conductivity", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                span(class="numericleft",
                                    numericInput("conductivityMin", "Min", value=NA, min=-100, max=100, step=0.1)
                                ),
                                span(class="numericright",
                                    numericInput("conductivityMax", "Max", value=NA, min=-100, max=100, step=0.1)
                                )
                            )
                        ),
                        # Turbidity
                        div(class="filtergroup overflow-hidden chemistry",
                            p(class="font-bold", icon("caret-right"), "Turbidity", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                span(class="numericleft",
                                    numericInput("turbidityMin", "Min", value=NA, min=-100, max=100, step=0.1)
                                ),
                                span(class="numericright",
                                    numericInput("turbidityMax", "Max", value=NA, min=-100, max=100, step=0.1)
                                )
                            )
                        ),
                        # E. coli
                        div(class="filtergroup overflow-hidden chemistry sawkillonly",
                            p(class="font-bold", icon("caret-right"), "E. coli conc.", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                span(class="numericleft",
                                    numericInput("ecoliMin", "Min", value=NA, min=-100, max=100, step=0.1)
                                ),
                                span(class="numericright",
                                    numericInput("ecoliMax", "Max", value=NA, min=-100, max=100, step=0.1)
                                )
                            )
                        ),
                        # Enterococci conc
                        div(class="filtergroup overflow-hidden chemistry",
                            p(class="font-bold", icon("caret-right"), "Enterococci conc.", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                span(class="numericleft",
                                    numericInput("enterococciMin", "Min", value=NA, min=-100, max=100, step=0.1)
                                ),
                                span(class="numericright",
                                    numericInput("enterococciMax", "Max", value=NA, min=-100, max=100, step=0.1)
                                )
                            )
                        ),
                        # Total coliform
                        div(class="filtergroup overflow-hidden chemistry sawkillonly",
                            p(class="font-bold", icon("caret-right"), "Total coliform conc.", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                span(class="numericleft",
                                    numericInput("totcoliformMin", "Min", value=NA, min=-100, max=100, step=0.1)
                                ),
                                span(class="numericright",
                                    numericInput("totcoliformMax", "Max", value=NA, min=-100, max=100, step=0.1)
                                )
                            )
                        ),
                        # Ammonium
                        div(class="filtergroup overflow-hidden nutrients hidden",
                            p(class="font-bold", icon("caret-right"), "Ammonium", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                span(class="numericleft",
                                    numericInput("ammoniumMin", "Min", value=NA, min=-100, max=100, step=0.01)
                                ),
                                span(class="numericright",
                                    numericInput("ammoniumMax", "Max", value=NA, min=-100, max=100, step=0.01)
                                )
                            )
                        ),
                        # Nitrate
                        div(class="filtergroup overflow-hidden nutrients hidden",
                            p(class="font-bold", icon("caret-right"), "Nitrate", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                span(class="numericleft",
                                    numericInput("nitrateMin", "Min", value=NA, min=-100, max=100, step=0.01)
                                ),
                                span(class="numericright",
                                    numericInput("nitrateMax", "Max", value=NA, min=-100, max=100, step=0.01)
                                )
                            )
                        ),
                        # Phosphate
                        div(class="filtergroup overflow-hidden nutrients hidden",
                            p(class="font-bold", icon("caret-right"), "Phosphate", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                span(class="numericleft",
                                    numericInput("phosphateMin", "Min", value=NA, min=-100, max=100, step=0.001)
                                ),
                                span(class="numericright",
                                    numericInput("phosphateMax", "Max", value=NA, min=-100, max=100, step=0.001)
                                )
                            )
                        ),
                        # Total Nitrogen
                        div(class="filtergroup overflow-hidden nutrients hidden",
                            p(class="font-bold", icon("caret-right"), "Total Nitrogen", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                span(class="numericleft",
                                    numericInput("totalNMin", "Min", value=NA, min=-100, max=100, step=0.01)
                                ),
                                span(class="numericright",
                                    numericInput("totalNMax", "Max", value=NA, min=-100, max=100, step=0.01)
                                )
                            )
                        ),
                        # Total Phosphorus
                        div(class="filtergroup overflow-hidden nutrients hidden",
                            p(class="font-bold", icon("caret-right"), "Total Phosphorus", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                span(class="numericleft",
                                    numericInput("totalPMin", "Min", value=NA, min=-100, max=100, step=0.001)
                                ),
                                span(class="numericright",
                                    numericInput("totalPMax", "Max", value=NA, min=-100, max=100, step=0.001)
                                )
                            )
                        ),
                        # Sub-basin
                        div(class="filtergroup landcover hidden",
                            p(class="font-bold", icon("caret-right"), "Sub-basin", onclick="togglefilter(event);"),
                            span(class="filter hidden", 
                                selectizeInput("subbasin", NULL, choices=list(), multiple=TRUE, options = list(plugins=list('remove_button')), width="100%")
                            )
                        )
                    )
                ),
                div(class="rightalign",
                    actionButton("statistics_open", "Statistics", class="redbutton float-right"),
                    tags$script('
                        $("#statistics_open").click(function(event) { showstatistics(event) });'
                    ),
                    div(id="statisticsbox", class="float-right hidden",
                        checkboxGroupInput("tabularstatsattribute", "Select Attribute:", choices=c(Temperature="temperature", Conductivity="conductivity", Turbidity="turbidity")),
                        actionButton("applyfilter", "View Statistics", class="bluebutton", `data-toggle`="modal", `data-target`="#statsmodal")
                    )
                )
            )
        ),
        # Chart view
        div(id="chartcontainer", class="hidden", # HAS STYLING
            div(id="chartoptions",
                span(class="select-row", selectInput("yaxis", "Data for Y Axis", choices=list())),
                span(class="select-row", selectInput("split", "Split By", choices=c(Site="site", Year="year"))),
                span(class="select-row", style="padding-top:15px;", checkboxInput("remove999", "Remove error codes", value=TRUE))
            ),
            tags$script(
                type="text/javascript",
                src="http://cdn.jsdelivr.net/g/filesaver.js"
            ),
            uiOutput("variableD3"),
##            This was the original PNG chart
#            plotOutput("chartcontent", height="500px"),
            div(id="chartbuttons",
                actionButton("downloadd3chart", "Download Chart", class="downloadbutton clearboth")
            ),
            textOutput("thresholddefinition")
        )
    )
}
