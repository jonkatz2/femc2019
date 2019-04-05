fluidPage(
    singleton(tags$head(
        tags$link(rel="stylesheet", type="text/css", href="css/custom.css"),
        tags$script(src="js/custom.js"),
        tags$script(src="js/messageHandlers.js")
    )),
    h3(class="text-center", "Saw Kill Data"),
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
                    div(class="filtergroup",
                        p(class="font-bold", icon("caret-right"), "Location", onclick="togglefilter(event);"),
                        span(class="filter hidden", 
                            radioButtons("location", NULL, choices=c(`Saw Kill`="SawKill", `Roe Jan`="RoeJan"), width="100%")
#                            selectizeInput("location", NULL, choices=c(`Saw Kill`="SawKill", `Roe Jan`="Roe Jan"), multiple=TRUE, options = list(plugins=list('remove_button')), width="100%")
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
                    )
#                    actionButton("applyfilter", "Apply", class="bluebutton")
                )
            ),
            div(class="rightalign",
                actionButton("statistics_open", "Statistics", class="redbutton float-right"),
                tags$script('
                    $("#statistics_open").click(function(event) { showstatistics(event) });'
                ),
                div(id="statisticsbox", class="float-right hidden",
                    dateRangeInput("statisticsdaterange", "Select Date Range:", start="1901-01-01", width="100%"),
                    checkboxGroupInput("filterattribute", "Select Attribute:", choices=c(Temperature="temp", Conductivity="cond", Turbidity="turb")),
                    actionButton("applyfilter", "View Statistics", class="bluebutton")
                )
            )
        )
    ),
    # Chart view
    div(id="chartcontainer", class="hidden", # HAS STYLING
        div(id="chartoptions",
            span(class="select-row", selectInput("graphtype", "Graph Type", choices=c(`Line/points`="o", Line="l", Points="p"))),
            span(class="select-row", selectInput("xaxis", "Data for X Axis", choices=c(Date="date"))), 
            span(class="select-row", selectInput("yaxis", "Data for Y Axis", choices=list())),
            span(class="select-row", selectInput("split", "Split By", choices=c(Site="site", Year="year")))
        ),

        plotOutput("chartcontent", height="500px"),
        div(id="chartbuttons",
            actionButton("chartinformation", "Chart Information", class="downloadbutton"),
            downloadButton("downloadchart", "Download Chart", class="downloadbutton"),
            actionButton("chart_statistics", "Statistics", class="downloadbutton")
        )
    )
)
