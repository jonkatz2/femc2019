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
                    div(class="filtergroup", 
                        p(class="font-bold", icon("caret-right"), "Date", onclick="togglefilter(event);"),
                        span(class="filter hidden", 
                            dateRangeInput("filterdaterange", NULL, start="1901-01-01", width="100%")
                        )
                    ),
                    div(class="filtergroup",
                        p(class="font-bold", icon("caret-right"), "Year", onclick="togglefilter(event);"),
                        span(class="filter hidden", 
                            selectizeInput("year", NULL, choices=list(), multiple=TRUE, options = list(plugins=list('remove_button')), width="100%")
                        )
                    ),
                    div(class="filtergroup",
                        p(class="font-bold", icon("caret-right"), "Location", onclick="togglefilter(event);"),
                        span(class="filter hidden", 
                            radioButtons("location", NULL, choices=c(`Saw Kill`="SawKill", `Roe Jan`="RoeJan"), width="100%")
#                            selectizeInput("location", NULL, choices=c(`Saw Kill`="SawKill", `Roe Jan`="Roe Jan"), multiple=TRUE, options = list(plugins=list('remove_button')), width="100%")
                        )
                    ),
                    div(class="filtergroup",
                        p(class="font-bold", icon("caret-right"), "Site", onclick="togglefilter(event);"),
                        span(class="filter hidden", 
                            selectizeInput("site", NULL, choices=list(), multiple=TRUE, options = list(plugins=list('remove_button')), width="100%")
                        )
                    ),
                    div(class="filtergroup overflow-hidden",
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
                    div(class="filtergroup overflow-hidden",
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
                    div(class="filtergroup overflow-hidden",
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
                    div(class="filtergroup overflow-hidden sawkillonly",
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
                    div(class="filtergroup overflow-hidden",
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
                    div(class="filtergroup overflow-hidden sawkillonly",
                        p(class="font-bold", icon("caret-right"), "Total coliform conc.", onclick="togglefilter(event);"),
                        span(class="filter hidden", 
                            span(class="numericleft",
                                numericInput("coliformMin", "Min", value=NA, min=-100, max=100, step=0.1)
                            ),
                            span(class="numericright",
                                numericInput("coliformMax", "Max", value=NA, min=-100, max=100, step=0.1)
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
    div(id="chartcontainer", # HAS STYLING
        uiOutput("chartcontent")
    )
)
