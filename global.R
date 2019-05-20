library(shiny)
library(DT)
library(curl)
library(jsonlite)
library(rgdal)
library(sp)
library(leaflet)
library(reshape2)
library(mapview) # requires libudunits2.so (from libudunits2-dev), then webshot::install_phantomjs()
library(RColorBrewer)
library(shinyBS)
library(r2d3)


options(
    stringsAsFactors=FALSE, 
    DT.options = list(
        lengthMenu = list(c(10,25,50,100,-1),c(10,25,50,100,'All'))
    )
) # options













