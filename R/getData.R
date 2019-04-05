#getData("SawKill", limit=10)

getData <- function(
    location = c("sawkill", "roejan"),
    dataset = c("chemistry", "nutrients", "landuse"),
    fields=NULL, # list
    where=NULL,  # list
    offset="", # Number
    limit="",  # Number
    apikey
) {

    # dataset key
    # 2731 = saw kill chemistry
    # 2732 = saw kill nutrients
    # 2733 = roe jan chemistry
    location <- tolower(location)
    location <- match.arg(location)
    dataset <- tolower(dataset)
    dataset <- match.arg(dataset)
    if(location == "sawkill") datasetid <- switch(dataset, chemistry=2731, nutrients=2732)
    else if(location == "roejan") datasetid <- switch(dataset, chemistry=2733)
    
    if(!length(fields)) fields <- "[]"
    else fields <- jsonlite::toJSON(fields, auto_unbox=TRUE)
    if(!length(where)) where <- "[]"
    else where <- jsonlite::toJSON(where, auto_unbox=TRUE)

    body=list(datasetid=datasetid, fields=fields, where=where, offset=offset, limit=limit)
    h <- curl::new_handle()
    curl::handle_setopt(h, postfields = jsonlite::toJSON(body, auto_unbox=TRUE));
    curl::handle_setheaders(h,
        'X-API-KEY'= apikey,
        'Accept'='application/json',
        'Content-Type'='application/json'
    )
    req <- curl::curl_fetch_memory("https://www.uvm.edu/femc/bard/getDatasetData", handle = h)
    if(req$status_code != 200) stop(req$status_code)
    dat <- jsonlite::fromJSON(rawToChar(req$content))
    
    if(!is.data.frame(dat)) {
        # Make DF
        dat <- lapply(dat, function(x) {
            z <- lapply(x, function(y) { if(length(y)) y else NA})
            data.frame(z)
        })
        dat <- do.call(rbind.data.frame, dat)
    } 
    # Coerce numbers-as-string to numeric
    for(i in 1:ncol(dat)) dat[i] <- tryCatch(as.numeric(dat[,i]), warning=function(w) dat[,i])
    dat
}

#getMetadata("SawKill")

getMetadata <- function(
    dataset = c("sawkill", "roejan", "nutrients"),  
    apikey
) {

    # dataset key
    # 2731 = saw kill
    # 2732 = nutrients
    # 2733 = roe jan
    dataset <- tolower(dataset)
    dataset <- match.arg(dataset)
    datasetid <- switch(dataset, sawkill=2731, nutrients=2732, roejan=2733)
    
    body=list(datasetid=datasetid)
    h <- curl::new_handle()
    curl::handle_setopt(h, postfields = jsonlite::toJSON(body, auto_unbox=TRUE));
    curl::handle_setheaders(h,
        'X-API-KEY'= apikey,
        'Accept'='application/json',
        'Content-Type'='application/json'
    )
    req <- curl::curl_fetch_memory("https://www.uvm.edu/femc/bard/getDatasetFieldsMetadata", handle = h)
    if(req$status_code != 200) stop(req$status_code)
    dat <- jsonlite::fromJSON(rawToChar(req$content))
    
    if(!is.data.frame(dat)) {
        # Make DF
        dat <- lapply(dat, function(x) {
            z <- lapply(x, function(y) { if(length(y)) y else NA})
            data.frame(z)
        })
        dat <- do.call(rbind.data.frame, dat)
    } 
    dat
}

## These are both the same
#skloc <- getLocationData("SawKill")
#nutloc <- getLocationData("nutrients")
## This fails with 404 error
#rjloc <- getLocationData("roejan")
getLocationData <- function(
    dataset = c("sawkill", "roejan", "nutrients"),  
    apikey
) {

    # dataset key
    # 2731 = saw kill
    # 2732 = nutrients
    # 2733 = roe jan
    dataset <- tolower(dataset)
    dataset <- match.arg(dataset)
    datasetid <- switch(dataset, sawkill=2731, nutrients=2732, roejan=2733)
    
    body=list(datasetid=datasetid)
    h <- curl::new_handle()
    curl::handle_setopt(h, postfields = jsonlite::toJSON(body, auto_unbox=TRUE));
    curl::handle_setheaders(h,
        'X-API-KEY'= apikey,
        'Accept'='application/json',
        'Content-Type'='application/json'
    )
    req <- curl::curl_fetch_memory("https://www.uvm.edu/femc/bard/getLocationData", handle = h)
    if(req$status_code != 200) stop(req$status_code)
    dat <- jsonlite::fromJSON(rawToChar(req$content))
    
    if(!is.data.frame(dat)) {
        # Make DF
        dat <- lapply(dat, function(x) {
            z <- lapply(x, function(y) { if(length(y)) y else NA})
            data.frame(z)
        })
        dat <- do.call(rbind.data.frame, dat)
    } 
    dat
}

#skloc.s <- locsToSHP(skloc)
#nutloc.s <- locsToSHP(nutloc)
locsToSHP <- function(locs) {
    coords <- locs[,c("Longtiude", "Latitude")]
    for(i in 1:ncol(coords)) coords[i] <- as.numeric(as.character(coords[,i]))
    # sp::CRS("+init=epsg:4326")
    sp::SpatialPointsDataFrame(coords=coords, data=locs, proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
}


# match column name with dataset
#columnName('temp', 'SawKill', 'chemistry', columnNames)
columnName <- function(selection, location, metric, lookup) {
    colval <- paste0(location, "_", metric)
    lookup[lookup$selection==selection, colval]
}







