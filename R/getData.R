#getData("SawKill", "chemistry", limit=10, apikey=apikey)

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

#getMetadata("SawKill", "chemistry", apikey)

getMetadata <- function(
    location = c("sawkill", "roejan"),
    dataset = c("chemistry", "nutrients"),  
    apikey
) {

    # dataset key
    # 2731 = saw kill chem
    # 2732 = saw kill nutrients
    # 2733 = roe jan chem
    location <- tolower(location)
    location <- match.arg(location)
    dataset <- tolower(dataset)
    dataset <- match.arg(dataset)
    if(location == "sawkill") datasetid <- switch(dataset, chemistry=2731, nutrients=2732)
    else if(location == "roejan") datasetid <- switch(dataset, chemistry=2733)
    
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
    lookup[lookup$selection==tolower(selection), tolower(colval)]
}

# Download all column names, make a lookuptable
makeColNameTab <- function(apikey) {
    dat <- sapply(c("sawkill", "roejan"), function(x) {
        sapply(c("nutrients", "chemistry"), function(y) {
            z <- tryCatch(getMetadata(x, y, apikey), error=function(e) NULL)
            z
        }, simplify=FALSE)
    }, simplify=FALSE)

    d2 <- do.call(c, dat) 

    datanames <- sapply(d2, rownames)
    filters <- c("date", "site", "year", "temperature", "turbidity", "ecoli", "conductivity", "coliform", "enterococc", "ammonium", "magnesium", "phosphorus", "calcium", "nitrate", "sodium", "chloride", "phosphate", "nitrogen")

    nm <- sapply(filters, function(x) {
        as.data.frame(sapply(datanames, function(y) {
            z <- y[grep(x, y, ignore.case=TRUE)]
            if(length(z)) z
            else NA
        }, simplify=FALSE))
    }, simplify=FALSE)

    nmtab <- do.call(rbind.data.frame, nm)
    nmtab <- data.frame(selection=rownames(nmtab), nmtab)
    colnames(nmtab) <- gsub("\\.", "_", colnames(nmtab))
    nmtab
}

# Some wrappers to remove error -999 codes and compute statistics
rmErrMean <- function(x) {
    x <- x[x > -999]
    round(mean(x, na.rm=TRUE), 3)
}
rmErrMin <- function(x) {
    x <- x[x > -999]
    min(x, na.rm=TRUE)
}
rmErrMax <- function(x) {
    x <- x[x > -999]
    max(x, na.rm=TRUE)
}
rmErrRange <- function(x) {
    x <- x[x > -999]
    range(x, na.rm=TRUE)
}
rmErrLength <- function(x) {
    x <- x[x > -999]
    x <- na.omit(x)
    length(x)
}
rmErrMode <- function(x) {
    x <- x[x > -999]
    x <- na.omit(x)
    x <- hist(x, 10, plot=FALSE)
    bin <- which(x$counts == max(x$counts))
    highct <- x$counts[bin]
    brk <- x$breaks[c(bin, bin+1)]
    if(length(bin)>1) {
        highct <- sum(highct)
        brk <- range(brk)
    }
    list(range=brk, n=highct)    
}
makeStatsDF <- function(x) {
    fmean <- rmErrMean(x) 
    fmin <- rmErrMin(x) 
    fmax <- rmErrMax(x) 
    fN <- rmErrLength(x) 
    fmode <- rmErrMode(x) 
    errs <- sum(!is.na(x) & x == -999)
    missing = sum(is.na(x))
    data.frame(Samples=fN, Mean=fmean, Min=fmin, Max=fmax, Mode=paste0(fmode$n, " samples between ", fmode$range[1], " and ", fmode$range[2]), Removed.999=errs, Removed.NA=missing) #Attribute=x, range=paste0(fmin, " - ", fmax), 
}


