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
        # Make DF of error codes
        errdat <- lapply(dat, function(x) {
            if(!length(x$list)) return(NULL)
            x <- x$list
            x[c("fldDescription", "fldItem")]
        })
        errdat <- errdat[as.logical(unlist(lapply(errdat, length)))]
        if(length(errdat)) {
            errdat <- lapply(names(errdat), function(x) {errdat[[x]]$column <- x; errdat[[x]]})
            errdat <- do.call(rbind.data.frame, errdat)
        }
        # Make DF of metadata
        dat <- lapply(dat, function(x) {
            x <- x[!grepl("list", names(x))]
            z <- lapply(x, function(y) { if(length(y)) y else NA})
            data.frame(z)
        })
        dat <- do.call(rbind.data.frame, dat)
    } 
    list(columnnames=dat, errorcodes=errdat)
}


# Action limit thresholds for sawkill chemistry
getThresholds <- function(
    apikey
) {
    h <- curl::new_handle()
    curl::handle_setheaders(h,
        'X-API-KEY'= apikey,
        'Accept'='application/json',
        'Content-Type'='application/json'
    )
    req <- curl::curl_fetch_memory("https://www.uvm.edu/femc/bard/getThresholds", handle = h)
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




#skloc <- getLocationData("SawKill", "sites", apikey)
getLocationData <- function(
    location = c("sawkill", "roejan"), 
    dataset = c("sites", "nutrients", "basins"),  
    apikey
) {

    # dataset key
    # 2731 = saw kill points
    # 2733 = roe jan points
    # 2738 = saw kill basins
    location <- tolower(location)
    location <- match.arg(location)
    dataset <- tolower(dataset)
    dataset <- match.arg(dataset)
    if(location == "sawkill") {
        datasetid <- switch(dataset, sites=2731, basins=2738)
    } else if(location == "roejan") {
        datasetid <- switch(dataset, sites=2733)
    }
    
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
    
    if(!is.data.frame(dat) && dataset != "basins") {
        # Make DF
        dat <- lapply(dat, function(x) {
            z <- lapply(x, function(y) { if(length(y)) y else NA})
            data.frame(z)
        })
        dat <- do.call(rbind.data.frame, dat)
    } 
    dat
}

# Convert download to SpatialPoints
#skloc.s <- locsToSHP(skloc)
#nutloc.s <- locsToSHP(nutloc)
toPoints <- function(locs, location) {
    coords <- locs[,c("Longitude", "Latitude")]
    for(i in 1:ncol(coords)) coords[i] <- as.numeric(as.character(coords[,i]))
    # sp::CRS("+init=epsg:4326")
    sp::SpatialPointsDataFrame(coords=coords, data=locs, proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
}

# Convert download to SpatialPolygons
toPolygons <- function(l) {
    Name <- names(l)
    dfl <- sapply(Name, function(x) {
        yn <- names(l[[x]])
        y <- l[[x]][!yn %in% c("locationGeom", "geoType", "coordsAr")]
        y[unlist(lapply(y, is.null))] <- NA
        as.data.frame(y)
    }, simplify=FALSE)
    df <- do.call(rbind.data.frame, dfl)
    rownames(df) <- 0:(nrow(df)-1)
    polygons <- sapply(Name, function(x) {l[[x]]$coordsAr}, simplify=FALSE)
    poly.l <- lapply(polygons, sp::Polygon, hole=FALSE)
    polys.l <- lapply(1:length(poly.l), function(x) sp::Polygons(list(poly.l[[x]]), ID=x-1) )
    spolys <- sp::SpatialPolygons(polys.l, proj4string=sp::CRS("+init=epsg:4326"))
    # sp::CRS("+init=epsg:4326")
    spdf <- sp::SpatialPolygonsDataFrame(spolys, data=df, match.ID = TRUE)
}


# match column name with dataset
#columnName('temp', 'SawKill', 'chemistry', columnNames)
columnName <- function(selection, location, metric, lookup) {
    colval <- paste0(location, "_", metric)
    lookup[lookup$selection==tolower(selection), tolower(colval)]
}

# Download all metadata, return as list
downloadMetadata <- function(apikey) {
    dat <- sapply(c("sawkill", "roejan"), function(x) {
        sapply(c("nutrients", "chemistry"), function(y) {
            z <- tryCatch(getMetadata(x, y, apikey), error=function(e) NULL)
            z
        }, simplify=FALSE)
    }, simplify=FALSE)
    d2 <- do.call(c, dat) 
    d2
}

# Download all column names, make a lookuptable
makeColNameTab <- function(metadatalist) {
#    dat <- sapply(c("sawkill", "roejan"), function(x) {
#        sapply(c("nutrients", "chemistry"), function(y) {
#            z <- tryCatch(getMetadata(x, y, apikey), error=function(e) NULL)
#            z
#        }, simplify=FALSE)
#    }, simplify=FALSE)
#    d2 <- do.call(c, dat) 
    metadatalist <- sapply(metadatalist, function(x) x$columnnames, simplify=FALSE)
    datanames <- sapply(metadatalist, rownames)
    filters <- c("date", "site", "year", "temperature", "turbidity", "ecoli", "conductivity", "coliform", "enterococc", "ammonium", "magnesium", "phosphorus", "calcium", "nitrate", "sodium", "chloride", "phosphate", "nitrogen", "sub_basin")

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
    lcdat <- read.csv("data/SawKillSubbasin_MasterMonitoring_Data_cleaned.csv", nrows=1)
    lcnames <- colnames(lcdat)
    lcnm <- unlist(sapply(filters, function(x) {
#        as.data.frame(sapply(lcnames, function(y) {
            z <- lcnames[grep(x, lcnames, ignore.case=TRUE)]
            if(length(z)) z[1]
            else NA
#        }, simplify=FALSE))
    }, simplify=FALSE))
    nmtab <- cbind(nmtab, sawkill_landuse=lcnm)
    nmtab
}

makeErrTab <- function(metadatalist) {
    mdl <- sapply(metadatalist, function(x) x$errorcodes, simplify=FALSE)
    mdl <- mdl[as.logical(unlist(lapply(mdl, length)))]
    if(length(mdl)) {
        mdl <- do.call(rbind.data.frame, mdl)
        rownames(mdl) <- 1:nrow(mdl)
        mdl
    }
}

# Some wrappers to remove error codes and compute statistics
rmErr <- function(x) {
    x <- x[x != -999]
    x <- x[x != -0.999]
    x <- x[x != -0.02]
    x <- x[x != -0.002]
    x <- x[x != -24197]
    x
}
notErr <- function(x) {
    x != -999 &
    x != -0.999 &
    x != -0.02 &
    x != -0.002 &
    x != -24197
}
rmErrMean <- function(x) {
    x <- rmErr(x)
    if(!length(x)) return(NA)
    round(mean(x, na.rm=TRUE), 3)
}
rmErrMin <- function(x) {
    x <- rmErr(x)
    if(!length(x)) return(NA)
    min(x, na.rm=TRUE)
}
rmErrMax <- function(x) {
    x <- rmErr(x)
    if(!length(x)) return(NA)
    max(x, na.rm=TRUE)
}
rmErrRange <- function(x) {
    x <- rmErr(x)
    if(!length(x)) return(NA)
    range(x, na.rm=TRUE)
}
rmErrLength <- function(x) {
    x <- rmErr(x)
    if(!length(x)) return(0)
    x <- na.omit(x)
    length(x)
}
rmErrGeom <- function(x) {
    x <- rmErr(x)
    if(!length(x)) return(NA)
    x <- na.omit(x)
    len <- length(x)
    # Avoid overflow if we can
    if(all(x > 0)) {
        xlog <- log(x)
        round(exp(sum(xlog) * (1/len)), 3)
    } else {
        prd <- prod(x, na.rm=TRUE)
        round(prd ^ (1/len), 3)
    }
}
rmErrMode <- function(x) {
    x <- rmErr(x)
    if(!length(x)) return(NA)
    x <- na.omit(x)
    breaks <- 20
    h <- hist(x, breaks=breaks, plot=FALSE)
    samebins <- TRUE
    while(samebins) {
        bin <- which(h$counts == max(h$counts))
        samebins <- length(bin) > 2
        if(samebins && breaks > 5) {
            breaks <- breaks - 5
            h <- hist(x, breaks=breaks, plot=FALSE)
        } else samebins <- FALSE
    }
    highct <- h$counts[bin]
    brk <- h$breaks[c(bin, bin+1)]
    if(length(bin)>1) {
        highct <- sum(highct)
        brk <- range(brk)
    }
    list(range=brk, n=highct)    
}
makeStatsDF <- function(x) {
    gmean <- rmErrGeom(x)
    fmean <- rmErrMean(x) 
    fmin <- rmErrMin(x) 
    fmax <- rmErrMax(x) 
    fN <- rmErrLength(x) 
    fmode <- rmErrMode(x) 
    if(!all(is.na(fmode))) fmode <- paste0(fmode$n, " samples between ", fmode$range[1], " and ", fmode$range[2])
    errs <- sum(!notErr(na.omit(x)))
    missing = sum(is.na(x))
    data.frame(GeometricMean=gmean, Mean=fmean, Min=fmin, Max=fmax, Mode=fmode, Samples=fN, Data_rows=length(x), Errors_Removed=errs, NA_Removed=missing) #Attribute=x, range=paste0(fmin, " - ", fmax), 
}


