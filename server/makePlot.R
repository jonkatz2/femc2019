
dat <- subsetTable()
dat <- dat$data
location <- input$location
metric <- navigation$currentmetric
if(length(dat)) {
    yax <- columnName(input$yaxis, location, metric, columnNames)
    xax <- columnName(input$xaxis, location, metric, columnNames)
    spl <- columnName(input$split, location, metric, columnNames)
    type <- input$graphtype
    if(all(c(yax, xax) != "")) {
        if(type != "barplot") {
            ylim <- range(dat[,yax], na.rm = TRUE)
            if(input$split == "year") {
                dateinfo <- as.POSIXlt(dat[,columnName("date", location, metric, columnNames)])
                dat$day <- dateinfo$yday
                monthnames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")
                dat$monthday <- paste(monthnames[dateinfo$mon+1], dateinfo$mday)
                dat.spl <- split(dat, f=dat[, spl])
                monthstarts <- as.POSIXlt(c("2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-05-01", "2019-06-01", "2019-07-01", "2019-08-01", "2019-09-01", "2019-10-01", "2019-11-01", "2019-12-01"))$yday
                years <- unique(dat[,spl])
                cols <- rainbow(length(years))
                plot(dat.spl[[1]][,'day'], dat.spl[[1]][,yax], ylab = yax, xlab = "Month", type=type, col=cols[1], pch=19, ylim=ylim, xaxt='n', xlim=c(1, 365), xaxs="i")
                rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightgray", border=NA)
                abline(v=monthstarts, col='white', lty=3)
                abline(h=pretty(dat[,yax]), col='white', lty=3)
                axis(1, at=monthstarts, labels=monthnames)
                graphics::box()
                sitecol <- columnName("site", location, metric, columnNames)
                for(i in 1:length(dat.spl)) {
                    sitedat <- split(dat.spl[[i]], dat.spl[[i]][,sitecol])
                    for(j in 1:length(sitedat)) {
                        if(type %in% c("l", "o")) lines(sitedat[[j]][,"day"], sitedat[[j]][,yax], col=cols[i], lwd=2)
                        if(type %in% c("o", "p")) points(sitedat[[j]][,"day"], sitedat[[j]][,yax], col=cols[i], pch=19, cex=1.5)
#                        axis(1, sitedat[[j]][,"day"], labels=sitedat[[j]][,"monthday"], las=3, cex.axis=0.7)
                    }
                }              
                args.leg <- list(x="topright", legend=years, col=cols, bty="n")
                if(type %in% c("l", "o")) args.leg <- c(args.leg, list(lty=1, lwd=2))
                if(type %in% c("o", "p")) args.leg <- c(args.leg, list(pch=19, pt.cex=1.5))
                do.call(legend, args.leg)
#                legend("topright", legend=years, col=cols, lty=1, pch=19, cex=1.5, lwd=2, bty="n")
                
            } else {
                dat.spl <- split(dat, f=dat[, spl])
                sites <- unique(dat[, spl])
                cols <- rainbow(length(sites))
                xaxval <- pretty(dat[,xax], n=12)
    #	                    cols <- terrain.colors(length(dat.spl))
                plot(dat.spl[[1]][,xax], dat.spl[[1]][,yax], ylab = yax, xlab = xax, type=type, col=cols[1], pch=19, ylim=ylim, xaxt='n')#
                rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightgray", border=NA)
                abline(v=xaxval, col='white', lty=3)
                abline(h=pretty(dat[,yax]), col='white', lty=3)
    #                        grid(nx=length(xaxval), col='white')
                for(i in 1:length(dat.spl)) {
                    if(type %in% c("l", "o")) lines(dat.spl[[i]][,xax], dat.spl[[i]][,yax], col=cols[i], lwd=2)
                    if(type %in% c("o", "p")) points(dat.spl[[i]][,xax], dat.spl[[i]][,yax], col=cols[i], pch=19, cex=1.5)
                }
                graphics::box()
                axis(1, at=xaxval, labels=as.character(xaxval))
                
                args.leg <- list(x="topright", legend=sites, col=cols, bty="n")
                if(type %in% c("l", "o")) args.leg <- c(args.leg, list(lty=1, lwd=2))
                if(type %in% c("o", "p")) args.leg <- c(args.leg, list(pch=19, pt.cex=1.5))
                do.call(legend, args.leg)
#                legend("topright", legend=sites, col=cols, lty=1, pch=19, cex=1.5, lwd=2, bty="n")
            }
        } else {
#                    browser()
            height <- reshape2::acast(dat, as.formula(paste(yax, "~", xax, "+", spl)), mean, value.var=yax)
            barplot(height, beside=TRUE)
        }
    }
}
