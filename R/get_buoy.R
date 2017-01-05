#' Get data from NDBC buoy
#'
#' @param buoyid (5-letter code)
#' @param scope (5d, 45d, histM, histY, lastM)
#' @param date (3-letter month e.g., Jan, Feb, Mar OR 4-digit year)
#'               only for histM and histY
#'
#' @return data.frame
#' @export
#'
#' @examples
getNDBC <- function(buoyid, scope = "5d", date = NULL) {

	buoyid <- toupper(buoyid)

	#**** CONSTRUCT URLs ***************************************************

    urlbase <- "http://www.ndbc.noaa.gov/data/"
    if (scope == "45d") {
        urldir <- "realtime2/"
        urltail <- ".txt"
        url <- paste0(urlbase, urldir, buoyid, urltail)
    } else if (scope == "5d") {
        urldir <- "5day2/"
        urltail <- "_5day.txt"
        url <- paste0(urlbase, urldir, buoyid, urltail)
    } else if (scope == "histM") {
    	months <- c("Jan","Feb","Mar","Apr","May","Jun",
    				"Jul","Aug","Sep","Oct","Nov","Dec")
        monthNum <- match(date, months)
        urldir <- "stdmet/"
        urltail <- paste0(monthNum, format(Sys.Date(), "%Y"), ".txt.gz")
        url <- paste0(urlbase, urldir, date, "/", tolower(buoyid), urltail)
    } else if (scope == "histY") {
    	urldir <- "historical/stdmet/"
        urltail <- ".txt.gz"
        url <- paste0(urlbase, urldir, tolower(buoyid), "h", date, urltail)
    } else if (scope == "lastM") {
        urldir <- "l_stdmet/"
        urltail <- ".txt"
        url <- paste0(urlbase, urldir, tolower(buoyid), urltail)
    }

    #**** DOWNLOAD / READ-IN ***********************************************

    if (scope %in% c("5d", "45d", "lastM")) {
    	# get column names from top of file
	    names <- unlist(strsplit(substring(readLines(url,1),2),
	    				split="[[:space:]]+"))
	    # read in file
	    if (scope == "lastM")
            NAs <- c("99.00","999","999.0","99.0","9999.0","")
        else
            NAs <- "MM"
        dat <- read.table(url, sep="", col.names=names, skip=2, na.strings=NAs,
	    			        colClasses="character", comment.char="", fill=T)
    }

    if (scope %in% c("histM", "histY")) {
    	temp <- tempfile()
    	download.file(url, temp, method="curl", quiet=T)

    	# get column names from top of file
	    names <- unlist(strsplit(substring(readLines(gzfile(temp),1),1),
	    				split="[[:space:]]+"))
	    # read in file
	    nastrings <- c("99.00","999","999.0","99.0","9999.0","")
	    dat <- read.table(gzfile(temp), sep="", col.names=names, skip=2, fill=T,
	    	colClasses="character", na.strings=nastrings, comment.char="")
	    unlink(temp)
    }

    #**** MUNGE ************************************************************

	# remove totally-NA columns
    dat <- dat[, !apply(dat, 2, function(x) all(is.na(x)))]
    colnames(dat)[1] <- "YY"
    if (nchar(dat$YY) == 2)
        dat$YY <- sapply(dat$YY, function(x) paste0("19", x))
    if (!('mm' %in% names(dat))) {
        dat$mm <- '00'
        dat <- dat[, c(1:4, ncol(dat), 5:(ncol(dat) - 1))]
    }
    # merge date/time columns
    dat$date <- strptime(paste(dat$YY,dat$MM,dat$DD,dat$hh,dat$mm, sep="-"),
    						format="%Y-%m-%d-%H-%M")
    dat <- dat[, -c(1:5)]
    # convert columns to numeric
    for(col in c(1, 2:(ncol(dat) - 1)))
        dat[, col] <- as.numeric(dat[, col])
    # convert date class
    dat$date <- as.POSIXct(dat$date)
    # put date in first column
    dat <- dat[, c(ncol(dat), 1:(ncol(dat) - 1))]
    # recode columns that have alternate names
    if ('BAR' %in% names(dat))
        colnames(dat)[colnames(dat)=="BAR"] <- "PRES"
	if ('WD' %in% names(dat))
	    colnames(dat)[colnames(dat)=="WD"] <- "WDIR"

    return(dat)
}
