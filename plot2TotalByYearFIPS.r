##

plot2TotalByYearFIPS <- function(dat = readNEIdata(),
                             file = "plot2TotalByYearFIPS.png",
                             dir = ".",
                             years = NULL,
                             FIPS = matrix(c("24510", "Baltimore City, MD"), ncol = 2)) {
    # Summarize data by year
    # Sets name for total PM2.5 tons
    dat <- dat[, sum(Emissions), by = c("year", "fips")]
    setnames(dat, c("year", "fips", "Total_PM2.5"))
    
    # Subset for 'years' if not NULL
    if(!is.null(years)) dat <- dat[year %in% years, ]
    
    # Subset for 'FIPS' if not NULL
    # Converts 'FIPS' to matrix to standardize function calls
    # If 'FIPS" is NULL, create FIPS matrix from 'dat'
    if(!is.null(FIPS)) {
        FIPS <- as.matrix(FIPS)
        dat[fips %in% FIPS[[1]], ]
    } else FIPS <- as.matrix(dat[["fips"]])
    
    # Define global parameters
    pch <- 3 # vertical cross
    numFIPS <- nrow(FIPS)
    nrows <- ceiling(sqrt(numFIPS))
    ncols <- ceiling(numFIPS / nrows)
    mfrow <- c(nrows, ncols)
    title <- "Total PM2.5 by Year"
    
    # Open PNG plot device to directory/file
    filename <- ifelse(dir == ".", file, paste(dir, file, sep = "/"))
    png(filename = filename, width = (480 * ncols), height = (480 * nrows))
    
    # Set global parameters
    par(pch = pch, mfrow = mfrow)
    
    # Create separate plots for each FIPS
    for(i in seq(numFIPS)) {
        # Define plot parameters
        main <- ifelse(ncol(FIPS) == 1, FIPS[i], FIPS[i, 2])
        thisDat <- dat[fips %in% FIPS[i, 1], ]
        xlab <- ""
        ylab <- "Tons PM2.5"
        col = "red"
        lwd <- 2
        
        # Create plot
        plot(thisDat$year, thisDat$Total_PM2.5, main = main,
             xlab = xlab, ylab = ylab, col = col)
        
        # Add regression line
        abline(lm(Total_PM2.5 ~ year, thisDat), lwd = lwd)
    }
    
    # Close PNG plot device
    dev.off()
    
    # Return filename
    filename
}