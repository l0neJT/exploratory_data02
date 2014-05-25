##

plot1TotalByYear <- function(dat = readNEIdata(),
                             file = "plot1TotalByYear.png",
                             dir = ".",
                             years = NULL) {
    # Load libraries
    library(data.table)
    
    # Summarize data by year
    # Sets name for total PM2.5 tons
    dat <- dat[, sum(Emissions), by = year]
    setnames(dat, c("year", "Total_PM2.5"))
    
    # Subset for 'years' if not NULL
    if(!is.null(years)) dat <- dat[year %in% years, ]
    
    # Define plot parameters
    pch <- 3 # vertical cross
    main <- "Total PM2.5 by Year"
    xlab <- ""
    ylab <- "Tons PM2.5"
    col = "red"
    lwd <- 2
    
    # Open PNG plot device to directory/file
    filename <- ifelse(dir == ".", file, paste(dir, file, sep = "/"))
    png(filename = filename)
    
    # Set global parameters
    par(pch = pch)
    
    # Create plot
    plot(dat$year, dat$Total_PM2.5, main = main, xlab = xlab, ylab = ylab, col = col)
    
    # Add regression line
    abline(lm(Total_PM2.5 ~ year, dat), lwd = lwd)
    
    # Close PNG plot device
    dev.off()
    
    # Return filename
    filename
}