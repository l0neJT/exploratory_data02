##

plot3TotalByYearType <- function(dat = readNEIdata(),
                                 file = "plot3TotalByYearType.png",
                                 dir = ".",
                                 years = NULL,
                                 types = NULL,
                                 FIPS = matrix(c("24510", "Baltimore City, MD"), ncol = 2)) {
    # Load libraries
    library(data.table)
    library(ggplot2)
    
    # Summarize data by year, fips, and type
    # Sets name for total PM2.5 tons
    dat <- dat[, sum(Emissions), by = c("year", "fips", "type")]
    setnames(dat, c("year", "fips", "type", "Total_PM2.5"))
    
    # Subset for 'years' if not NULL
    if(!is.null(years)) dat <- dat[year %in% years, ]
    
    # Subset for 'types' if not NULL
    if(!is.null(types)) dat <- dat[type %in% types, ]
    
    # Subset for 'FIPS' if not NULL
    # Converts 'FIPS' to matrix to standardize function calls
    # If 'FIPS" is NULL, create FIPS matrix from 'dat'
    if(!is.null(FIPS)) {
        FIPS <- as.matrix(FIPS)
        dat[fips %in% FIPS[[1]], ]
    } else FIPS <- as.matrix(dat[["fips"]])
    
    # Add FIPS Name column by either merging from FIPS or by copying 'fips
    # Assumes first column = FIPS and second column = FIPS Name
    if(ncol(FIPS) == 1) FIPS[2] <- FIPS[1]
    FIPS <- FIPS[, 1:2, drop = FALSE]
    colnames(FIPS) <- c("fips", "fips_Name")
    dat <- merge(FIPS, dat, by = "fips")
    
    # Define plot parameters
    geom <- c("point", "smooth")
    method <- "lm"
    xlab <- ""
    ylab <- "Tons PM2.5"
    dpi <- 90
    width <- 1440 / dpi
    height <- 900 / dpi * nrow(FIPS)
    
    # Create plot
    p <- qplot(year, Total_PM2.5, data = dat, geom = geom, method = method, se = FALSE,
               xlab = xlab, ylab = ylab, col = type, facets = fips_Name ~ .)
    
    # Save plot
    filename <- ifelse(dir == ".", file, paste(dir, file, sep = "/"))
    ggsave(filename = filename, plot = p, width = width, height = height, dpi = dpi)
    
    # Return filename
    filename
}