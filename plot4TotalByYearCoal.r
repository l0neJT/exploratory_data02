##

plot4TotalByYearCoal <- function(dat = readNEIdata(srcCols = c("SCC", "EI.Sector")),
                                 file = "plot4TotalByYearCoal.png",
                                 dir = ".",
                                 years = NULL) {
    # Load libraries
    library(data.table)
    library(ggplot2)
    
    # Summarize data by year and EI sector
    # Sets name for total PM2.5 tons
    dat <- dat[, sum(Emissions), by = c("year", "EI.Sector")]
    setnames(dat, c("year", "EI.Sector", "Total_PM2.5"))
    
    # Subset for 'years' if not NULL
    if(!is.null(years)) dat <- dat[year %in% years, ]
    
    # Subset for coal
    isCoal <- function(txt) {grepl("coal", txt, ignore.case = TRUE)}
    dat <- dat[sapply(dat$EI.Sector, isCoal), ]
    
    # Define plot parameters
    geom <- c("point", "smooth")
    method <- "lm"
    xlab <- ""
    ylab <- "Tons PM2.5"
    dpi <- 72
    width <- 720 / dpi
    height <- 480 / dpi
    
    # Create plot
    p <- qplot(year, Total_PM2.5, data = dat, geom = geom, method = method,
               col = EI.Sector, xlab = xlab, ylab = ylab)
    
    # Save plot
    filename <- ifelse(dir == ".", file, paste(dir, file, sep = "/"))
    ggsave(filename = filename, plot = p, width = width, height = height, dpi = dpi)
    
    # Return filename
    filename
}