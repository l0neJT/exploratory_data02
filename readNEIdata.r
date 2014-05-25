## 

readNEIdata <- function(dir = "./data", srcCols = c("SCC", "Short.Name"),
                        datCols = NULL) {
    # Load libraries
    library(data.table)
    
    
    # Create vector with required file names
    reqFiles <- c("Source_Classification_Code.rds",
                  "summarySCC_PM25.rds")
    
    # Check directory for required files
    # Adds found to 'files' and missing to 'misFiles'
    files <- NULL
    misFiles <- NULL
    for(f in reqFiles) {
        file <- paste(dir, f, sep = "/")
        if(file.exists(file)) {
            files <- c(files, file)
        } else misFiles <- c(misFiles, file)
    }
    
    # Return error if files missing
    if(!is.null(misFiles)) stop(paste("Missing File:", misFiles, collapse = "\t"))
    
    # Read source codes and PM2.5 data
    sources <- readRDS(files[1])
    pm25Dat <- readRDS(files[2])
    print(1)
    
    # Limit columns to those pased in 'srcCols' and 'datCols'
    # Adds "SCC" if excluded from either column list; required for merge
    if(is.character(srcCols)) {
        if(!"SCC" %in% srcCols) srcCols <- c("SCC", srcCols)
        sources <- sources[, srcCols]
    }
    if(is.character(datCols)) {
        if(!"SCC" %in% datCols) datCols <- c("SCC", datCols)
        pm25Dat <- pm25Dat[, datCols]
    }
    print(2)
    
    # Convert to data.table
    sources <- data.table(sources)
    pm25Dat <- data.table(pm25Dat)
    print(3)
    
    # Return merged source codes and PM2.5 data
    merge(sources, pm25Dat, by = "SCC")
}