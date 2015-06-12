


    
    install.packages(c("maps", "mapproj"));
    counties <- readRDS("~/Coursera/05 Reproducible Research/counties.rds");
    head(counties, n=10);
    
    library(maps)
    library(mapproj)
    helpers <- source("~/Coursera/05 Reproducible Research/helpers.R")
    counties <- readRDS("~/Coursera/05 Reproducible Research/counties.rds")
    percent_map(counties$white, "purple", "% white")
    
    
