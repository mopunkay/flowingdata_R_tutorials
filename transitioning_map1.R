##################################################################
# Create a transitioning map: part 1
##################################################################
# following transitioning map tutorial series from FlowingData

# install and load relevant packages
packages <- c("maptools", "rgdal", "akima", "rgeos", "reshape", "prevR")
install.packages(packages, dependencies = TRUE)

library(maptools)
library(rgdal)
library(akima)
library(rgeos)
library(reshape)
library(prevR)

# set directories
wd <- "~/Documents/sample R stuff/interpolation_tutorial_part1"

# load state boundary shapefile
state_shp <- "data/basemaps/cb_2013_us_state_20m/cb_2013_us_state_20m.shp"
states <- readShapePoly(paste(wd, state_shp, sep = "/"))
states <- states[!states$STATEFP %in% c("02", "15", "72"), ]

# load county boundary shapefile
county_shp <- "data/basemaps/cb_2013_us_county_20m/cb_2013_us_county_20m.shp"
counties <- readShapePoly(paste(wd, county_shp, sep = "/"))
counties <- counties[!counties$STATEFP %in% c("02", "15", "72"), ]

# load county level data
foodstamp_csv <- "data/ACS_13_5YR_S2201"
foodstamps <- read.csv(paste(wd, foodstamp_csv, "ACS_13_5YR_S2201.csv", sep = "/"),
                       colClasses = c(GEO.id2 = "character"))

# calculate county centroids in order to do interpolation
centroids <- SpatialPointsDataFrame(gCentroid(counties, byid = TRUE),
                                    counties@data,
                                    match.ID = FALSE)

# plot centroids themselves on county map
plot(counties, border="#cccccc", lwd = 0.5)
plot(centroids, add = TRUE, pch = 19, cex = 0.1)

# order foodstamps data to match order of centroid data
foodstamps_ord <- foodstamps[match(centroids@data$GEOID, foodstamps$GEO.id2), ]

# calculate avg food stamps per household
stamp_rate <- foodstamps_ord$HC02_EST_VC01 / foodstamps_ord$HC01_EST_VC01

# set vectors of coordinates and values for interp function
x <- centroids@coords[,"x"]
y <- centroids@coords[,"y"]
z <- stamp_rate

# set grid granularity
xo <- seq(states@bbox["x", "min"], states@bbox["x", "max"], length = 500)
yo <- seq(states@bbox["y", "min"], states@bbox["y", "max"], length = 300)

# smooth foodstamp rates
foodstamps_smooth <- interp(x, y, z, xo, yo, extrap = TRUE, linear = FALSE)

# make colors and breaks for image function
colors <- c("white", "#cccccc", "#999999", "#666666", "#444444", "#000000")
breaks <- c(-99, 0.05, 0.10, 0.15, 0.2, 0.25, 99)

# draw the grid of values without regard to country boundaries
image(foodstamps_smooth, col = colors, breaks = breaks, asp = 1)

# put x, y, and z coords in the right format for boundarying function
metric_x <- foodstamps_smooth$x
metric_y <- foodstamps_smooth$y
metric_z <- foodstamps_smooth$z
metric_mat <- matrix(metric_z,
                     nrow = length(metric_x),
                     ncol = length(metric_y),
                     dimnames = list(metric_x, metric_y))
metric_df <- melt(metric_mat)

# mark each point as either inside or outside of state boundaries
metric_df$keep <- point.in.SpatialPolygons(metric_df[, 1], metric_df[, 2], states)

# create matrix of just inside-or-outside indicators
country_mat <- matrix(metric_df$keep,
                      nrow=length(metric_x),
                      ncol=length(metric_y),
                      dimnames = list(metric_x, metric_y))

# remove all points in value matrix that are not in the country
metric_mat[which(country_mat == FALSE)] <- NA

# replace value matrix in smoothed objest with in-country value matrix
foodstamps_country <- foodstamps_smooth
foodstamps_country$z <- metric_mat

# redraw image to include only in-country points
image(foodstamps_country, col = colors, breaks = breaks, asp = 1)

# now make this plot with some different color schemes and tweaks
## purple
purples <- colorRampPalette(c("white", "purple"))
image(foodstamps_country, col = purples(6), breaks = breaks, asp = 1, axes = FALSE)

## blue-green
bluegreen <- colorRampPalette(c("blue", "green"))
image(foodstamps_country, col = bluegreen(6), breaks = breaks, asp = 1, axes = FALSE)

## make version with continuous color shading
more_breaks <- seq(0, .5, by = 0.02)
pal <- colorRampPalette(c("white", "blue", "black"))
image(foodstamps_country, col = pal(length(more_breaks) - 1),
      breaks = more_breaks, asp = 1, axes = FALSE)

## reverse the colors
image(foodstamps_country, col = rev(pal(length(more_breaks) - 1)),
      breaks = more_breaks, asp = 1, axes = FALSE)

## add state borders
image(foodstamps_country, col = pal(length(more_breaks) - 1),
      breaks = more_breaks, asp = 1, axes = FALSE)
plot(states, add = TRUE, border = "white", col = NA)



