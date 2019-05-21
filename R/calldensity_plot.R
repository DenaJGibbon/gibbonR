#' @title Call density plot
#' @description This function requires the user to input a file with GPS points and a file with call detections
#' @import sp
#' @import gstat
#' @import ggplot2
#' @export
#'

calldensityPlot <- function(gps.df, calltiming.df = calltiming.df, begin = 0, end = 1, option = "D") {
    
    if (!"recorder.id" %in% names(calltiming.df)) 
        stop("recorder.id does not exist")
    if (!"date" %in% names(calltiming.df)) 
        stop("date does not exist")
    if (!"time" %in% names(calltiming.df)) 
        stop("time does not exist")
    
    gps.pts <- gps.df
    
    calltiming.df <- calltiming.df[, c("recorder.id", "date", "time")]
    
    recorder.index <- unique(calltiming.df$recorder.id)
    
    interpolation.df <- data.frame()
    
    for (x in 1:length(recorder.index)) {
        index.sub <- recorder.index[x]
        gps.sub <- subset(gps.pts, Name == as.character(index.sub))
        detects.sub <- subset(calltiming.df, recorder.id == as.character(index.sub))
        new.df <- cbind.data.frame(gps.sub$Name, gps.sub$latitude, gps.sub$longitude, nrow(detects.sub))
        colnames(new.df) <- c("name", "y", "x", "n.detect")
        interpolation.df <- rbind.data.frame(interpolation.df, new.df)
    }
    
    interpolation.df.for.pts <- interpolation.df
    
    x.range <- as.numeric(c(min(interpolation.df$x), max(interpolation.df$x)))  # min/max longitude of the interpolation area
    y.range <- as.numeric(c(min(interpolation.df$y), max(interpolation.df$y)))  # min/max latitude of the interpolation area
    
    sp::coordinates(interpolation.df) = ~x + y
    grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 1e-05), y = seq(from = y.range[1], to = y.range[2], by = 1e-05))  # expand points to grid
    sp::coordinates(grd) <- ~x + y
    sp::gridded(grd) <- TRUE
    
    idw <- gstat::idw(formula = n.detect ~ 1, locations = interpolation.df, newdata = grd)  # apply idw model for the data
    ## [inverse distance weighted interpolation]
    idw.output = as.data.frame(idw)  # output is defined as a data table
    names(idw.output)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables
    
    vid.cols <- viridis::viridis(length(recorder.index), begin = begin, end = end, option = option)
    call.density.plot <- ggplot2::ggplot() + geom_tile(data = idw.output, aes(x = long, y = lat, fill = var1.pred)) + geom_point(data = interpolation.df.for.pts, aes(x = x, y = y), shape = 16, size = 3, 
        colour = "black") + scale_fill_gradient(low = max(vid.cols), high = min(vid.cols)) + xlab("Longitude") + ylab("Latitude") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
        theme(axis.text.x = element_text(size = 14)) + theme(axis.text.y = element_text(size = 14)) + theme(axis.title.y = element_text(size = 16)) + theme(axis.title.x = element_text(size = 16)) + 
        guides(fill = guide_legend(title = "Call Density")) + theme(legend.text = element_text(size = 16)) + theme(legend.title = element_text(size = 16))
    return(list(call.density.plot = call.density.plot))
}



