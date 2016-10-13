#' High performance zonal statistics function.
#'
#' This function calculate statistics for each zone defined by a zone dataset.
#' @param zonal.ras a raster objects indicating each zone
#' @param val.ras the raster dataset to be calculated
#' @param stat character.statistics method to be performed
#' @param extent extent object.Spatial extent of output raster.If missing, the extent of x will be used.
#' @export
#' @return RasterLayer
#' @examples
#' ###############################
#' # rasterize points
#' ###############################
#' r <- raster(ncols=36, nrows=18)
#' n <- 1000
#' x <- runif(n) * 360 - 180
#' y <- runif(n) * 180 - 90
#' xy <- cbind(x, y)
#' # with a SpatialPointsDataFrame
#' vals <- 1:n
#' p <- data.frame(xy, name=vals)
#' coordinates(p) <- ~x+y
#' ## If you are using windows,you must set local GDAL installation options before running function###
#' #Not run:
#' #gdal_setInstallation(search_path = 'the path of OSGeo4W64/bin',rescan = T)
#' r2 <- rasterize.gdal(p, field = 'name', res =res(r)[1],extent=extent(r))
zonalStat<-function(zonal.ras,val.ras,stat='mean'){
    myZonal <- function (x, z, stat, digits =0, na.rm = TRUE,
                         ...) {
        library(data.table)
        library(raster)
        fun <- match.fun(stat)
        vals <- getValues(x)
        zones <- round(getValues(z), digits = digits)
        rDT <- data.table(vals, z=zones)
        setkey(rDT, z)
        rDT[, lapply(.SD, fun), by=z]
    }
    Zstat<-data.frame(myZonal(val.ras, zonal.ras, stat=stat))
    colnames(Zstat)[2:length(Zstat)]<-
        paste0("B", c(1:(length(Zstat)-1)), "_",stat)
    return (Zstat)
}
