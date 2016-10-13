#' High performance rasterize function using gdalUtils.
#'
#' This function rasterize a vector data by gdal binary utils. This is a wrap function of gdalUtils::gdal_rasterize.
#' @param x a Spatial*DataFrame objects.
#' @param field character.The column name of the variable to be transferred
#' @param res numeric.Resolution of the output raster
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
rasterize.gdal<-function(x,field,res=1e4,extent=NULL){
    library(rgdal)
    src_datasource.dsn<-file.path(tempdir(),'zonal.shp')
    if(file.exists(src_datasource.dsn)) file.remove(src_datasource.dsn)
    writeOGR(x,dsn = src_datasource.dsn,driver = 'ESRI Shapefile',layer = 'zonal',
             overwrite_layer = T)
    if(is.null(extent)){
        extent.x<-extent(x)
    }else{
        extent.x<-extent
    }
    dst_filename.dsn<-file.path(tempdir(),paste0(field,'.img') )
    gdalUtils::gdal_rasterize(src_datasource.dsn,dst_filename = dst_filename.dsn,a=field,
                   te =c(extent.x@xmin,extent.x@ymin,
                         extent.x@xmax,extent.x@ymax),
                   a_nodata=32768,of='HFA',tr=c(res,res),verbose=T,output_Raster=T)
    raster(dst_filename.dsn)
}

