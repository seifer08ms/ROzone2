#' High performance rasterize function using gdalUtils.
#'
#' This function rasterize a vector data by gdal binary utils.
#' @param x a Spatial*DataFrame objects.
#' @param field character.The column name of the variable to be transferred
#' @param res numeric.Resolution of the output raster
#' @param extent extent object.Spatial extent of output raster.If missing, the extent of x will be used.
#' @export
#' @return RasterLayer
#' @examples
#' ###### the default path is 'data-raw/2010'. You should change it to your data folder. ###
#' grid.poly<-loadGRID()
#' plot(grid.poly)
#' ###### set the path of data #######
#' grid.poly<-loadGRID(datapath='data-raw/2010')
rasterize.gdal<-function(x,field,res=1e4,extent=NULL){
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
    gdal_rasterize(src_datasource.dsn,dst_filename = dst_filename.dsn,a=field,
                   te =c(extent.x@xmin,extent.x@ymin,
                         extent.x@xmax,extent.x@ymax),
                   a_nodata=32768,of='HFA',tr=c(res,res),verbose=T,output_Raster=T)
    raster(dst_filename.dsn)
}
