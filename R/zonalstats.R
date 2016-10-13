#' High performance zonal statistics function.
#'
#' This function calculate statistics for each zone defined by a zone dataset.
#' @import data.table
#' @param x the raster dataset to be calculated
#' @param zonal.ras a raster objects indicating each zone
#' @param stat character.statistics method to be performed.The defautl value is 'mean'.
#' @param digit integer indicating the number of decimal places (round) for zone id.The default value is 0.
#' @param na.rm logic indicating whether to remove NA value during the zonal statistic.The default value is true
#' @param prefix character indicating column name prefix of zonal statistical value.
#' @export
#' @return RasterLayer
#' @examples
#' gg<-loadGRID()
#' res.test<-1e4
#' zonal.ras<-rasterize.gdal(china.pop[,'county_ID'],'county_ID',res=res.test)
#' grid.poly.proj<-gg%>%spTransform(proj4string(china.pop))
#' o3.test1<-rasterize.gdal(grid.poly.proj,names(grid.poly.proj)[2],res=res.test,
#'                          extent=extent(china.pop))
#' zonal.test<-zonalStat(zonal.ras,val.ras =o3.test1)
#' ##############  batch processing ######
#' o3.stack.3<-lapply(names(grid.poly.proj)[-1],rasterize.gdal,
#' x=grid.poly.proj,res=res.test,extent=extent(china.pop))
#' zonal.res.all<-lapply(X=o3.stack.3,FUN = zonalStat,zonal.ras=zonal.ras)

zonalStat<-function(x,zonal.ras,stat='mean',digits=0,na.rm=T,prefix='zonal',...){

    library(data.table)
    library(raster)
    fun <- match.fun(stat)
    vals <- getValues(x)
    zones <- round(getValues(zonal.ras), digits = digits)
    rDT <- data.table(vals, z=zones)
    setkey(rDT, z)
    Zstat<-data.frame(rDT[, lapply(.SD, fun,na.rm=na.rm,...), by=z])
    colnames(Zstat)[2:length(Zstat)]<-paste(prefix,stat,sep="_")
    return (Zstat)
}

