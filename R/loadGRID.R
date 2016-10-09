#' Generate a grid polygon of all daily rozone dataset in a single year.
#'
#' This function generatea a grid-shape polygons of  all daily rozone dataset in one year.
#' @param datapath the path of year folder,the default value is data-raw/2010
#' @export
#' @return a spatial polygon data frame which contains daily datasets in this year.Every daily value will attached as a columns.
#' @examples
#' ###### the default path is 'data-raw/2010'. You should change it to your data folder. ###
#' grid.poly<-loadGRID()
#' plot(grid.poly)
#' ###### set the path of data #######
#' grid.poly<-loadGRID(datapath='data-raw/2010')
loadGRID<-function(datapath='data-raw/2010'){
    # datapath<-'data/2010'
    year<-basename(datapath)
    CMAQ_files.path.full<-list.files(path=datapath,pattern = 'txt$',
                                     include.dirs = T,full.names = T)
    library(dplyr)
    library(tidyr)
    library(rgdal)
    library(maptools)
    china_grid<-read.csv(CMAQ_files.path.full[1],header = F,sep='')
    names(china_grid)<-c('row_id','col_id','grid_id',
                         c('lat','long')%>%rep(5)%>%paste0(rep(1:5,each=2)),
                         'O3')
    lat.col<-3+seq(1,by=2,length.out = 5)
    long.col<-lat.col+1
    create.poly.i<-function(i){
        lat.i<-china_grid[i,lat.col]%>%stack%>%'['(,1)
        long.i<-china_grid[i,long.col]%>%stack%>%'['(,1)
        coords.i<-cbind(long.i,lat.i)
        Polygon(coords.i)%>%list%>%Polygons(paste0('pts',i))
    }
    grid.cache<-'data/grid_rozone.rds'
    if(!file.exists(grid.cache)){
        grid.poly<-lapply(1:nrow(china_grid),FUN =create.poly.i )%>%
        SpatialPolygons(1:nrow(china_grid))
    proj4string(grid.poly)<-'+init=epsg:4326'
        saveRDS(grid.poly,file = grid.cache)
    }else{
        grid.poly<-readRDS(grid.cache)
    }
    # sapply(slot(grid.poly, "polygons"), slot, "area")
    # different areas per polygon
    library(stringr)
    CMAQ_files.names<-list.files(path=datapath,pattern = 'txt$')%>%
        str_replace_all(pattern = paste0('CMAQ_China_8hmax_O3\\.',year,'|\\.txt'),
                        replacement = '')
    nfiles<-length(CMAQ_files.names)
    read_days<-function(path){
        read.csv(path,sep = '',header = F)%>%dplyr::select(one_of('V3','V14'))%>%
            rename(grid_id=V3,O3=V14)
    }
    o3_days<-lapply(CMAQ_files.path.full, read_days)
    names(o3_days)<-paste0('O3_',CMAQ_files.names)
    o3_days.cbind<-do.call('cbind',o3_days)%>%rename(id=O3_152.grid_id)%>%
        dplyr::select(-ends_with('grid_id'))
    colnames(o3_days.cbind)<-str_replace_all(colnames(o3_days.cbind),'\\.O3','')
    grid.poly$id<-1:nrow(o3_days.cbind)
    grid.poly@data<-o3_days.cbind
    grid.poly
}
