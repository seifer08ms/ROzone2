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
