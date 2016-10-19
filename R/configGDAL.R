#' Configure GDAL binary utilities under windows.
#'
#' This function help to find without proxy or install GDAL binary utilities with proxy.
#' @param install_path character. path for unzip of GDAL binary utilities
#' @param proxy_url character indicating proxy url. The format is 'http://server_host:port'
#' @param extent extent object.Spatial extent of output raster.If missing, the extent of x will be used.
#' @export
#' @details If proxy_url is avaliable, this function will download gdal binary with proxy, and set the search path for gdal_setInstallation automatically.
#' @details If proxy_url is missing, this function will open the download page with browser because author can't find a good netdisk to host these files.The only way to download them quickly is to download files by yourself.
#' @examples
#' install.gdal(install_path='/tmp/gisinter',proxy_url="http://127.0.0.1:8087/")
install.gdal<-function(install_path=NULL,proxy_url=NULL){
    if (.Platform$OS.type!='unix'){
        if(R.Version()$arch!='x86_64'){
            url<-'http://download.gisinternals.com/sdk/downloads/release-1500-gdal-1-11-4-mapserver-6-4-3.zip'
            if(is.null(proxy_url)){
                url<-'https://pan.baidu.com/s/1miHGTyK'
            }
        }else{
            url<-'http://download.gisinternals.com/sdk/downloads/release-1500-x64-gdal-1-11-4-mapserver-6-4-3.zip'
            if(is.null(proxy_url)){
                url<-'https://pan.baidu.com/s/1pKR3X6F'
            }
        }
        if(!is.null(proxy_url)){
            temp <- file.path(tempdir(),'gisinter.zip')
            Sys.setenv(http_proxy  = proxy_url)
            download.file(url,temp)
            Sys.unsetenv('http_proxy')
            unzip(temp,exdir =install_path,overwrite = T)
            gdalUtils::gdal_setInstallation(search_path =
                                                file.path(install_path,'bin/gdal/apps'))
        }else{
            browseURL(url)
        }
    }# end of if
}
