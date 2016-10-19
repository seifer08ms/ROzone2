#' Download and configure GDAL binary utilities under windows.
#' This function help to download and install GDAL binary utilities.
#' @param install_path character. path for unzip of GDAL binary utilities
#' @param proxy_url character indicating proxy url. The format is 'http://server_host:port'
#' @return returns an logical value which represents whether GDAL binary installing is success
#' @export
#' @details This function will download gdal binary(with proxy), and set the search path for gdal_setInstallation automatically.
#' @examples
#' install.gdal(install_path='/c/gdalwin')
install.gdal<-function(install_path='c:/gdal_map_win',proxy_url=NULL){
    u<-'https://codeload.github.com/seifer08ms/gisinter_gdal/zip/'
    if (.Platform$OS.type!='unix'){
        if(R.Version()$arch!='x86_64'){
            ostype<-'win32'
        }else{
            ostype<-'win64'
        }
        url<-paste0(u,ostype)
        temp <- file.path(tempdir(),'gisinter.zip')
        if(!is.null(proxy_url)){
            Sys.setenv(http_proxy  = proxy_url)
            download.file(url,temp,mode='wb')
            Sys.unsetenv('http_proxy')
        }else{
            download.file(url,temp,mode = 'wb')
        }
        unzip(temp,exdir =install_path,overwrite = T)
        gdal_path<-file.path(install_path,
                             paste('gisinter_gdal',ostype,sep='-'),
                             paste('gisinter',ostype,sep='_'),
                             'bin')
        cat(gdal_path,'\n')
        WPATH <- Sys.getenv("PATH")
        WPATH1 <- paste(gdal_path, WPATH, sep=";")
        Sys.setenv(PATH=WPATH1)
        gdalUtils::gdal_setInstallation(search_path =gdal_path)
        return(!is.null(getOption("gdalUtils_gdalPath")))
    }# end of if
}
