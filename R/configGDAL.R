#' @title Download and configure GDAL binary utilities under windows.
#' @description  This function help to download and install GDAL binary utilities.
#' @param install_path character. path for unzip of GDAL binary utilities
#' @param proxy_url character indicating proxy url. The format is 'http://server_host:port'
#' @param manual logical indicating whether to download gdal binanry tools manually.It only works for windows.The default value is false.If it is true ,the download page will be opened in browser.
#' @param url character indicating the remote repository which contains gdal binary tools to clone.If you are not sure about this parameter,do not change its value.
#' @return returns an logical value which represents whether GDAL binary installing is successful
#' @import git2r
#' @export
#' @details This function will download gdal binary(with proxy), and set the search path for gdal_setInstallation automatically.
#' @details If manual if true, you have to download files by your hands.Functions will wait until your hit enter to continue and try to complete the rest configuration.
#' @examples
#' install.gdal(install_path='/c/gdalwin')
install.gdal<-function(install_path='c:/gdal_map_win',proxy_url=NULL,manual=F,
                       url="https://git.oschina.net/seifer_08ms/gisinter_gdal.git"){
    if (.Platform$OS.type!='unix'){
        if(R.Version()$arch!='x86_64'){
            ostype<-'win32'
            url.baidupan<-'https://pan.baidu.com/s/1nvTkrLv'
        }else{
            ostype<-'win64'
            url.baidupan<-'https://pan.baidu.com/s/1i4ImQYL'
        }
        # try to set the path of exsiting folder
        gdal_path<-file.path(install_path,
                             # paste('gisinter_gdal',ostype,sep='-'),
                             paste('gisinter',ostype,sep='_'),
                             'bin')
        if(!dir.exists(install_path)){
            if(manual){
                cat('Please download and extract zip files into ',
                    install_path,
                    'from your browser ...\n')
                cat('Please pay attention to keep folder structure as ',gdal_path,'\n')
                dir.create(install_path)
                cat ("Press [enter] to continue ",
                     "to open download page ...",'\n')
                line <- readline()
                browseURL(url.baidupan)
                cat ("Press [enter] to continue ",
                     "if you have already finished the previous step ...",'\n')
                line <- readline()
            }else{
                #automatically download gdal binary tools
                if(!dir.exists(install_path)){
                    # need to clone files into install_path
                    #library(git2r)
                    dir.create(install_path)
                    if(!is.null(proxy_url)){
                        Sys.setenv(http_proxy  = proxy_url)
                        #clone under proxy
                        repo <- git2r::clone(url = url,
                                             local_path =install_path,
                                             branch = ostype)
                        Sys.unsetenv('http_proxy')
                    }else{
                        #clone directly
                        repo <- git2r::clone(url = url,
                                             local_path =install_path,
                                             branch = ostype)
                    }
                }
            }
        }# end of  if(!dir.exists(install_path)){
        cat('try to set the path for gdal binary:','\n')
        cat(gdal_path,'\n')
        WPATH <- Sys.getenv("PATH")
        WPATH1 <- paste(gdal_path, WPATH, sep=";")
        Sys.setenv(PATH=WPATH1)
        gdalUtils::gdal_setInstallation(search_path =gdal_path)

    }else{
        # if os is unix or linux, we should directly set installation of gdal binary Utils
        gdalUtils::gdal_setInstallation()
    }# end of ostype if block
    # return logical value indicating system
    return(!is.null(getOption("gdalUtils_gdalPath")))
}
