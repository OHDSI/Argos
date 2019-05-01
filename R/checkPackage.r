#' check and ready packages needed
#' @param pkgname   package name
#' @export

check.packages <- function(pkgname){
    if(!(pkgname %in% installed.packages()[,"Package"]) ){
        install.packages(pkgname)
        tryCatch(
            library(pkgname, character.only = TRUE)
        )
    } else {
        library(pkgname, character.only = TRUE)
    }
}

