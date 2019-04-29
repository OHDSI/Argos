#'Make report; render rmd file and excute html file
#'
#'@export

#Excute R markdown file
make_report <- function(){
    tryCatch({
        file.copy(from = paste0(.libPaths()[1], "/Argos/data/Argos_md.Rmd"), 
                  to   = getwd(), overwrite = T)
        
        markdown::render(paste0(getwd(),"/Argos_md.Rmd"), encoding = "UTF-8")
        browseURL(url = paste0(getwd(),"/Argos_md.html"))
        
    }, error = function(x){
        message("Need Rmd file.")
    })
    
    rm(list = ls())
}