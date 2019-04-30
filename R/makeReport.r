#'Make report; render rmd file and excute html file
#'
#'@export

#Excute R markdown file
make_report <- function(){
    tryCatch({
        file.copy(from = paste0(.libPaths()[1], "/Argos/data/Argos_md.Rmd"), 
                  to   = outputFolder, overwrite = T)
        
        markdown::render(file.path(outputFolder,"argos_document.Rmd"), encoding = "UTF-8")
        browseURL(url = file.path(outputFolder,"argos_document.html"))
        
    }, error = function(x){
        message("Need Rmd file.")
    })
    
    rm(list = ls())
}