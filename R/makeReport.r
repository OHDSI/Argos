#'Make report; render rmd file and excute html file
#'
#'@export

#Excute R markdown file

make_report <- function(){
    file.copy(from = file.path(getwd(), "data/argos_document.Rmd"), 
              to = outputFolder, overwrite = T)
    rmarkdown::render(file.path(outputFolder,"argos_document.Rmd"), encoding = "UTF-8")
}

