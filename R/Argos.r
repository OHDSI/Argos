#'Run Argos, create R markdown HTML
#'
#'
#'@export

Argos <- function(){
    
    check.packages('ggplot2')
    check.packages('dplyr')
    check.packages('rmarkdown')
    check.packages('survival')
    check.packages('knitr')
    check.packages('reshape2')
    
    
}
# head(incidenceData_cohortId_1$data)
# A <- incidenceData_cohortId_1$data %>%
#     group_by(cohortStartYear,genderConceptId) %>%
#     summarise(cohortCount = sum(aggregatedNum) )
# reshape2::dcast(A,A$genderConceptId~A$cohortStartYear)
