getsurvData<-function(plpData = plpData,
                      population = population){
    #get age data
    ageData <- as.data.frame(plpData$covariates) %>%
            filter( covariateId == 1002 ) %>% 
            rename( age = covariateValue ) %>%
            select( rowId, age)
    
    #get genderConceptId data
    genderData <- as.data.frame(plpData$covariates) %>%
        filter( covariateId != 1002 ) %>%
        mutate( genderConceptId = if_else(covariateId == 8507001, 8507, 8532)) %>%
        select(rowId, genderConceptId)
    
    #select subjectId, age, genderConceptId, diagnosisYr, survivalTime
    readysurvData <- population %>%
        left_join(ageData, by = "rowId") %>%
        left_join(genderData, by = "rowId") %>%
        mutate( startYear = lubridate::year(cohortStartDate),
                birthYear = lubridate::year(cohortStartDate)-age) %>%
        select(subjectId, age, genderConceptId, startYear, birthYear, outcomeCount, survivalTime)
}



