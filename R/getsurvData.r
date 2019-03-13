
#' get outcome, survivaltime, needed variables 
#' @param plpData           outcome of PatientLevelPrediction package getPlpData code
#' @param population        outcome of PatientLevelPrediction package createStudyPopulation code
#' @import dplyr
#' @export
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

#' calculate survival rate using survival package
#' @param survivalDuration          time gap between first diagnosis date and death date or last observation date (survivalTime)
#' @param outcomeCount              binary value (if outcome = death, death = 1 and alive = 0)
#' @param survivalDurationTime      n-year survival -> 365*n
#' @param valueWant                 surv = survival rate, uci = upper CI, lci = lower CI
#' @import survival
#' @export
survivalCal<-function(survivalDuration = survivalTime,
                      outcomeCount = outcomeCount,
                      survivalDurationTime = survivalDurationTime){
    
    survivalRate<-summary(survfit(Surv(survivalDuration, outcomeCount)~1), time = survivalDurationTime)$surv
    return(survivalRate)
}

survivalCalUCI<-function(survivalDuration = survivalTime,
                      outcomeCount = outcomeCount,
                      survivalDurationTime = survivalDurationTime){
    
    survivalRate<-summary(survfit(Surv(survivalDuration, outcomeCount)~1), time = survivalDurationTime)$upper
    return(survivalRate)
}

survivalCalLCI<-function(survivalDuration = survivalTime,
                      outcomeCount = outcomeCount,
                      survivalDurationTime = survivalDurationTime){
    
    survivalRate<-summary(survfit(Surv(survivalDuration, outcomeCount)~1), time = survivalDurationTime)$lower
    return(survivalRate)
}
#summary(survfit(Surv(s$time, s$out)~1), time = 365*1)$upper
# survivalCal(survivalDuration = s$time,
#             outcomeCount = s$out,
#             survivalDurationTime = 365*5,
#             valueWant = surv)
