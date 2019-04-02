# Copyright 2019 Observational Health Data Sciences and Informatics
#
# This file is part of Argos
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' get outcome  data
#' @param connectionDetails
#' @param cdmDatabaseSchema
#' @param cohortDatabaseSchema
#' @param outcomeDatabaseSchema
#' @param cohortTable
#' @param covariateSettings
#' @param targetCohortId                    cancer type cohort id
#' @param outcomeId                         outcome = death
#' @param requireTimeAtRisk                 must be FALSE (default)
#' @param riskWindowStart
#' @param riskWindowEnd                     if you want to see max 5-year survival rate then 365*5
#' @param removeSubjectsWithPriorOutcome      
#' @param minDateUnit                       minumal unit for cohort start date ('year' > 'quarter' > 'month' > 'day')
#' @export
#'
#targetCohortId<-cancerList$cohortId[5]
readySurvData<-function(connectionDetails , 
                        cdmDatabaseSchema ,
                        cohortDatabaseSchema ,
                        outcomeDatabaseSchema  ,
                        cohortTable,
                        covariateSettings,
                        targetCohortId ,
                        outcomeId,
                        requireTimeAtRisk = FALSE,
                        minTimeAtRisk = 1,
                        riskWindowStart = 0,
                        riskWindowEnd = 365*5,
                        removeSubjectsWithPriorOutcome = TRUE,
                        minDateUnit = "year"){
    plpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                                  cohortTable = cohortTable,
                                                  cohortId = targetCohortId,
                                                  covariateSettings = covariateSettings,
                                                  outcomeDatabaseSchema = cohortDatabaseSchema,
                                                  outcomeTable = cohortTable,
                                                  outcomeIds = outcomeId)
    #PatientLevelPrediction::savePlpData(plpData,file.path(outputFolder,paste0("plpData",Sys.time())))
    population <- PatientLevelPrediction::createStudyPopulation(plpData = plpData,
                                                                outcomeId = outcomeId,
                                                                binary = TRUE,
                                                                firstExposureOnly = TRUE,
                                                                washoutPeriod = 0,
                                                                removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
                                                                riskWindowStart = riskWindowStart,
                                                                riskWindowEnd = riskWindowEnd,
                                                                addExposureDaysToStart = FALSE,
                                                                addExposureDaysToEnd = FALSE,
                                                                requireTimeAtRisk = requireTimeAtRisk,
                                                                minTimeAtRisk = minTimeAtRisk,
                                                                includeAllOutcomes = TRUE,
                                                                verbosity = "DEBUG")
    
    readySurv <- getsurvData(plpData = plpData,
                             population = population)
    
    return(readySurv)
}


#' Calculating survival rate function
#' @param survivalData          outcome of Argos package readySurvData code
#' @param refPopulation
#' @param Agedivided            TRUE then total patients survival rate will calculated, FALSE then age specified survival rate will calculated
#' @param AgeSet                30-39, 40-49, 50-59, 60-69, 70-79, 80-99 will set as default
#' @param genderSet             8507 as male, 8532 as female
#' @param startYearSet          
#' @param birthYearSet
#' @import dplyr
#' @export

calculateSurvival <- function(survivalData = survivalData,
                              Agedivided = TRUE,
                              AgeSet = list(30:39,
                                            40:49,
                                            50:59,
                                            60:69,
                                            70:79,
                                            80:99),
                              genderSet = list(8507,8532),
                              startYearSet = startYearSet,
                              #birthYearSet = list(1960:1964, 1965:1969, 1970:1974, 1975:1979, 1980:1984, 1985:1989),
                              observationEndYear=2013){
    if (Agedivided){
        settings<-list(age=AgeSet, gender=genderSet, startYear=startYearSet)#, birthYear = birthYearSet)
        expanded.set<-expand.grid(settings)
        observeSurvDf <- data.frame()
        for (i in seq(nrow(expanded.set))){
            settings<-list(age=AgeSet, gender=genderSet, startYear=startYearSet)
            expanded.set<-expand.grid(settings)
            df<-survivalData %>%
                left_join(expSurv, by = c("genderConceptId" = "genderConceptId",
                                          "age" = "startAge",
                                          "startYear" = "startYear")) %>%
                filter(age %in% unlist(expanded.set[i,]$age)) %>%
                filter(genderConceptId %in% unlist(expanded.set[i,]$gender) ) %>%
                filter(startYear %in% unlist(expanded.set[i,]$startYear))
            if(nrow(df)==0) next
            surv<-data.frame(startYear = min(unlist(expanded.set[i,]$startYear)),
                             age = min(unlist(expanded.set[i,]$age)),
                             genderConceptId = unlist(expanded.set[i,]$gender),
                             survival1Yr = ifelse( min(unlist(expanded.set[i,]$startYear)) <= observationEndYear-1,
                                                   ifelse( !is.null(survivalCal(survivalDuration = df$survivalTime,
                                                                                outcomeCount = df$outcomeCount,
                                                                                survivalDurationTime = 365*1)),
                                                           survivalCal(survivalDuration = df$survivalTime,
                                                                       outcomeCount = df$outcomeCount,
                                                                       survivalDurationTime = 365*1),
                                                           NA),
                                                   NA),
                             survival3Yr = ifelse( min(unlist(expanded.set[i,]$startYear)) <= observationEndYear-3,
                                                   ifelse( !is.null(survivalCal(survivalDuration = df$survivalTime,
                                                                                outcomeCount = df$outcomeCount,
                                                                                survivalDurationTime = 365*3)),
                                                           survivalCal(survivalDuration = df$survivalTime,
                                                                       outcomeCount = df$outcomeCount,
                                                                       survivalDurationTime = 365*3),
                                                           NA),
                                                   NA),
                             survival5Yr = ifelse(min(unlist(expanded.set[i,]$startYear)) <= observationEndYear-5,
                                                  ifelse( !is.null(survivalCal(survivalDuration = df$survivalTime,
                                                                               outcomeCount = df$outcomeCount,
                                                                               survivalDurationTime = 365*5)),
                                                          survivalCal(survivalDuration = df$survivalTime,
                                                                      outcomeCount = df$outcomeCount,
                                                                      survivalDurationTime = 365*5),
                                                          NA),
                                                  NA)
            ) %>%
                mutate(relativesurvival5Yr = surviva5Yr/mean(df$expectedSurvivalRate))
                
            observeSurvDf<-rbind(observeSurvDf, surv)
        }
        return(observeSurvDf)
    }else{
        settings<-list(gender=genderSet, startYear=startYearSet)
        expanded.set<-expand.grid(settings)
        observeSurvDf <- data.frame()
        
        for (i in seq(nrow(expanded.set))){
            df<-survivalData %>%
                left_join(expSurv, by = c("genderConceptId" = "genderConceptId",
                                          #"age" = "startAge",
                                          "startYear" = "startYear")) %>%
                filter(genderConceptId %in% unlist(expanded.set[i,]$gender) ) %>%
                filter(startYear %in% unlist(expanded.set[i,]$startYear))
            if(nrow(df)==0) next
            
            surv<-data.frame(startYear = min(unlist(expanded.set[i,]$startYear)),
                             genderConceptId = unlist(expanded.set[i,]$gender),
                             survival1Yr = ifelse( min(unlist(expanded.set[i,]$startYear)) <= observationEndYear-1,
                                                   ifelse( !is.null(survivalCal(survivalDuration = df$survivalTime,
                                                                                outcomeCount = df$outcomeCount,
                                                                                survivalDurationTime = 365*1)),
                                                           survivalCal(survivalDuration = df$survivalTime,
                                                                       outcomeCount = df$outcomeCount,
                                                                       survivalDurationTime = 365*1),
                                                           NA),
                                                   NA),
                             survival3Yr = ifelse( min(unlist(expanded.set[i,]$startYear)) <= observationEndYear-3,
                                                   ifelse( !is.null(survivalCal(survivalDuration = df$survivalTime,
                                                                                outcomeCount = df$outcomeCount,
                                                                                survivalDurationTime = 365*3)),
                                                           survivalCal(survivalDuration = df$survivalTime,
                                                                       outcomeCount = df$outcomeCount,
                                                                       survivalDurationTime = 365*3),
                                                           NA),
                                                   NA),
                             survival5Yr = ifelse(min(unlist(expanded.set[i,]$startYear)) <= observationEndYear-5,
                                                  ifelse( !is.null(survivalCal(survivalDuration = df$survivalTime,
                                                                               outcomeCount = df$outcomeCount,
                                                                               survivalDurationTime = 365*5)),
                                                          survivalCal(survivalDuration = df$survivalTime,
                                                                      outcomeCount = df$outcomeCount,
                                                                      survivalDurationTime = 365*5),
                                                          NA),
                                                  NA),
                             survival5YrUci = ifelse(min(unlist(expanded.set[i,]$startYear)) <= observationEndYear-5,
                                                  ifelse( !is.null(survivalCal(survivalDuration = df$survivalTime,
                                                                               outcomeCount = df$outcomeCount,
                                                                               survivalDurationTime = 365*5)),
                                                          survivalCalUCI(survivalDuration = df$survivalTime,
                                                                      outcomeCount = df$outcomeCount,
                                                                      survivalDurationTime = 365*5),
                                                          NA),
                                                  NA),
                             survival5YrLci = ifelse(min(unlist(expanded.set[i,]$startYear)) <= observationEndYear-5,
                                                     ifelse( !is.null(survivalCal(survivalDuration = df$survivalTime,
                                                                                  outcomeCount = df$outcomeCount,
                                                                                  survivalDurationTime = 365*5)),
                                                             survivalCalLCI(survivalDuration = df$survivalTime,
                                                                         outcomeCount = df$outcomeCount,
                                                                         survivalDurationTime = 365*5),
                                                             NA),
                                                     NA)
            ) %>%
                mutate(relativesurvival5Yr = survival5Yr/mean(df$expectedSurvivalRate))
            
            observeSurvDf<-rbind(observeSurvDf, surv) %>%
                arrange(genderConceptId, startYear)
        }
        return(observeSurvDf)
    }
    
}
