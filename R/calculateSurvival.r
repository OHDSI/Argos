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
#' @param
#' @param
#' @param
#' @param
#' @param incidenceData
#' @param basePopulation           
#' @param baseVar           'startYear' or 'birthYear'
#' @export
#'
survivalData <- getSurvData(connectionDetails = connectionDetails, 
                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                     cohortDatabaseSchema = cohortDatabaseSchema,
                                     cohortTable = cohortTable,
                                     covariateSettings = covariateSettings,
                                     outcomeDatabaseSchema = cohortDatabaseSchema ,
                                     targetCohortId = cancerList$cohortId[[i]],
                                     outcomeId = outcomeId,
                                     requireTimeAtRisk = TRUE,
                                     riskWindowStart = 0,
                                     riskWindowEnd = 365*11,
                                     removeSubjectsWithPriorOutcome = TRUE,
                                     minDateUnit = "year")

getSurvData<-function(connectionDetails = connectionDetails, 
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         outcomeDatabaseSchema = cohortDatabaseSchema ,
                         cohortTable = cohortTable,
                         covariateSettings = covariateSettings,
                         targetCohortId,
                         outcomeId,
                         requireTimeAtRisk = TRUE,
                         riskWindowStart = 0,
                         riskWindowEnd = 0,
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
                                                                requireTimeAtRisk = TRUE,
                                                                includeAllOutcomes = TRUE,
                                                                verbosity = "DEBUG")

    readySurv <- getsurvData(plpData = plpData,
                             population = population)
    return(readySurv)
}


#' Calculating outcome function
#' @import survival
#' @import dplyr
#' @export
#' @export
#' @export
#' 
#' 
calculateSurvival <- function(survivalData = survivalData,
                              refPopulation = refPop,
                              standardization = "direct",
                              Agestandardization = TRUE,
                              genderStandardization = TRUE,
                              startYearStandardization = TRUE,
                              AgeSet = list(30:39,
                                            40:49,
                                            50:59,
                                            60:69,
                                            70:79,
                                            80:99),
                              genderSet = list(8507,8532),
                              startYearSet = list(2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012),
                              birthYearSet = list(1960:1964, 1965:1969, 1970:1974, 1975:1979, 1980:1984, 1985:1989)){

    settings<-list(age=AgeSet, gender=genderSet, startYear=startYearSet, birthYear = birthYearSet)
    expanded.set<-expand.grid(settings)
    
    resultDf <- data.frame()
    ##for loop should be replaced by apply function
    
    if(standardization=="direct"){
        refPopulation$stdWt<-refPopulation$standardPopulation/sum(refPopulation$standardPopulation)
        
        for (i in seq(nrow(expanded.set))){
            df<-survivalData %>%
                filter(age %in% unlist(expanded.set[i,]$age)) %>%
                filter(genderConceptId %in% unlist(expanded.set[i,]$gender) ) %>%
                filter(startYear %in% unlist(expanded.set[i,]$startYear)) %>%
                filter(birthYear %in% unlist(expanded.set[i,]$birthYear)) 
            if(nrow(df)==0) next
            
            surv<-as.data.frame(startYear = min(unlist(expanded.set[i,]$startYear)),
                                age = min(unlist(expanded.set[i,]$age)),
                                birthYear = min(unlist(expanded.set[i,]$birthYear)),
                                genderConceptId = unlist(expanded.set[i,]$gender),
                                survival1Yr = ifelse(df$startYear<=2012-1, 
                                                     survivalCal(data = df,
                                                                 survivalDuration = survivalTime,
                                                                 outcomeCount = outcomeCount,
                                                                 targetSurvivalTime = 365*1),
                                                     NA),
                                survival3Yr = ifelse(df$startYear<=2012-3, 
                                                     survivalCal(survivalDuration = df$survivalTime,
                                                                 outcomeCount = df$outcomeCount,
                                                                 targetSurvivalTime = 365*3),
                                                     NA),
                                survival5Yr = ifelse(df$startYear<=2012-5, 
                                                     survivalCal(survivalDuration = df$survivalTime,
                                                                 outcomeCount = df$outcomeCount,
                                                                 targetSurvivalTime = 365*5),
                                                     NA))
            surv<-df %>%
                group_by(startYear)%>%
                summarise(survival1Yr = ifelse(startYear<=2012-1,
                                               survivalCal(survivalDuration = survivalTime,
                                                           outcomeCount = outcomeCount,
                                                           targetSurvivalTime = 365*1),
                                               NA),
                          survival3Yr = ifelse(startYear<=2012-3,
                                               summary(survfit(Surv(survivalTime, outcomeCount)~1), time = 365*3)$surv,
                                               NA),
                          survival5Yr = ifelse(startYear<=2012-5, 
                                               summary(survfit(Surv(survivalTime, outcomeCount)~1), time = 365*5)$surv, 
                                               NA)) %>%
                mutate(age = min(unlist(expanded.set[i,]$age)),
                       birthYear = min(unlist(expanded.set[i,]$birthYear)),
                       genderConceptId = unlist(expanded.set[i,]$gender))
            
            # if(df$survivalrate5Yr ==0 | is.null(df$survivalrate5Yr) | is.na(df$survivalrate5Yr)) next
            # 
            # 
            # df<-dplyr::inner_join()
    #         
    #         df<-dplyr::inner_join(df,refPopulation,by=c("age"="startAge", "genderConceptId"="genderConceptId"))
    #         tempDf<-data.frame(startYear = min(unlist(expanded.set[i,]$startYear)),
    #                            age = min(unlist(expanded.set[i,]$age)),
    #                            birthYear = min(unlist(expanded.set[i,]$birthYear)),
    #                            genderConceptId = unlist(expanded.set[i,]$gender),
    #                            targetPopNum = sum(df$targetNum, na.rm = TRUE),
    #                            outcomePopNum = sum(df$outcomeNum, na.rm = TRUE),
    #                            refPopulation = sum(df$standardPopulation, na.rm = TRUE),
    #                            outcomeProportion = sum(df$outcomeNum, na.rm = TRUE)/sum(df$targetNum, na.rm = TRUE),
    #                            standProp = sum(df$stdWt* (df$outcomeNum/df$targetNum))
    #         )
    #         resultDf<-rbind(resultDf,tempDf)
    #     }
    # }else{
    #     for (i in seq(nrow(expanded.set))){
    #         df<-pop %>% 
    #             filter(age %in% unlist(expanded.set[i,]$age))  %>%
    #             filter(genderConceptId %in% unlist(expanded.set[i,]$gender) ) %>%
    #             filter(cohortStartYear %in% unlist(expanded.set[i,]$startYear) ) %>%
    #             filter(birthYear %in% unlist(expanded.set[i,]$birthYear) )
    #         if(nrow(df)==0) next
    #         
    #         tempDf<-data.frame(startYear = min(unlist(expanded.set[i,]$startYear)),
    #                            age = min(unlist(expanded.set[i,]$age)),
    #                            birthYear = min(unlist(expanded.set[i,]$birthYear)),
    #                            genderConceptId = unlist(expanded.set[i,]$gender),
    #                            targetPopNum = sum(df$targetNum, na.rm = TRUE),
    #                            outcomePopNum = sum(df$outcomeNum, na.rm = TRUE),
    #                            basePop = sum(df$population, na.rm = TRUE),
    #                            outcomeProportion = survival)
    #         resultDf<-rbind(resultDf,tempDf)
    #     }
    
    # resultDf<-unique(resultDf)
    # return(resultDf)
        }
    }
}

sum(df$outcomeNum, na.rm = TRUE)/sum(df$targetNum, na.rm = TRUE)