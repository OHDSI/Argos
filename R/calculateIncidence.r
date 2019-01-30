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

#'get incidence data
#' @param connectionDetails
#' @param minDateUnit           minumal unit for cohort start date ('year' > 'quarter' > 'month' > 'day')
#'
#'@export
#'
#'@import dplyr
getIncidenceData<-function(connectionDetails, 
                           cdmDatabaseSchema,
                           cohortDatabaseSchema,
                           cohortTable,
                           covariateSettings,
                           outcomeDatabaseSchema,
                           cohortId,
                           minDateUnit = 'year'){
    plpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails, 
                                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                                  cohortTable = cohortTable,
                                                  cohortId = cohortId,
                                                  covariateSettings = covariateSettings,
                                                  outcomeDatabaseSchema = cohortDatabaseSchema,
                                                  outcomeTable = cohortTable,
                                                  outcomeIds = cohortId,
                                                  sampleSize = NULL)
    #PatientLevelPrediction::savePlpData(plpData,file.path(outputFolder,paste0("plpData",Sys.time())))
    
    # population <- PatientLevelPrediction::createStudyPopulation(plpData = plpData, 
    #                                                             outcomeId = outcomeId,
    #                                                             washoutPeriod = 0,
    #                                                             firstExposureOnly = TRUE,
    #                                                             removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
    #                                                             priorOutcomeLookback = 99999,
    #                                                             riskWindowStart = 0,
    #                                                             riskWindowEnd = 0,
    #                                                             addExposureDaysToStart = FALSE,
    #                                                             addExposureDaysToEnd = FALSE,
    #                                                             minTimeAtRisk = survivalTime[j]-1,
    #                                                             requireTimeAtRisk = TRUE,
    #                                                             includeAllOutcomes = TRUE,
    #                                                             verbosity = "DEBUG")
    
    # covariateData <- FeatureExtraction::getDbCovariateData(connectionDetails = connectionDetails,
    #                                                        cdmDatabaseSchema = cdmDatabaseSchema,
    #                                                        cohortDatabaseSchema = cohortDatabaseSchema,
    #                                                        cohortTable = cohortTable,
    #                                                        cohortId = cohortId,
    #                                                        rowIdField = "subject_id",
    #                                                        covariateSettings = covariateSettings)

    cohort<-ff::as.ram(plpData$cohort)
    covRef<-ff::as.ram(plpData$covariateRef)
    covariates<-ff::as.ram(plpData$covariates)
    
    if(minDateUnit=='day'){
        cohortStartDateRef <- data.frame(covariateId = c(-1,-2,-3,-4),
                                         covariateName = c("cohortStartYear","cohortStartQuarter","cohortStartMonth","cohortStartDay"),
                                         analysisId = -1,
                                         conceptId = 0)
        covariates<-rbind(covariates,
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -1,
                                     covariateValue = lubridate::year(cohort$cohortStartDate)),
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -2,
                                     covariateValue = lubridate::quarter(cohort$cohortStartDate)),
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -3,
                                     covariateValue = lubridate::month(cohort$cohortStartDate)),
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -4,
                                     covariateValue = lubridate::day(cohort$cohortStartDate))
        )
    }
    
    if(minDateUnit=='month'){
        cohortStartDateRef <- data.frame(covariateId = c(-1,-2,-3),
                                         covariateName = c("cohortStartYear","cohortStartQuarter","cohortStartMonth"),
                                         analysisId = -1,
                                         conceptId = 0)
        covariates<-rbind(covariates,
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -1,
                                     covariateValue = lubridate::year(cohort$cohortStartDate)),
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -2,
                                     covariateValue = lubridate::quarter(cohort$cohortStartDate)),
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -3,
                                     covariateValue = lubridate::month(cohort$cohortStartDate))
        )
    }
    
    if(minDateUnit=='quarter'){
        cohortStartDateRef <- data.frame(covariateId = c(-1,-2),
                                         covariateName = c("cohortStartYear","cohortStartQuarter"),
                                         analysisId = -1,
                                         conceptId = 0)
        covariates<-rbind(covariates,
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -1,
                                     covariateValue = lubridate::year(cohort$cohortStartDate)),
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -2,
                                     covariateValue = lubridate::quarter(cohort$cohortStartDate))
        )
    }
    
    if(minDateUnit=='year'){
        cohortStartDateRef <- data.frame(covariateId = c(-1),
                                         covariateName = c("cohortStartYear"),
                                         analysisId = -1,
                                         conceptId = 0)
        covariates<-rbind(covariates,
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -1,
                                     covariateValue = lubridate::year(cohort$cohortStartDate))
        )
    }
    covRef<-rbind(covRef,cohortStartDateRef)
    covariates$newCovId<-as.numeric(as.factor(covariates$covariateId))
    
    #aggregate accroding to the covariates
    cov.df<-as.data.frame(as.matrix(Matrix::sparseMatrix(i=covariates$rowId,
                                                         j=covariates$newCovId,
                                                         x=covariates$covariateValue)))
    cov.df$n = 1
    
    resultData<-aggregate(n~.-n,cov.df,sum,na.rm=TRUE)
    
    #naming the columns
    colnames(resultData)<-c(as.character(covRef$covariateName[match(levels(as.factor(covariates$covariateId)),as.character(covRef$covariateId))]),
                            "aggregatedNum")
    colnames(resultData)<-gsub("age in years","age", colnames(resultData))
    
    if("gender = MALE"%in%colnames(resultData) & "gender = FEMALE"%in%colnames(resultData)){
        resultData$genderConceptId<-resultData[,grepl("gender = MALE",colnames(resultData))]*8507+resultData[,grepl("gender = FEMALE",colnames(resultData))]*8532
        #remove number of population with both genders
        resultData<-resultData[resultData$genderConceptId%in%c(8507,8532),]
        resultData<-resultData[,!grepl("gender =",colnames(resultData))]
    }
    
    resultData <- list(data=resultData,
                       cohortId = cohortId,
                       minDateUnit = minDateUnit)
    
    return(resultData)
}

#'calculate incidence data
#' @param incidenceData
#' @param basePopulation           
#' @param baseVar           'startYear' or 'birthYear'
#'@export
calculateIncidence<-function(incidenceData = incidenceData,
                             basePopulation = basePop,
                             standardization = "direct",
                             refPopulation = NULL,
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
    incD<-incidenceData$data
    incPop<-dplyr::inner_join(incD,basePop,by = c("cohortStartYear"="startYear", "age" = "startAge", "genderConceptId"="genderConceptId") )
    
    settings<-list(age=AgeSet, gender=genderSet, startYear=startYearSet, birthYear = birthYearSet)
    expanded.set<-expand.grid(settings)
    
    resultDf <- data.frame()
    ##for loop should be replaced by apply function
    
    if(standardization=="direct"){
        refPopulation$stdWt<-refPopulation$standardPopulation/sum(refPopulation$standardPopulation)
        
        for (i in seq(nrow(expanded.set))){
            df<-incPop %>% 
                filter(age %in% unlist(expanded.set[i,]$age))  %>%
                filter(genderConceptId %in% unlist(expanded.set[i,]$gender) ) %>%
                filter(cohortStartYear %in% unlist(expanded.set[i,]$startYear) ) 
            
            df<-dplyr::inner_join(df,refPopulation,by=c("age"="startAge", "endAge"="endAge","genderConceptId"="genderConceptId"))
            
            tempDf<-data.frame(startYear = min(unlist(expanded.set[i,]$startYear)),
                               age = min(unlist(expanded.set[i,]$age)),
                               genderConceptId = unlist(expanded.set[i,]$gender),
                               targetPopNum = sum(df$aggregatedNum, na.rm = TRUE),
                               basePop = sum(df$population, na.rum = TRUE),
                               refPopulation = sum(df$standardPopulation, na.rum = TRUE),
                               proportion = sum(df$aggregatedNum, na.rm = TRUE) / sum(df$population, na.rum = TRUE),
                               standProp = sum(df$stdWt* (df$aggregatedNum/df$population))
                               )
            resultDf<-rbind(resultDf,tempDf)
        }
    }else{
        for (i in seq(nrow(expanded.set))){
            df<-incPop %>% 
                filter(age %in% unlist(expanded.set[i,]$age))  %>%
                filter(genderConceptId %in% unlist(expanded.set[i,]$gender) ) %>%
                filter(cohortStartYear %in% unlist(expanded.set[i,]$startYear) )
            
            tempDf<-data.frame(startYear = min(unlist(expanded.set[i,]$startYear)),
                               age = min(unlist(expanded.set[i,]$age)),
                               genderConceptId = unlist(expanded.set[i,]$gender),
                               targetPopNum = sum(df$aggregatedNum, na.rm = TRUE),
                               basePop = sum(df$population, na.rum = TRUE),
                               proportion = sum(df$aggregatedNum, na.rm = TRUE) / sum(df$population, na.rum = TRUE))
            resultDf<-rbind(resultDf,tempDf)
        }
    }
    
    return(resultDf)
}
