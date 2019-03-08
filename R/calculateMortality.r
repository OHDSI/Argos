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

#'get mortality rate 
#'@param connectionDetails
#'@param cdmDatabaseSchema          
#'@param cohortDatabaseSchema
#'@param outcomeDatabaseSchema
#'@param covariateSettings
#'@param cohortTable
#'@param targetCohortId
#'@param outcomeId                  for mortality rate, death_id = 99
#'@param minDateUnit                minumal unit for cohort start date ('year' > 'quarter' > 'month' > 'day')
#'@export
#'

getMortalityData<-function(connectionDetails,
                           cdmDatabaseSchema,
                           cohortDatabaseSchema,
                           outcomeDatabaseSchema,
                           cohortTable,
                           targetCohortId,
                           outcomeId = 99,
                           minDateUnit = 'year'){
    #get death data
    deathPlpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails, 
                                                       cdmDatabaseSchema = cdmDatabaseSchema,
                                                       cohortDatabaseSchema = cohortDatabaseSchema,
                                                       cohortTable = cohortTable,
                                                       cohortId = outcomeId,
                                                       covariateSettings = covariateSettings,
                                                       outcomeDatabaseSchema = cohortDatabaseSchema,
                                                       outcomeTable = cohortTable,
                                                       outcomeIds = outcomeId,
                                                       sampleSize = NULL)
    #get cancer patient data
    incidencePlpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails, 
                                                           cdmDatabaseSchema = cdmDatabaseSchema,
                                                           cohortDatabaseSchema = cohortDatabaseSchema,
                                                           cohortTable = cohortTable,
                                                           cohortId = targetCohortId,
                                                           covariateSettings = covariateSettings,
                                                           outcomeDatabaseSchema = cohortDatabaseSchema,
                                                           outcomeTable = cohortTable,
                                                           outcomeIds = targetCohortId,
                                                           sampleSize = NULL)
    
    mortalitycohorts<- ff::as.ram(deathPlpData$cohorts) %>%
        filter(subjectId %in% incidencePlpData$cohorts$subjectId)
    mortalitycovariates<-ff::as.ram(deathPlpData$covariates) %>% 
        inner_join(mortalitycohorts, by = "rowId") %>%
        select(rowId, covariateId, covariateValue)
    mortalitycovariateRef<- ff::as.ram(deathPlpData$covariateRef)
    #ready to use calculateNumberPerCovTime
    df<-list(cohorts = mortalitycohorts,
             covariates = mortalitycovariates,
             covariateRef = mortalitycovariateRef)
    
    mortalityResultData<-calculateNumberPerCovTime(plpData = df,
                                                   population = NULL,
                                                   minDateUnit = minDateUnit)
    
    result<-list(data=mortalityResultData,
                 targetCohortId = targetCohortId,
                 minDateUnit = minDateUnit)
    class(result)<-"incidenceData"
    
    return(result)
}


i<-3
incidencePlpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails, 
                                                       cdmDatabaseSchema = cdmDatabaseSchema,
                                                       cohortDatabaseSchema = cohortDatabaseSchema,
                                                       cohortTable = cohortTable,
                                                       cohortId = cancerList$cohortId[i],
                                                       covariateSettings = covariateSettings,
                                                       outcomeDatabaseSchema = cohortDatabaseSchema,
                                                       outcomeTable = cohortTable,
                                                       outcomeIds = cancerList$cohortId[i],
                                                       sampleSize = NULL)
mortalitycohorts<- ff::as.ram(deathPlpData$cohorts) %>%
    filter(subjectId %in% incidencePlpData$cohorts$subjectId)
hist(lubridate::year(mortalitycohorts$cohortStartDate))
t<-getMortalityData(connectionDetails,
                 cdmDatabaseSchema,
                 cohortDatabaseSchema,
                 outcomeDatabaseSchema,
                 cohortTable,
                 targetCohortId = cancerList$cohortId[i],
                 outcomeId = 99,
                 minDateUnit = 'year')
death<-getIncidenceData(connectionDetails, 
                  cdmDatabaseSchema,
                  cohortDatabaseSchema,
                  outcomeDatabaseSchema,
                  cohortTable,
                  targetCohortId = 99,
                  minDateUnit = 'year')
calculatedeath<-calculateIncidence(incidenceData = death,
                             basePopulation = basePop,
                             standardization = "direct",
                             refPopulation = refPop,
                             Agestandardization = TRUE,
                             genderStandardization = TRUE,
                             startYearStandardization = TRUE,
                             AgeSet = list(30:39,40:49,50:59,60:69,70:79,80:99),
                             genderSet = list(8507,8532),
                             startYearSet = list(2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013),
                             birthYearSet = list(1960:1964, 1965:1969, 1970:1974, 1975:1979, 1980:1984, 1985:1989))
out<-ageadjust(incidencePropdata = calculateMortality,
                    alpha = 0.05)

PlotByDiagnosisIncAgeAd(calculatedeath)
