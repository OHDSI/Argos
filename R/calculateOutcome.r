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

#'calculate outcome incidence data
#' @param incidenceData
#' @param basePopulation           
#' @param baseVar           'startYear' or 'birthYear'
#' @export
#'
getOutcomeData<-function(connectionDetails = connectionDetails, 
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = cohortTable,
                         covariateSettings = covariateSettings,
                         outcomeDatabaseSchema = cohortDatabaseSchema ,
                         cohortId = cancerList$cohortId[i],
                         outcomeId = outcomeId,
                         requireTimeAtRisk = TRUE,
                         riskWindowStart = 0,
                         riskWindowEnd = 365,
                         removeSubjectsWithPriorOutcome = TRUE,
                         minDateUnit = "year"){
    plpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails, 
                                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                                  cohortTable = cohortTable,
                                                  cohortId = cohortId,
                                                  covariateSettings = covariateSettings,
                                                  outcomeDatabaseSchema = cohortDatabaseSchema,
                                                  outcomeTable = cohortTable,
                                                  outcomeIds = outcomeId,
                                                  sampleSize = NULL)
    #PatientLevelPrediction::savePlpData(plpData,file.path(outputFolder,paste0("plpData",Sys.time())))
    population <- PatientLevelPrediction::createStudyPopulation(plpData = plpData,
                                                                outcomeId = outcomeId,
                                                                washoutPeriod = 0,
                                                                firstExposureOnly = TRUE,
                                                                removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
                                                                priorOutcomeLookback = 99999,
                                                                riskWindowStart = riskWindowStart,
                                                                riskWindowEnd = riskWindowEnd,
                                                                addExposureDaysToStart = FALSE,
                                                                addExposureDaysToEnd = FALSE,
                                                                minTimeAtRisk = max(riskWindowEnd - riskWindowStart -1,0),
                                                                requireTimeAtRisk = requireTimeAtRisk,
                                                                includeAllOutcomes = TRUE,
                                                                verbosity = "DEBUG")
    
    #count number for target cohort
    targetCohort<-calculateNumberPerCovTime(plpData = plpData,
                                            population = population,
                                            minDateUnit = minDateUnit)
    
    #count number for cohort only with outcome
    outcomeCohort<-calculateNumberPerCovTime(plpData = plpData,
                                             population = population[population$outcomeCount>=1,],
                                             minDateUnit = minDateUnit)
    return(list(targetCohort=targetCohort,
                outcomeCohort=outcomeCohort,
                cohortId = cohortId,
                minDateUnit = minDateUnit))
}