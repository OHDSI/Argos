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

#'get cost data
#' @param connectionDetails
#' @param minCostDateUnit           minumal unit for cohort start date ('year' > 'quarter' > 'month' > 'week' > day')
#' @param cohortId           target Cohort Id, if this is -1, it will extract cost data for whole subject in the cohortTable
#'@export
#'
#'@import dplyr
extractVisitCost<-function(connectionDetails, 
                           cdmDatabaseSchema,
                           cohortDatabaseSchema,
                           vocabularyDatabaseSchema,
                           cohortTable,
                           cohortId=-1,
                           costWindowStart = -60,
                           costWindowEnd =365,
                           minCostDateUnit = 'quarter',
                           specifyCondition = FALSE,
                           conditionConceptIds=NULL){
    connection<-DatabaseConnector::connect(connectionDetails)
    
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "extractVisitCost.sql",
                                             packageName = "Argos",
                                             dbms = attr(connection,"dbms"),
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema=cdmDatabaseSchema,
                                             target_database_schema = cohortDatabaseSchema,
                                             vocabulary_database_schema=vocabularyDatabaseSchema,
                                             target_cohort_table = cohortTable,
                                             min_date_unit = minCostDateUnit,
                                             target_cohort_id = cohortId,
                                             cost_window_start = costWindowStart,
                                             cost_window_end = costWindowEnd,
                                             specify_condition = specifyCondition,
                                             condition_concept_ids=conditionConceptIds
    )
    # fileCon<-file(file.path(getwd(),"output.txt"))
    # writeLines(sql,fileCon)
    # close(fileCon)
    costData<-DatabaseConnector::querySql(connection, sql)
    colnames(costData)<-SqlRender::snakeCaseToCamelCase(colnames(costData))
    costData %>% dplyr::arrange(dateUnit,cohortDefinitionId,visitConceptId)
    
    costData$totalChargePerVisit <-costData$totalChargeSum/costData$visitCount
    costData$paidByPayerPerVisit <-costData$paidByPayerSum/costData$visitCount
    costData$paidByPatientPerVisit <-costData$paidByPatientSum/costData$visitCount
    
    costData$totalChargePerActivePatient <-costData$totalChargeSum/costData$subjectCount
    costData$paidByPayerPerActivePatient <-costData$paidByPayerSum/costData$subjectCount
    costData$paidByPatientPerActivePatient <-costData$paidByPatientSum/costData$subjectCount
    
    return(costData)
}
