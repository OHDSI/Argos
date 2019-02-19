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
#'@import ggplot2

savecost<-function(outputFolder,
                      imageExtension = "png"){
    dir.create(file.path(outputFolder, "cost"))
    costFolder<-paste(outputFolder, "cost", sep = "/")
    
    saveRDS(costData,file.path(costFolder,paste0("costData_cohortId_",cancerList$cohortId[[i]],"costWindowEnd_","1825",".rds" )))
    write.csv(costData,file.path(costFolder,paste0("costData_cohortId_",cancerList$cohortId[[i]],"costWindowEnd_","1825",".csv" )))
    write.csv(costperMt, file = file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "TotalCostperMt", ".", "csv")))
    
    ggsave(file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "TotalCostperMt", ".", "png")), plottotalCostperMt, height = 15, width = 30, units = "cm")
    
    write.csv(costperYrTotalDiv, file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "TotalCostperYrDividedByVisit", ".", "csv")))
    write.csv(costperYrTotal, file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "TotalCostperYrTotalAvg", ".", "csv")))
    write.csv(costpayerperYr, file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "PaidByPayer", ".", "csv")))
    write.csv(costpatientperYr, file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "PaidByPatient", ".", "csv")))
    
    ggsave(file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "TotalCostperYr", ".", "png")), PlottotalcostperYrdiv, height = 10, width = 30, units = "cm")
    ggsave(file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "PayerCostperYr", ".", "png")), plotperYr_barplot_payer, height = 10, width = 30, units = "cm")
    ggsave(file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "PatientCostperYr", ".", "png")), plotperYr_barplot_patient, height = 10, width = 30, units = "cm")
    
 }