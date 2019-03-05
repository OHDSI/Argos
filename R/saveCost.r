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

#'saving tables and plot image which are the results cost analysis
#'@param outputFolder
#'@param PlottotalcostperYrdiv 
#'@param plotperYrBarplotPayer 
#'@param plotperYrBarplotPatient 
#'@param imageExtension plot file's extension
#'@import ggplot2
#'@export
savecost<-function(outputFolder,
                   PlottotalcostperYrdiv,
                   plotperYrBarplotPayer,
                   plotperYrBarplotPatient,
                   imageExtension = "png"){
    ifelse(!dir.exists(file.path(outputFolder, "cost")), dir.create(file.path(outputFolder, "cost")), print("saving in cost folder"))
    costFolder<-paste(outputFolder, "cost", sep = "/")
    
    # write.csv(costperMt, file = file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "TotalCostperMt", ".", "csv")))
    
    ggsave(file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "TotalCostperMt", ".", imageExtension)), plottotalCostperMt, height = 15, width = 30, units = "cm")
    
    # write.csv(costperYrTotalDiv, file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "TotalCostperYrDividedByVisit", ".", "csv")))
    # write.csv(costperYrTotal, file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "TotalCostperYrTotalAvg", ".", "csv")))
    # write.csv(costpayerperYr, file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "PaidByPayer", ".", "csv")))
    # write.csv(costpatientperYr, file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "PaidByPatient", ".", "csv")))
    
    ggsave(file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "TotalCostperYr", ".", imageExtension)), PlottotalcostperYrdiv, height = 10, width = 30, units = "cm")
    ggsave(file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "PayerCostperYr", ".", imageExtension)), plotperYrBarplotPayer, height = 10, width = 30, units = "cm")
    ggsave(file.path(costFolder, paste0(cancerList$cohortName[[i]], "Cancer", "PatientCostperYr", ".", imageExtension)), plotperYrBarplotPatient, height = 10, width = 30, units = "cm")
}

