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


#'get plot for cost 
#'@param costData
#'@param outputFolder  
#' 
#'@export
#'
#'@import dplyr
#'@import ggplot2
#'@import cowplot


plotforCostPerMt<- function(costData,
                            title,
                            outputFolder,
                            fileName,
                            imageExtension = "png"){
    costperMt<- costData %>%
                group_by(cohortStartYear, dateUnit) %>%
                summarise(avgCostSumperMt = sum(totalChargeSum)/sum(subjectCount),
                          avgCostPatientperMt = sum(paidByPatientSum)/sum(subjectCount),
                          avgCostPayerperMt = sum(paidByPayerSum)/sum(subjectCount))
    
    plottotalCostperMt<- ggplot2::ggplot(data = costperMt, ggplot2::aes(x = as.factor(dateUnit), y = avgCostSumperMt, group = cohortStartYear, colour = as.factor(cohortStartYear)))+
                         ggplot2::geom_line()+ 
                         ggplot2::xlab("0 = diagnosis month") + 
                         ggplot2::ylab("Total Cost per Month (won)") + 
                         ggplot2::ggtitle(paste(cancerList$cohortName[[i]], "Cancer", "Total Cost per month 1mt before and 10mt after diagnosis", sep = " ")) +
                         ggplot2::theme_bw()+
                         ggplot2::theme(legend.title = ggplot2::element_blank())
    ggplot2::ggsave(file.path(outputFolder, paste0(cancerList$cohortName[[i]], "Cancer", "TotalCostperMt", ".", "imageExtension")), plottotalCostperMt, height = 30, width = 120, units = "cm")
    
    plotPatientCostperMt<- ggplot2::ggplot(data = costperMt, ggplot2::aes(x = as.factor(dateUnit), y = avgCostPatientperMt, group = cohortStartYear, colour = as.factor(cohortStartYear)))+
                           ggplot2::geom_line()+
                           ggplot2::xlab("0 = diagnosis month (won)")+
                           ggplot2::ylab("Cost Paid by Patient per Month")+
                           ggplot2::ggtitle(paste(cancerList$cohortName[[i]], "Cancer", "Cost by patient per month 1mt before and 10mt after diagnosis", sep = " ")) +
                           ggplot2::theme_bw()+
                           ggplot2::theme(legend.title = ggplot2::element_blank())
    ggplot2::ggsave(file.path(outputFolder, paste0(cancerList$cohortName[[i]], "Cancer", "PatientCostperMt", ".", "imageExtension")), plotPatientCostperMt, height = 30, width = 120, units = "cm")
    
    plotpayerCostperMt<- ggplot2::ggplot(data = costperMt, ggplot2::aes(x = as.factor(dateUnit), y = avgCostPayerperMt, group = cohortStartYear, colour = as.factor(cohortStartYear)))+
                         ggplot2::geom_line()+
                         ggplot2::xlab("0 = diagnosis month (won)")+
                         ggplot2::ylab("Cost Paid by Payer per Month")+
                         ggplot2::ggtitle(paste(cancerList$cohortName[[i]], "Cancer", "Cost by payer per month 1mt before and 10mt after diagnosis", sep = " ")) +
                         ggplot2::theme_bw()+
                         ggplot2::theme(legend.title = ggplot2::element_blank())
    ggplot2::ggsave(file.path(outputFolder, paste0(cancerList$cohortName[[i]], "Cancer", "PayerCostperMt", ".", "imageExtension")), plotpayerCostperMt, height = 30, width = 120, units = "cm")
    
}

plotforCostPerYr<- function(costData,
                            title,
                            outputFolder,
                            fileName,
                            imageExtension = "png"){
    costperYr<- costData %>%
                mutate( visitConceptId = factor(visitConceptId, levels = c(9201, 9202, 9203), labels = c("inpatient", "outpatient", "emergency room"))) %>%
                group_by(cohortStartYear, visitConceptId) %>%
                summarise(avgCostSumperMt = sum(totalChargeSum)/sum(subjectCount)) 
    
    plotperYr<- ggplot2::ggplot(data = costperYr, ggplot2::aes(x = as.factor(cohortStartYear), y = avgCostSumperMt, group = visitConceptId, colour = as.factor(visitConceptId)))+
                ggplot2::geom_point()+
                ggplot2::geom_line()+
                ggplot2::xlab("visit year")+
                ggplot2::ylab("Total Cost per Year(won)")+
                ggplot2::ggtitle(paste(cancerList$cohortName[[i]], "Cancer Cost per Year", sep = " "))+
                ggplot2::theme_bw()+
                ggplot2::theme(legend.title = ggplot2::element_blank())
    ggplot2::ggsave(file.path(outputFolder, paste0(cancerList$cohortName[[i]], "Cancer", "TotalCostperYr", ".", "imageExtension")), plotperYr, height = 30, width = 100, units = "cm")
    
}