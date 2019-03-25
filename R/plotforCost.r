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

#'monthly cost from 2months before diagnosis to 12months after diagnosis
#'@param costData   outcome of Argos package extractVisitCost code if minCostDateUnit = "month" 
#'@import dplyr
#'@import ggplot2
#'@export
plotforCostPerMt<- function(costData){
    costperMt<- costData %>%
        filter( dateUnit<=12 & dateUnit>=-2) %>%
        group_by(cohortStartYear, dateUnit) %>%
        summarise( avgCostPatientperMt = (sum(paidByPatientSum)/sum(subjectCount))*0.001,
                   avgCostPayerperMt = (sum(paidByPayerSum)/sum(subjectCount))*0.001,
                   avgCostTotalperMt = ((sum(paidByPatientSum)+sum(paidByPayerSum))/sum(subjectCount))*0.001)
    
    plottotalCostperMt<- ggplot2::ggplot(data = costperMt, ggplot2::aes(x = as.factor(dateUnit), y = avgCostTotalperMt, group = cohortStartYear, colour = as.factor(cohortStartYear)))+
        ggplot2::geom_line()+ 
        ggplot2::xlab("0 = diagnosis month") + 
        ggplot2::ylab("Cost Paid by Patient per Month (1000 won)") + 
        ggplot2::ggtitle(paste("Monthly Whole Medical Cost of",cancerList$cohortName[[i]], "Cancer", "from 2 months before diagnosis to a year after diagnosis", sep = " ")) +
        ggplot2::theme_bw()+
        ggplot2::scale_color_discrete(name = "diagnosis year")+
        ggplot2::theme(legend.title = element_blank(),
                       legend.text = element_text(size = 15),
                       plot.title = element_text(size = 17),
                       axis.text.x = element_text(size = 12),
                       axis.title.x = element_text(size = 15),
                       axis.text.y = element_text(size = 12),
                       axis.title.y = element_text(size = 15))
    
    return(plottotalCostperMt)
}

#'the total cost of the diagnosed year 
#'@param costData   outcome of Argos package extractVisitCost code if minCostDateUnit = "year" 
#'@import dplyr
#'@import ggplot2
#'@export
plotforCostPerYrdiv<- function(costData){
    costperYrTotalDiv<- costData %>%
        filter( dateUnit == 0 ) %>%
        mutate( visitConceptId = factor(visitConceptId, levels = c(9201, 9202, 9203), labels = c("inpatient", "outpatient", "emergency room"))) %>%
        group_by(cohortStartYear, visitConceptId) %>%
        summarise(avgCostSumperYr = ((sum(paidByPayerSum)+sum(paidByPatientSum))/sum(subjectCount))*0.001) 
    
    # costperYrTotal<- costData %>%
    #     filter( dateUnit == 0 ) %>%
    #     mutate( visitConceptId = factor(visitConceptId, levels = c(9201, 9202, 9203), labels = c("inpatient", "outpatient", "emergency room"))) %>%
    #     group_by(cohortStartYear) %>%
    #     summarise(avgCostSumperYr = ((sum(paidByPayerSum)+sum(paidByPatientSum))/sum(subjectCount))*0.001) %>%
    #     mutate( visitConceptId = "totalAverage") 
    
    PlottotalcostperYrdiv<-ggplot2::ggplot(data = costperYrTotalDiv, aes(x = as.factor(cohortStartYear), y = avgCostSumperYr, fill = visitConceptId))+
        ggplot2::geom_bar(stat = "identity", width = .5)+
        ggplot2::xlab("diagnosis year")+
        ggplot2::ylab("Total Cost per Year (1000 won)")+
        ggplot2::ggtitle(paste(cancerList$cohortName[[i]], "Cancer Total Cost during 1year after diagnosis", sep = " "))+
        ggplot2::theme_bw()+
        ggplot2::theme(legend.title = element_blank(),
                       legend.text = element_text(size = 15),
                       plot.title = element_text(size = 17),
                       axis.text.x = element_text(size = 12),
                       axis.title.x = element_text(size = 15),
                       axis.text.y = element_text(size = 12),
                       axis.title.y = element_text(size = 15))
    
    return(PlottotalcostperYrdiv)
}

#'the cost of the diagnosed year paid by payer 
#'@param costData   outcome of Argos package extractVisitCost code if minCostDateUnit = "year" 
#'@import dplyr
#'@import ggplot2
#'@export
plotforCostPerYrBarPay<- function(costData){
    costpayerperYr<- costData %>%
        filter( dateUnit == 0 ) %>%
        mutate( visitConceptId = factor(visitConceptId, levels = c(9201, 9202, 9203), labels = c("inpatient", "outpatient", "emergency room"))) %>%
        group_by(cohortStartYear, visitConceptId) %>%
        summarise(avgCostSumperYr = (sum(paidByPayerSum)/sum(subjectCount))*0.001) 
    
    plotperYr_barplot_payer<- ggplot2::ggplot(data = costpayerperYr, ggplot2::aes(x = as.factor(cohortStartYear), y = avgCostSumperYr, fill = visitConceptId))+
        ggplot2::geom_bar(stat = "identity", width = 0.5)+
        ggplot2::xlab("diagnosis year")+
        ggplot2::ylab("Total Cost per Year (1000 won)")+
        ggplot2::ggtitle(paste(cancerList$cohortName[[i]], "Cancer Cost paid by payer during 1year after diagnosis", sep = " "))+
        ggplot2::theme_bw()+
        ggplot2::theme(legend.title = element_blank(),
                       legend.text = element_text(size = 15),
                       plot.title = element_text(size = 17),
                       axis.text.x = element_text(size = 12),
                       axis.title.x = element_text(size = 15),
                       axis.text.y = element_text(size = 12),
                       axis.title.y = element_text(size = 15))
    
    return(plotperYr_barplot_payer)
}

#'the cost of the diagnosed year paid by patient
#'@param costData   outcome of Argos package extractVisitCost code if minCostDateUnit = "year" 
#'@import dplyr
#'@import ggplot2
#'@export
plotforCostPerYrBarPat<- function(costData){
    costpatientperYr<- costData %>%
        filter( dateUnit == 0 ) %>%
        mutate( visitConceptId = factor(visitConceptId, levels = c(9201, 9202, 9203), labels = c("inpatient", "outpatient", "emergency room"))) %>%
        group_by(cohortStartYear, visitConceptId) %>%
        summarise(avgCostSumperYr = (sum(paidByPatientSum)/sum(subjectCount))*0.001) 
    
    plotperYr_barplot_patient<- ggplot2::ggplot(data = costpatientperYr, ggplot2::aes(x = as.factor(cohortStartYear), y = avgCostSumperYr, fill = visitConceptId))+
        ggplot2::geom_bar(stat = "identity", width = 0.5)+
        #ggplot2::geom_line(aes(colour = visitConceptId, group = visitConceptId), size = 0.8)+
        ggplot2::xlab("diagnosis year")+
        ggplot2::ylab("Total Cost per Year (1000 won)")+
        ggplot2::ggtitle(paste(cancerList$cohortName[[i]], "Cancer Cost paid by patient during 1year after diagnosis", sep = " "))+
        ggplot2::theme_bw()+
        ggplot2::theme(legend.title = element_blank(),
                       legend.text = element_text(size = 15),
                       plot.title = element_text(size = 17),
                       axis.text.x = element_text(size = 12),
                       axis.title.x = element_text(size = 15),
                       axis.text.y = element_text(size = 12),
                       axis.title.y = element_text(size = 15))
    
    return(plotperYr_barplot_patient)
}

