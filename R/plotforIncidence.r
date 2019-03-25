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

#'incidence proportion plot by birth Year
#'@param birthcohortIncData output of Argos packages bybirth code 
#'@import dplyr
#'@import ggplot2
#'@export
PlotByBirthInc<- function (birthcohortIncData){
    bybirth<- birthcohortIncData %>%
        mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women"))) %>%
        mutate( proportion = proportion*100000)
    
    bybirthPlot<- ggplot2::ggplot(data = bybirth, aes(x = as.factor(birthYear), y = proportion, group = age, colour = as.factor(age))) + 
        ggplot2::geom_point() + 
        ggplot2::geom_line(size = 1) + 
        ggplot2::xlab("Year of Birth") + 
        ggplot2::ylab("incidence rate") + 
        ggplot2::facet_wrap(~genderConceptId) +
        ggplot2::ggtitle(paste(cancerList$cohortName[[i]],"Cancer", "Incidence Rate By Birth Year", sep = " ")) + 
        ggplot2::theme_bw()+
        ggplot2::theme(legend.title = element_blank(),
                       legend.text = element_text(size = 15),
                       plot.title = element_text(size = 17),
                       axis.text.x = element_text(size = 12),
                       axis.title.x = element_text(size = 15),
                       axis.text.y = element_text(size = 12),
                       axis.title.y = element_text(size = 15),
                       strip.text.x = element_text(size = 15))
    
    return(bybirthPlot)
}

#'Age specified incidence proportion plot by diagnosis year
#'@param agespecifiedIncData output of Argos packages agespe code 
#'@import dplyr
#'@import ggplot2
#'@export
PlotByDiagnosisIncAgeS <- function(agespecifiedIncData){
    ageSpe<- agespecifiedIncData %>%
        mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women"))) %>%
        mutate( proportion = proportion*100000)
    
    ageSpePlot<- ggplot2::ggplot(data = ageSpe, aes(x = as.factor(startYear), y = proportion, group = age, colour = as.factor(age))) + 
        ggplot2::geom_point() + 
        ggplot2::geom_line(size = 1) + 
        ggplot2::xlab("Diagnosis Time") + 
        ggplot2::ylab("incidence rate") + 
        ggplot2::facet_wrap(~genderConceptId) +
        ggplot2::ggtitle(paste(cancerList$cohortName[[i]],"Cancer","Incidence Rate According to Age", sep = " ")) +  
        ggplot2::theme_bw()+
        ggplot2::theme(legend.title = element_blank(),
                       legend.text = element_text(size = 15),
                       plot.title = element_text(size = 17),
                       axis.text.x = element_text(size = 12),
                       axis.title.x = element_text(size = 15),
                       axis.text.y = element_text(size = 12),
                       axis.title.y = element_text(size = 15),
                       strip.text.x = element_text(size = 15))
    
    return(ageSpePlot)
}

#'Age adjusted incidence proportion plot by diagnosis year 
#'@param ageadjustIncData output of Argos packages ageadjust code   
#'@import dplyr
#'@import ggplot2
#'@export
PlotByDiagnosisIncAgeAd <- function(ageadjustIncData){
    ageAdj<- ageadjustIncData %>%
        mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women")))
    
    ageAdjPlot<- ggplot2::ggplot(data = ageAdj, aes(x = as.factor(startYear), y = AgeadjProp, group = 1)) + 
        ggplot2::geom_point() + 
        ggplot2::geom_line(size = 1) +
        ggplot2::xlab("Diagnosis Time") + 
        ggplot2::ylab("incidence rate") + 
        ggplot2::facet_wrap(~genderConceptId) +
        ggplot2::ggtitle(paste(cancerList$cohortName[[i]], "Cancer", "Age standardized Incidence Rate", sep = " ")) + 
        ggplot2::theme_bw()+
        ggplot2::theme(#legend.title = element_blank(),
            #legend.text = element_text(size = 15),
            plot.title = element_text(size = 17),
            axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 15),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 15),
            strip.text.x = element_text(size = 15))
    
    return(ageAdjPlot)
}
