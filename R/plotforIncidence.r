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
#'@param incidencePropdata
#'@import dplyr
#'@import ggplot2
#'@export
PlotByBirthInc<- function (incidencePropdata){
    bybirth<- incidencePropdata %>%
        mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women"))) %>%
        group_by(birthYear, age, genderConceptId) %>%
        summarise( proportion = (sum(targetPopNum)/sum(refPopulation))*100000,
                   stdproportion = sum(standProp)*100000)
    
    bybirthPlot<- ggplot2::ggplot(data = bybirth, ggplot2::aes(x = as.factor(birthYear), y = proportion, group = age, colour = as.factor(age))) + 
        ggplot2::geom_point() + 
        ggplot2::geom_line(size = 1) + 
        ggplot2::xlab("Year of Birth") + 
        ggplot2::ylab("incidence proportion") + 
        ggplot2::facet_wrap(~genderConceptId) +
        ggplot2::ggtitle(paste(cancerList$cohortName[[i]],"Cancer", "Incidence Proportion By Birth Year", sep = " ")) + 
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
#'@param incidencePropdata  outcome of Argos package getIncidenceData code
#'@import dplyr
#'@import ggplot2
#'@export
PlotByDiagnosisIncAgeS <- function(incidencePropdata){
    ageSpe<- incidencePropdata %>%
        mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women"))) %>%
        group_by(startYear, age, genderConceptId) %>%
        summarise( proportion = (sum(targetPopNum)/sum(refPopulation))*100000,
                   stdproportion = sum(standProp)*100000)
    
    ageSpePlot<- ggplot2::ggplot(data = ageSpe, ggplot2::aes(x = as.factor(startYear), y = proportion, group = age, colour = as.factor(age))) + 
        ggplot2::geom_point() + 
        ggplot2::geom_line(size = 1) + 
        ggplot2::xlab("Diagnosis Time") + 
        ggplot2::ylab("incidence proportion") + 
        ggplot2::facet_wrap(~genderConceptId) +
        ggplot2::ggtitle(paste(cancerList$cohortName[[i]],"Cancer","Incidence Proportion According to Age", sep = " ")) +  
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
#'@param incidencePropdata  outcome of Argos package getIncidenceData code
#'@import dplyr
#'@import ggplot2
#'@export
PlotByDiagnosisIncAgeAd <- function(incidencePropdata){
    ageAdj<- incidencePropdata %>%
        mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women"))) %>%
        group_by(startYear, genderConceptId) %>%
        summarize( AgeadjProp = sum(standProp)*100000)
    
    ageAdjPlot<- ggplot2::ggplot(data = ageAdj, ggplot2::aes(x = as.factor(startYear), y = AgeadjProp, group = 1)) + 
        ggplot2::geom_point() + 
        ggplot2::geom_line(size = 1) +
        ggplot2::xlab("Diagnosis Time") + 
        ggplot2::ylab("incidence proportion") + 
        ggplot2::facet_wrap(~genderConceptId) +
        ggplot2::ggtitle(paste(cancerList$cohortName[[i]], "Cancer", "Age standardized Incidence Proportion", sep = " ")) + 
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
