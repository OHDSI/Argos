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

#'total patients's survival rate plot by diagnosis year 
#'@param totalSurvCal outcome of Argos package calculateSurvival code if Agediv = FALSE 
#'@import dplyr
#'@import ggplot2
#'@export
plotSurvivalTotal <- function(totalSurvCal){
    SurvCal<- totalSurvCal %>%
        mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women")))
    
    plottotalsurvival<-ggplot2::ggplot(data = SurvCal, ggplot2::aes(x = as.factor(startYear), y = relativesurvival1Yr, group = 1)) +
        ggplot2::geom_point() +
        ggplot2::geom_line(size = 1) +
        ggplot2::geom_point(aes( y = relativesurvival3Yr)) +
        ggplot2::geom_line(aes( y = relativesurvival3Yr), linetype = "dashed", size =1) +
        ggplot2::geom_point(aes(y = relativesurvival5Yr))+
        ggplot2::geom_line(aes( y = relativesurvival5Yr), linetype = "dotted", size = 1) +
        ggplot2::xlab("Diagnosis Time") +
        ggplot2::ylab("Total survival rate") +
        ggplot2::facet_wrap(~genderConceptId) +
        ggplot2::ggtitle(paste( cancerList$cohortName[[i]],"Cancer","Total Survival Rate", sep = " ")) +
        ggplot2::ylim(0,1)+
        ggplot2::theme_bw()+
        ggplot2::theme(legend.title = element_blank(),
                       legend.text = element_text(size = 15),
                       plot.title = element_text(size = 17),
                       axis.text.x = element_text(size = 12),
                       axis.title.x = element_text(size = 15),
                       axis.text.y = element_text(size = 12),
                       axis.title.y = element_text(size = 15),
                       strip.text.x = element_text(size = 15))
    return(plottotalsurvival)
}

#'1year survival rate plot by diagnosis year
#'@param agedivSurvCal outcome of Argos package calculateSurvival code if Agediv = TRUE 
#'@import dplyr
#'@import ggplot2
#'@export
plotSurvival1Yr <- function(agedivSurvCal){
    SurvCal<- agedivSurvCal %>%
        mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women")))
    
    plot1yrsurvival<-ggplot2::ggplot(data = SurvCal, ggplot2::aes(x = as.factor(startYear), y = relativesurvival1Yr , group = age, colour = as.factor(age))) +
        ggplot2::geom_point() +
        ggplot2::geom_line(size = 1) +
        ggplot2::xlab("Diagnosis Time") +
        ggplot2::ylab("1year survival rate") +
        ggplot2::facet_wrap(~genderConceptId) +
        ggplot2::ggtitle(paste(cancerList$cohortName[[i]],"Cancer","1year Survival Rate", sep = " ")) +
        ggplot2::ylim(0,1)+
        ggplot2::theme_bw()+
        ggplot2::theme(legend.title = element_blank(),
                       legend.text = element_text(size = 15),
                       plot.title = element_text(size = 17),
                       axis.text.x = element_text(size = 12),
                       axis.title.x = element_text(size = 15),
                       axis.text.y = element_text(size = 12),
                       axis.title.y = element_text(size = 15),
                       strip.text.x = element_text(size = 15))
    return(plot1yrsurvival)
}

#'3year survival rate plot by diagnosis year
#'@param agedivSurvCal outcome of Argos package calculateSurvival code if Agediv = TRUE 
#'@import dplyr
#'@import ggplot2
#'@export

plotSurvival3Yr <- function(agedivSurvCal){
    SurvCal<- agedivSurvCal %>%
        mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women")))
    
    plot3yrsurvival<-ggplot2::ggplot(data = SurvCal, ggplot2::aes(x = as.factor(startYear), y = relativesurvival3Yr , group = age, colour = as.factor(age))) +
        ggplot2::geom_point() +
        ggplot2::geom_line(size = 1) +
        ggplot2::xlab("Diagnosis Time") +
        ggplot2::ylab("3year survival rate") +
        ggplot2::facet_wrap(~genderConceptId) +
        ggplot2::ggtitle(paste(cancerList$cohortName[[i]],"Cancer","3year Survival Rate", sep = " ")) +
        ggplot2::ylim(0,1)+
        ggplot2::theme_bw()+
        ggplot2::theme(legend.title = element_blank(),
                       legend.text = element_text(size = 15),
                       plot.title = element_text(size = 17),
                       axis.text.x = element_text(size = 12),
                       axis.title.x = element_text(size = 15),
                       axis.text.y = element_text(size = 12),
                       axis.title.y = element_text(size = 15),
                       strip.text.x = element_text(size = 15))
    return(plot3yrsurvival)
}    

#'5year survival rate plot by diagnosis year
#'@param agedivSurvCal outcome of Argos package calculateSurvival code if Agediv = TRUE 
#'@import dplyr
#'@import ggplot2
#'@export

plotSurvival5Yr <- function(agedivSurvCal){
    SurvCal<- agedivSurvCal %>%
        mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women")))
    
    plot5yrsurvival<-ggplot2::ggplot(data = SurvCal, ggplot2::aes(x = as.factor(startYear), y = relativesurvival5Yr , group = age, colour = as.factor(age))) + 
        ggplot2::geom_point() + 
        ggplot2::geom_line(size = 1) + 
        ggplot2::xlab("Diagnosis Time") + 
        ggplot2::ylab("5year survival rate") + 
        ggplot2::facet_wrap(~genderConceptId) +
        ggplot2::ggtitle(paste(cancerList$cohortName[[i]],"Cancer","5year Survival Rate", sep = " ")) +  
        ggplot2::ylim(0,1)+
        ggplot2::theme_bw()+
        ggplot2::theme(legend.title = element_blank(),
                       legend.text = element_text(size = 15),
                       plot.title = element_text(size = 17),
                       axis.text.x = element_text(size = 12),
                       axis.title.x = element_text(size = 15),
                       axis.text.y = element_text(size = 12),
                       axis.title.y = element_text(size = 15),
                       strip.text.x = element_text(size = 15))
    return(plot5yrsurvival)
}

