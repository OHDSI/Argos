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
#'@import dplyr
#'@import ggplot2
#'@export
## incidence proportion plot by birth Year
PlotByBirthInc<- function (incidencePropdata,
                        title,
                        outputFolder){
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
                  ggplot2::ggtitle(title) + 
                  ggplot2::theme_bw()
}

#'@import dplyr
#'@import ggplot2
#'@export
## incidence proportion plot by diagnosis year 
PlotByDiagnosisInc <- function(incidencePropdata,
                            ageSpetitle,
                            ageAdjtitle,
                            outputFolder){
    ageSpe<- incidencePropdata %>%
             mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women"))) %>%
             group_by(startYear, age, genderConceptId) %>%
             summarise( proportion = (sum(targetPopNum)/sum(refPopulation))*100000,
                        stdproportion = sum(standProp)*100000)
    
    ageAdj<- incidencePropdata %>%
             mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women"))) %>%
             group_by(startYear, genderConceptId) %>%
             summarize( AgeadjProp = sum(standProp)*100000)
    
    ageSpePlot<- ggplot2::ggplot(data = ageSpe, ggplot2::aes(x = as.factor(startYear), y = proportion, group = age, colour = as.factor(age))) + 
                 ggplot2::geom_point() + 
                 ggplot2::geom_line(size = 1) + 
                 ggplot2::xlab("Diagnosis Time") + 
                 ggplot2::ylab("incidence proportion") + 
                 ggplot2::facet_wrap(~genderConceptId) +
                 ggplot2::ggtitle(ageSpetitle) +  
                 ggplot2::theme_bw()
      
    ageAdjPlot<- ggplot2::ggplot(data = ageAdj, ggplot2::aes(x = as.factor(startYear), y = AgeadjProp, group = 1)) + 
                 ggplot2::geom_point() + 
                 ggplot2::geom_line(size = 1) +
                 ggplot2::xlab("Diagnosis Time") + 
                 ggplot2::ylab("incidence proportion") + 
                 ggplot2::facet_wrap(~genderConceptId) +
                 ggplot2::ggtitle(ageAdjtitle) + 
                 ggplot2::theme_bw()
}
