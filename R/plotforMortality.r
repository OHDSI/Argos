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
#
#'
#'plot mortality proportion

## mortality proportion plot by birth Year
PlotByBirth<- function (mortalityPropdata = outCal,
                        savefile = outputFolder,
                        imageExtension = "png"){
    bybirth<- outCal %>%
              mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women"))) %>%
              group_by(birthYear, age, genderConceptId) %>%
              summarise( proportion = sum(outcomeNum)/sum(targetPopNum),
                         stdproportion = sum(standProp))
    
    bybirthPlot<- ggplot2::ggplot(data = bybirth, ggplot2::aes(x = as.factor(birthYear), y = proportion, group = age, colour = as.factor(age))) + 
                  ggplot2::geom_point() + 
                  ggplot2::geom_line(size = 1) + 
                  ggplot2::xlab("Year of Birth") + 
                  ggplot2::ylab("mortality proportion") + 
                  ggplot2::facet_wrap(~genderConceptId) +
                  ggplot2::ggtitle(paste(cancerList$cohortName[i], "Cancer", "Mortality Proportion By Birth Year", sep = " ")) + 
                  ggplot2::theme_bw()
    ggplot2::ggsave(bybirthPlot, file.path(savefile, paste0(cancerList$cohortName[i],"CancerMP_byBirthYr.", imageExtension)), width = 30,height = 15,units = "cm" ) 
}

## mortality proportion plot by diagnosis year 
PlotByDiagnosis <- function(incidencePropdata = outCal,
                            savefile = outputFolder,
                            imageExtension = "png"){
    ageSpe<- incCal %>%
             mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women"))) %>%
             group_by(startYear, age, genderConceptId) %>%
             summarise( proportion = sum(targetPopNum)/sum(refPopulation),
                        stdproportion = sum(standProp))
    
    ageAdj<- incCal %>%
             mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women"))) %>%
             group_by(startYear, genderConceptId) %>%
             summarize( AgeadjProp = sum(standProp))
    
    ageSpePlot<- ggplot2::ggplot(data = ageSpe, ggplot2::aes(x = as.factor(startYear), y = proportion, group = age, colour = as.factor(age))) + 
                 ggplot2::geom_point() + 
                 ggplot2::geom_line(size = 1) + 
                 ggplot2::xlab("Diagnosis Time") + 
                 ggplot2::ylab("mortality proportion") + 
                 ggplot2::facet_wrap(~genderConceptId) +
                 ggplot2::ggtitle(paste(cancerList$cohortName[i], "Cancer", "Age Specified Mortality Proportion", sep = " ")) + 
                 ggplot2::theme_bw()
    ggplot2::ggsave(ageSpePlot, file.path(savefile, paste0(cancerList$cohortName[i],"CancerASMP_byDiagnosisYr.", imageExtension)), width = 30,height = 15,units = "cm" ) 
    
    ageAdjPlot<- ggplot2::ggplot(data = ageAdj, ggplot2::aes(x = as.factor(startYear), y = AgeadjProp, group = 1)) + 
                 ggplot2::geom_point() + 
                 ggplot2::geom_line(size = 1) +
                 ggplot2::xlab("Diagnosis Time") + 
                 ggplot2::ylab("mortality proportion") + 
                 ggplot2::facet_wrap(~genderConceptId) +
                 ggplot2::ggtitle(paste(cancerList$cohortName[i], "Cancer", "Age Adjusted Mortality Proportion", sep = " ")) + 
                 ggplot2::theme_bw()
    ggplot2::ggsave(ageAdjPlot, file.path(savefile, paste0(cancerList$cohortName[i],"CancerADMP_byDiagnosisYr.", imageExtension)), width = 30,height = 15,units = "cm" ) 
}

