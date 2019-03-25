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

#'calculate Annual percent change of age adjusted incidence proportion 
#'@param incidencePropdata  
#'@import dplyr
#'@export
ageAdjAPC<-function(incidencePropdata = incCal){
    
    ageAdj<- incidencePropdata$incidenceCalculate %>%
        mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women"))) %>%
        group_by(startYear, genderConceptId) %>%
        summarize( AgeadjProp = sum(standProp)*100000)
    
    ageAdj_women_lm<-ageAdj %>%
        filter(genderConceptId == 8507) %>%
        lm(formula = log(AgeadjProp)~startYear)
    female_slope<-summary(ageAdj_women_lm)$coefficients["startYear","Estimate"]
    female_p_value<-summary(ageAdj_women_lm)$coefficients["startYear","Pr(>|t|)"]
    female_APC<-(exp(female_slope)-1)*100
    
    ageAdj_male_lm<-ageAdj %>%
        filter(genderConceptId == 8532) %>%
        lm(formula = log(AgeadjProp)~startYear)
    male_slope<-summary(ageAdj_male_lm)$coefficients["startYear","Estimate"]
    male_p_value<-summary(ageAdj_male_lm)$coefficients["startYear","Pr(>|t|)"]
    male_APC<-(exp(male_slope)-1)*100
    
    
    results<-data.frame(femaleSlope = female_slope,
                        femalePValue = female_p_value,
                        femaleAPC = female_APC,
                        maleSlope = male_slope,
                        malePValue = male_p_value,
                        maleAPC = male_APC)
    
    return(results)    
}
