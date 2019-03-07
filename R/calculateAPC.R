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


ArgosResult<-data.frame()
for(i in unique(cancerList$cohortId)){
    i<-2
    incidenceData <- Argos::getIncidenceData(connectionDetails = connectionDetails,
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             cohortDatabaseSchema = cohortDatabaseSchema,
                                             cohortTable = cohortTable,
                                             outcomeDatabaseSchema = cohortDatabaseSchema ,
                                             targetCohortId = cancerList$cohortId[i],
                                             minDateUnit = "year")
    incCal<-Argos::calculateIncidence(incidenceData = incidenceData,
                                      basePopulation = basePop,
                                      refPopulation = refPop,
                                      standardization = "direct",
                                      Agestandardization = TRUE,
                                      genderStandardization = TRUE,
                                      startYearStandardization = TRUE,
                                      AgeSet = list(30:39,
                                                    40:49,
                                                    50:59,
                                                    60:69,
                                                    70:79,
                                                    80:99),
                                      genderSet = list(8507,8532),
                                      startYearSet = startYearSet,
                                      birthYearSet = list(1910:1919, 1920:1929,
                                                          1930:1939, 1940:1949,
                                                          1950:1959, 1960:1964,
                                                          1965:1969, 1970:1974,
                                                          1975:1979, 1980:1989))
    
    ageAdj<- incCal %>%
        mutate( genderConceptId = factor(genderConceptId, levels = c(8507, 8532), labels = c("men", "women"))) %>%
        group_by(startYear, genderConceptId) %>%
        summarize( AgeadjProp = sum(standProp)*100000)
    
    ageAdj_women_lm<-ageAdj %>%
        lm(formula = log(AgeadjProp)~startYear)
    female_slope<-summary(ageAdj_women_lm)$coefficients["startYear","Estimate"]
    female_p_value<-summary(ageAdj_women_lm)$coefficients["startYear","Pr(>|t|)"]
    female_APC<-(exp(female_slope)-1)*100
    
    ageAdj_male_lm<-ageAdj %>%
        filter(genderConceptId == 'men') %>%
        lm(formula = log(AgeadjProp)~startYear)
    male_slope<-summary(ageAdj_male_lm)$coefficients["startYear","Estimate"]
    male_p_value<-summary(ageAdj_male_lm)$coefficients["startYear","Pr(>|t|)"]
    male_APC<-(exp(male_slope)-1)*100
    
    
    df<-data.frame(cohortId = i,
                   femaleSlope = female_slope,
                   femalePValue = female_p_value,
                   femaleAPC = female_APC,
                   maleSlope = male_slope,
                   malePValue = male_p_value,
                   maleAPC = male_APC)
    ArgosResult<-rbind(ArgosResult, df)
    write.csv(ArgosResult, file.path(outputFolder, "incidenceAPCArgos.csv"))
}

##for comparing with reference 
# data.frame()
# refincCalMale<-list(cohortId_1<-data.frame(startYear = c(2003:2012),
#                                            ageAdj = c(35.3,38.0,41.2,43.3,45.3,47.0,49.9,50.0,51.9,50.0)),
#                     cohortId_2<-data.frame(startYear = c(2003:2012),
#                                            ageAdj = c(50.0,50.8,50.9,49.2,48.7,47.6,47.5,47.5,46.4,44.3)),
#                     cohortId_3<-data.frame(startYear = c(2003:2012),
#                                            ageAdj = c(66.0,62.3,66.9,65.4,63.1,64.5,65.0,63.3,63.7,59.3)),
#                     cohortId_4<-data.frame(startYear = c(2003:2012),
#                                            ageAdj = c(0.2,0.2,0.2,0.2,0.1,0.3,0.2,0.2,0.2,0.2)),
#                     cohortId_5<-data.frame(startYear = c(2003:2012),
#                                            ageAdj = c(42.3,42.2,42.6,40.3,39.8,39.5,38.2,36.8,35.9,34.3)),
#                     cohortId_6<-data.frame(startYear = c(2003:2012),
#                                            ageAdj = c(3.7,4.8,5.9,7.5,10.0,13.3,15.6,18.6,20.4,23.0)))
# refincCalFemale<-list(cohortId_1<-data.frame(startYear = c(2003:2012),
#                                              ageAdj = c(20.5,21.5,23.0,24.1,24.6,25.2,26.3,26.1,26.7,26.8)),
#                       cohortId_2<-data.frame(startYear = c(2003:2012),
#                                              ageAdj = c(12.4,13.0,13.5,14.0,14.0,14.2,14.2,14.7,15.3,14.9)),
#                       cohortId_3<-data.frame(startYear = c(2003:2012),
#                                              ageAdj = c(25.9,24.7,26.8,25.1,24.8,25.1,25.7,25.3,25.3,23.5)),
#                       cohortId_4<-data.frame(startYear = c(2003:2012),
#                                              ageAdj = c(27.9,29.4,32.0,33.3,35.7,37.2,38.8,40.7,44.0,44.7)),
#                       cohortId_5<-data.frame(startYear = c(2003:2012),
#                                              ageAdj = c(11.5,11.3,11.4,11.1,11.1,10.7,10.6,10.4,10.4,9.5)),
#                       cohortId_6<-data.frame(startYear = c(2003:2012),
#                                              ageAdj = c(21.8,29.6,35.4,43.3,55.7,69.5,80.7,88.8,98.0,102.4)))
# 
# RefResults<-data.frame()                
# for(i in unique(cancerList$cohortId)){                                      
# 
#     ageAdj_male<-refincCalMale[[i]]
#     ageAdj_male_lm<-ageAdj_male %>%
#         lm(formula = log(ageAdj)~startYear)
#     male_slope<-summary(ageAdj_male_lm)$coefficients["startYear","Estimate"]
#     male_p_value<-summary(ageAdj_male_lm)$coefficients["startYear","Pr(>|t|)"]
#     male_APC<-(exp(male_slope)-1)*100
#     
#     ageAdj_female<-refincCalFemale[[i]]
#     ageAdj_female_lm<-ageAdj_female %>%
#         lm(formula = log(ageAdj)~startYear)
#     female_slope<-summary(ageAdj_female_lm)$coefficients["startYear","Estimate"]
#     female_p_value<-summary(ageAdj_female_lm)$coefficients["startYear","Pr(>|t|)"]
#     female_APC<-(exp(female_slope)-1)*100
#     
#     df<-data.frame(cohortId = i,
#                    femaleSlope = female_slope,
#                    femalePValue = female_p_value,
#                    femaleAPC = female_APC,
#                    maleSlope = male_slope,
#                    malePValue = male_p_value,
#                    maleAPC = male_APC)
#     RefResults<-rbind(RefResults, df)
#     write.csv(RefResults, file.path(outputFolder, "RefAPC.csv"))
# }
# 

for(i in unique(cancerList$cohortId)){
    SurvData<-Argos::readySurvData(connectionDetails = connectionDetails, 
                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                   outcomeDatabaseSchema = cohortDatabaseSchema ,
                                   cohortTable = cohortTable,
                                   covariateSettings = covariateSettings,
                                   targetCohortId = cancerList$cohortId[i],
                                   outcomeId,
                                   requireTimeAtRisk = FALSE,
                                   riskWindowStart = 0,
                                   riskWindowEnd = 365*5,
                                   removeSubjectsWithPriorOutcome = TRUE,
                                   minDateUnit = "year")
    
    totalSurvCal<-Argos::calculateSurvival(survivalData = SurvData,
                                           refPopulation = refPop,
                                           Agedivided = FALSE,
                                           AgeSet = list(30:39,
                                                         40:49,
                                                         50:59,
                                                         60:69,
                                                         70:79,
                                                         80:99),
                                           genderSet = list(8507,8532),
                                           startYearSet = startYearSet,
                                           birthYearSet = list(1960:1964, 1965:1969, 1970:1974, 1975:1979, 1980:1984, 1985:1989),
                                           observationEndYear = 2013)
    SurvpercentChange<-totalSurvCal%>%
        filter(genderConceptId == 8507) %>%
        filter(!is.na(survival5Yr)) %>%
        filter(startYear %in% c(max(startYear), min(startYear)) )%>%
        summarise(survivalChange = survival5yr)
    
}