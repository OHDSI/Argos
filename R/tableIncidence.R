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

#'make age adjusted incidence rate table 
#'@import dplyr
#'@import reshape2
#'@param ageadjIncData              result of ageadjust code
#'@export
#'

tableAgeAdjusted <- function(ageadjIncData){
    
    df <- ageadjIncData %>%
        select(startYear,genderConceptId,AgeadjProp)
    
    df_cast <- reshape2::dcast(df,df$genderConceptId~df$startYear)
    colnames(df_cast)[1] <- c("genderConceptId")
    
    APC <- Argos::ageAdjAPC(ageAdjustedInc = ageadjIncData)
    
    df_APC_join <- left_join(df_cast,APC,by = "genderConceptId")
    
    result <- mutate(df_APC_join, genderConceptId = case_when(genderConceptId == 8532 ~ 'women',
                                                              genderConceptId == 8507 ~ 'men') )
    
    return(result)
}

#'make age specified incidence rate table 
#'@import dplyr
#'@import reshape2
#'@param ageSpecifiedData              result of agespe code
#'@export
#'

tableAgeSpecified <- function(ageSpecifiedData){
    df <- ageSpecifiedData %>%
        select(startYear,age,genderConceptId,proportion)%>%
        mutate(proportion = round(proportion*100000,2))
    
    df_cast <- reshape2::dcast(df,df$genderConceptId+df$age~df$startYear)
    colnames(df_cast)[c(1,2)] <- c("genderConceptId","age")
    
    result <- mutate(df_cast, genderConceptId = case_when(genderConceptId == 8532 ~ 'women',
                                                          genderConceptId == 8507 ~ 'men') )
    
    return(result)
}

#'make incidence rate by birth cohort table 
#'@import dplyr
#'@import reshape2
#'@param birthIncData              result of bybirth code
#'@export
#'
tablebirthCohortInc <- function(birthIncData){
unique(incCal$incidenceCalculate$birthYear)
    df <- birthIncData %>%
        select(birthYear,age,genderConceptId,proportion)%>%
        mutate(proportion = round(proportion*100000,2))
    
    df_cast <- reshape2::dcast(df,df$genderConceptId+df$age~df$birthYear)
    colnames(df_cast)[c(1,2)] <- c("genderConceptId","age")
    
    result <- mutate(df_cast, genderConceptId = case_when(genderConceptId == 8532 ~ 'women',
                                                          genderConceptId == 8507 ~ 'men') )
    
    return(result)
}

