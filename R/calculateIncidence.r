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

#'get incidence data
#'@param connectionDetails
#'@param cdmDatabaseSchema          
#'@param cohortDatabaseSchema
#'@param outcomeDatabaseSchema
#'@param covariateSettings
#'@param cohortTable
#'@param targetCohortId
#'@param minDateUnit                minumal unit for cohort start date ('year' > 'quarter' > 'month' > 'day')
#'@export
#'
getIncidenceData<-function(connectionDetails, 
                           cdmDatabaseSchema,
                           cohortDatabaseSchema,
                           outcomeDatabaseSchema,
                           cohortTable,
                           targetCohortId,
                           minDateUnit = 'year'){
    plpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails, 
                                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                                  cohortTable = cohortTable,
                                                  cohortId = targetCohortId,
                                                  covariateSettings = covariateSettings,
                                                  outcomeDatabaseSchema = cohortDatabaseSchema,
                                                  outcomeTable = cohortTable,
                                                  outcomeIds = targetCohortId,
                                                  sampleSize = NULL)
    
    resultData<-calculateNumberPerCovTime(plpData = plpData,
                                          population = NULL,
                                          minDateUnit = minDateUnit)
    result<-list(data=resultData,
                 targetCohortId = targetCohortId,
                 minDateUnit = minDateUnit)
    class(result)<-"incidenceData"
    
    return(result)
}

#'calculate incidence data
#'@param incidenceData
#'@param basePopulation
#'@param standardization
#'@param refPopulation
#'@param Agestandardization
#'@param genderStandardization
#'@param startYearStandardization
#'@param AgeSet
#'@param genderSet
#'@param startYearSet
#'@param birthYearSet           
#'@export
calculateIncidence<-function(incidenceData = incidenceData,
                             basePopulation = basePop,
                             standardization = "direct",
                             refPopulation = NULL,
                             Agestandardization = TRUE,
                             genderStandardization = TRUE,
                             startYearStandardization = TRUE,
                             AgeSet = list(20:29,30:39,40:49,50:59,60:69,70:79,80:99),
                             genderSet = list(8507,8532),
                             startYearSet = list(2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013),
                             birthYearSet = list(1960:1964, 1965:1969, 1970:1974, 1975:1979, 1980:1984, 1985:1989)){
    
    incD<-incidenceData$data
    incD$birthYear<-incD$cohortStartYear-incD$age
    
    incPop<-dplyr::inner_join(incD,basePop,by = c("cohortStartYear"="startYear", "age" = "startAge", "genderConceptId"="genderConceptId") )
    incPop %>% arrange(cohortStartYear,age)
    settings<-list(age=AgeSet, gender=genderSet, startYear=startYearSet, birthYear = birthYearSet)
    expanded.set<-expand.grid(settings)
    
    resultDf <- data.frame()
    ##for loop should be replaced by apply function
    
    if(standardization=="direct"){
        refPopulation$stdWt<-refPopulation$standardPopulation/sum(refPopulation$standardPopulation)
        
        for (i in seq(nrow(expanded.set))){
            df<-incPop %>% 
                filter(age %in% unlist(expanded.set[i,]$age))  %>%
                filter(genderConceptId %in% unlist(expanded.set[i,]$gender) ) %>%
                filter(cohortStartYear %in% unlist(expanded.set[i,]$startYear) ) %>%
                filter(birthYear %in% unlist(expanded.set[i,]$birthYear) )
            if(nrow(df)==0) next
            df<-dplyr::inner_join(df,refPopulation,by=c("age"="startAge", "endAge"="endAge","genderConceptId"="genderConceptId"))
            
            tempDf<-data.frame(startYear = min(unlist(expanded.set[i,]$startYear)),
                               age = min(unlist(expanded.set[i,]$age)),
                               birthYear = min(unlist(expanded.set[i,]$birthYear)),
                               genderConceptId = unlist(expanded.set[i,]$gender),
                               targetPopNum = sum(df$aggregatedNum, na.rm = TRUE),
                               basePop = sum(df$population, na.rm = TRUE),
                               refPopulation = sum(df$standardPopulation, na.rm = TRUE),
                               standardPopWt = sum(df$stdWt, na.rm = T),
                               proportion = sum(df$aggregatedNum, na.rm = TRUE) / sum(df$population, na.rm = TRUE)
                               #,standProp = sum(df$stdWt* (df$aggregatedNum/df$population)
                               )
            
            resultDf<-rbind(resultDf,tempDf)
        }
    }else{
        for (i in seq(nrow(expanded.set))){
            df<-incPop %>% 
                filter(age %in% unlist(expanded.set[i,]$age))  %>%
                filter(genderConceptId %in% unlist(expanded.set[i,]$gender) ) %>%
                filter(cohortStartYear %in% unlist(expanded.set[i,]$startYear) ) %>%
                filter(birthYear %in% unlist(expanded.set[i,]$birthYear) )
            if(nrow(df)==0) next
            
            tempDf<-data.frame(startYear = min(unlist(expanded.set[i,]$startYear)),
                               age = min(unlist(expanded.set[i,]$age)),
                               birthYear = min(unlist(expanded.set[i,]$birthYear)),
                               genderConceptId = unlist(expanded.set[i,]$gender),
                               targetPopNum = sum(df$aggregatedNum, na.rm = TRUE),
                               basePop = sum(df$population, na.rm = TRUE),
                               proportion = sum(df$aggregatedNum, na.rm = TRUE) / sum(df$population, na.rm = TRUE))
            resultDf<-rbind(resultDf,tempDf)
        }
    }
    resultDf<-unique(resultDf)
    return(resultDf)
}


#'aggregate incidence proportion according to gender and age section
#'@param  incidencePropdata output of Argos packages calculateIncidence code 
#'@import dplyr
#'@export

agespe<- function(incidencePropdata){
    ageSpecdata<-incidencePropdata %>%
        group_by(startYear, age, genderConceptId) %>%
        summarise( targetPop = sum(targetPopNum, na.rm = T),
                   basePop = sum(basePop, na.rm = T),
                   refPop = sum(refPopulation, na.rm = T),
                   proportion = (sum(targetPopNum)/sum(basePop)),
                   stdPopWt = sum(standardPopWt, na.rm = T))
    
    return(ageSpecdata)
}

#'sum age adjusted incidence proportion and calculate 95% lowerCI and 95% upperCI 
#'@param  agespecifiedPropdata output of Argos packages calculateIncidence code, agespe 
#'@param  alpha 0.05
#'@import dplyr
#'@export
ageadjust<-function(agespecifiedPropdata,
                    alpha = 0.05){
    ageAdjudata<- agespecifiedPropdata %>%
        group_by(startYear, genderConceptId) %>%
        summarize( SumbasePop = sum(basePop),
                   crudeProp = sum(targetPop) / sum(basePop),
                   AgeadjProp = sum(proportion*stdPopWt, na.rm = T),
                   dsr_var = sum(stdPopWt^2*proportion^2/basePop, na.rm = T),
                   wm = max(stdPopWt/basePop)) %>%
        mutate(lci = qgamma(alpha/2, 
                            shape = (AgeadjProp^2/dsr_var),
                            scale = dsr_var/AgeadjProp),
               uci = qgamma(1-alpha/2,
                            shape = ((AgeadjProp+wm)^2/(dsr_var+wm^2)),
                            scale = (dsr_var+wm^2)/(AgeadjProp+wm))) %>%
        mutate(crudeProp = crudeProp*100000,
               AgeadjProp = AgeadjProp*100000,
               lci = lci*100000,
               uci = uci*100000)
    return(ageAdjudata)
}


#'aggregate incidence proportion according to gender and birth year section
#'@param  incidencePropdata output of Argos packages calculateIncidence code  
#'@import dplyr
#'@export
bybirth<- function(incidencePropdata){
    bybirthdata<-incidencePropdata %>%
        group_by(birthYear, age, genderConceptId) %>%
        summarise( proportion = (sum(targetPopNum)/sum(refPopulation))*100000,
                   stdproportion = sum(standProp)*100000)
    
    return(bybirthdata)
}
