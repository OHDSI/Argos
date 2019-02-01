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

#' Load default life expectancy data
#' @param country 'WHO' for world standard life expectancy or 3-digit Coutry code (eg 'KOR' = 'Republic of Korea')
#' @export
loadLifeExpectancy<-function(country){
    
    return(
        read.csv(system.file("census",paste0(country,"_" ,"life_expectancy",".csv"), package = "Argos"))
    )
}

#' Load default mid-year population data
#' @param country 'WHO' for world standard population publisehd by WHO, or 3-digit Coutry code (eg 'KOR' = 'Republic of Korea')
#' @export
loadMidYearPopulation<-function(country){
    
    return(
        read.csv(system.file("census",paste0(country,"_" ,"mid_year_population",".csv"), package = "Argos"))
    )
}

#' Load default disability weight  data
#' @param country  'IHME' or or 3-digit Coutry code (eg 'KOR' = 'Republic of Korea')
#' @param year
#' @export
loadDisabilityWeight<-function(country,year){
    return(
        read.csv(system.file("disability_weight",paste0(country,year,"_disability_weight.csv"), package = "Argos"))
    )
}


# restricts to pop and saves/creates mapping
MapCovariates <- function(covariates, covariateRef, population, map=NULL){
    
    # restrict to population for speed
    OhdsiRTools::logTrace('restricting to population for speed...')
    idx <- ffbase::ffmatch(x = covariates$rowId, table = ff::as.ff(population$rowId))
    idx <- ffbase::ffwhich(idx, !is.na(idx))
    covariates <- covariates[idx, ]
    
    OhdsiRTools::logTrace('Now converting covariateId...')
    oldIds <- as.double(ff::as.ram(covariateRef$covariateId))
    newIds <- 1:nrow(covariateRef)
    
    if(!is.null(map)){
        OhdsiRTools::logTrace('restricting to model variables...')
        OhdsiRTools::logTrace(paste0('oldIds: ',length(map[,'oldIds'])))
        OhdsiRTools::logTrace(paste0('newIds:', max(as.double(map[,'newIds']))))
        ind <- ffbase::ffmatch(x=covariateRef$covariateId, table=ff::as.ff(as.double(map[,'oldIds'])))
        ind <- ffbase::ffwhich(ind, !is.na(ind))
        covariateRef <- covariateRef[ind,]
        
        ind <- ffbase::ffmatch(x=covariates$covariateId, table=ff::as.ff(as.double(map[,'oldIds'])))
        ind <- ffbase::ffwhich(ind, !is.na(ind))
        covariates <- covariates[ind,]
    }
    if(is.null(map))
        map <- data.frame(oldIds=oldIds, newIds=newIds)
    
    return(list(covariates=covariates,
                covariateRef=covariateRef,
                map=map))
}

#' function to limit covariates of plpData to population
limitCovariatesToPopulation <- function(covariates, rowIds) {
    idx <- !is.na(ffbase::ffmatch(covariates$rowId, rowIds))
    if(sum(idx)!=0){
        covariates <- covariates[ffbase::ffwhich(idx, idx == TRUE), ]
    }else{
        stop('No covariates')
    }
    return(covariates)
}

#' function to calculate number per covariate per time unit
#' 
#' @param cohort  cohort should be data frame with at least four columns 'rowId', 'subjectId', 'cohortId', 'cohortStartDate'
calculateNumberPerCovTime <- function(plpData,
                                      population = NULL,
                                      minDateUnit = 'year'){
    if(is.null(population)){
        cohort<-ff::as.ram(plpData$cohort)
        covRef<-ff::as.ram(plpData$covariateRef)
        covariates<-ff::as.ram(plpData$covariates)
    } else {
        #load covariates
        #limit covariates of plpData to the population
        covariates<-ff::as.ram(limitCovariatesToPopulation(covariates = plpData$covariates,rowIds = ff::as.ff(population$rowId)))
        #load covariate reference
        covRef<-ff::as.ram(plpData$covariateRef)
        #limit covarite Ref to the existing covarites in the population
        covRef <- covRef [covRef$covariateId %in% unique(covariates$covariateId), ]
        cohort<-population
    }
    
    if(minDateUnit=='day'){
        cohortStartDateRef <- data.frame(covariateId = c(-1,-2,-3,-4),
                                         covariateName = c("cohortStartYear","cohortStartQuarter","cohortStartMonth","cohortStartDay"),
                                         analysisId = -1,
                                         conceptId = 0)
        covariates<-rbind(covariates,
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -1,
                                     covariateValue = lubridate::year(cohort$cohortStartDate)),
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -2,
                                     covariateValue = lubridate::quarter(cohort$cohortStartDate)),
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -3,
                                     covariateValue = lubridate::month(cohort$cohortStartDate)),
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -4,
                                     covariateValue = lubridate::day(cohort$cohortStartDate))
        )
    }
    
    if(minDateUnit=='month'){
        cohortStartDateRef <- data.frame(covariateId = c(-1,-2,-3),
                                         covariateName = c("cohortStartYear","cohortStartQuarter","cohortStartMonth"),
                                         analysisId = -1,
                                         conceptId = 0)
        covariates<-rbind(covariates,
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -1,
                                     covariateValue = lubridate::year(cohort$cohortStartDate)),
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -2,
                                     covariateValue = lubridate::quarter(cohort$cohortStartDate)),
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -3,
                                     covariateValue = lubridate::month(cohort$cohortStartDate))
        )
    }
    
    if(minDateUnit=='quarter'){
        cohortStartDateRef <- data.frame(covariateId = c(-1,-2),
                                         covariateName = c("cohortStartYear","cohortStartQuarter"),
                                         analysisId = -1,
                                         conceptId = 0)
        covariates<-rbind(covariates,
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -1,
                                     covariateValue = lubridate::year(cohort$cohortStartDate)),
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -2,
                                     covariateValue = lubridate::quarter(cohort$cohortStartDate))
        )
    }
    
    if(minDateUnit=='year'){
        cohortStartDateRef <- data.frame(covariateId = c(-1),
                                         covariateName = c("cohortStartYear"),
                                         analysisId = -1,
                                         conceptId = 0)
        covariates<-rbind(covariates,
                          data.frame(rowId=cohort$rowId,
                                     covariateId = -1,
                                     covariateValue = lubridate::year(cohort$cohortStartDate))
        )
    }
    covRef<-rbind(covRef,cohortStartDateRef)
    covariates$newCovId<-as.numeric(as.factor(covariates$covariateId))
    
    #aggregate accroding to the covariates
    cov.df<-as.data.frame(as.matrix(Matrix::sparseMatrix(i=covariates$rowId,
                                                         j=covariates$newCovId,
                                                         x=covariates$covariateValue)))
    cov.df$n = 1
    
    resultData<-aggregate(n~.-n,cov.df,sum,na.rm=TRUE)
    
    #naming the columns
    colnames(resultData)<-c(as.character(covRef$covariateName[match(levels(as.factor(covariates$covariateId)),as.character(covRef$covariateId))]),
                            "aggregatedNum")
    colnames(resultData)<-gsub("age in years","age", colnames(resultData))
    
    if("gender = MALE"%in%colnames(resultData) & "gender = FEMALE"%in%colnames(resultData)){
        resultData$genderConceptId<-resultData[,grepl("gender = MALE",colnames(resultData))]*8507+resultData[,grepl("gender = FEMALE",colnames(resultData))]*8532
        #remove number of population with both genders
        resultData<-resultData[resultData$genderConceptId%in%c(8507,8532),]
        resultData<-resultData[,!grepl("gender =",colnames(resultData))]
    }
    return(resultData)
}


##Helper function (calculates integral)
f<-function(x,ageWeighting,C = 0.1658, beta = 0.04, discount, age){
    ageWeighting * C *x *exp(-beta*x)*exp(-discount*(x-age))+ (1-ageWeighting)*exp(-discount*(x-age))
}

##Burden calculation function
burden <- function(disabilityWeight,disabilityStartAge,duration,ageWeighting,discount,age){
    disabilityWeight * integrate(f, lower = disabilityStartAge, upper = disabilityStartAge+duration, ageWeighting=ageWeighting, discount=discount, age=age )$value
}