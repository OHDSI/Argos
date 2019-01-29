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
