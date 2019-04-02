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
#'@param ageAdjustedInc  
#'@import dplyr
#'@export
ageAdjAPC<-function(ageAdjustedInc = ageadjInc){

    ageadjSplit <- split(ageAdjustedInc, ageAdjustedInc$genderConceptId)
    
    ageadj_APC<-lapply(ageadjSplit, FUN = function(x){
        
        lm <- x %>%
            lm(formula = log(AgeadjProp)~startYear)
        slope <- summary(lm)$coefficients["startYear","Estimate"]
        p_value <- summary(lm)$coefficients["startYear","Pr(>|t|)"]
        APC <- (exp(slope)-1)*100
        
        result<-data.frame(slope = slope,
                           p_value = p_value,
                           APC = APC)
        
        return(result)
    })
    
    return(ageadj_APC)
}
