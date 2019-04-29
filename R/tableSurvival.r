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

#'make total survival rate table 
#'@import dplyr
#'@import reshape2
#'@param  totalSurvData              result of calculateSurvival code Agedivided = FALSE
#'@export
#'

tableSurvTotal <- function(totalSurvData){
    
    df_result <- data.frame()
    
    for(i in 8:10){
        df <- totalSurvData[,c(1,2,i)]
        df[,3] <- round(df[,3],3)
        df_cast <- reshape2::dcast(df,df$genderConceptId~df$startYear)
        df_castOut <- cbind(value = colnames(df)[3], df_cast)
        df_result <- rbind(df_result,df_castOut)
    }
    
    colnames(df_result)[2] <- "gender"
    df_result <- mutate(df_result, gender = case_when(gender == 8532 ~ 'women',
                                                      gender == 8507 ~ 'men') )

    return(df_result)
}
