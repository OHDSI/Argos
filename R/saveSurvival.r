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
#'saving tables and plot image which are the results incidence analysis

#'@import ggplot2
#'@export
#'
saveSurvival<-function(){
    
    outputFolder = outputFolder
    plot1yrsurvival = plot1yrsurvival
    plot3yrsurvival = plot3yrsurvival
    plot5yrsurvival = plot5yrsurvival
    plottotalsurvival = plottotalsurvival 
    tableSurvivalRate = tableSurvivalRate
    imageExtension = "png"
    
    ifelse(!dir.exists(file.path(outputFolder, "survival")), dir.create(file.path(outputFolder, "survival")), print("saving in survival folder"))
    survivalFolder<-paste(outputFolder, "survival", sep = "/")
    
    # saveRDS(agedivSurvCal,file.path(survivalFolder,paste0("survivalData_cohortId_",cancerList$cohortId[[i]],".rds" )))
    # write.csv(agedivSurvCal,file.path(survivalFolder,paste0("survivalData_cohortId_",cancerList$cohortId[[i]],".csv" )))
    # saveRDS(totalSurvCal,file.path(survivalFolder,paste0("survivalData_Total_cohortId_",cancerList$cohortId[[i]],".rds" )))
    # write.csv(totalSurvCal,file.path(survivalFolder,paste0("survivalData_Total_cohortId_",cancerList$cohortId[[i]],".csv" )))
    
    ggplot2::ggsave(file.path(survivalFolder, paste0(paste0(cancerList$cohortName[[i]],"Cancer", "1YrSurvivalRate"),".",imageExtension) ), plot1yrsurvival, width = 30,height = 10,units = "cm" )  
    ggplot2::ggsave(file.path(survivalFolder, paste0(paste0(cancerList$cohortName[[i]],"Cancer", "3YrSurvivalRate"),".",imageExtension) ), plot3yrsurvival, width = 30,height = 10,units = "cm" ) 
    ggplot2::ggsave(file.path(survivalFolder, paste0(paste0(cancerList$cohortName[[i]],"Cancer", "5YrSurvivalRate"),".",imageExtension) ), plot5yrsurvival, width = 30,height = 10,units = "cm" ) 
    ggplot2::ggsave(file.path(survivalFolder, paste0(paste0(cancerList$cohortName[[i]],"Cancer", "TotalSurvivalRate"),".",imageExtension) ), plottotalsurvival, width = 30,height = 10,units = "cm" )
    write.csv(tableSurvivalRate, file.path(survivalFolder, paste0(paste0(cancerList$cohortName[[i]],"Cancer", "TotalSurvivalRate"),".csv") ))
}

