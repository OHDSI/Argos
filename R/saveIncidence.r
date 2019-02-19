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
#'@import ggplot2
#'@export
## save incidence talbe and incidence proportion plot

saveIncidence<-function(outputFolder,
                      imageExtension = "png"){
    dir.create(file.path(outputFolder, "incidence"))
    incidenceFolder<-paste(outputFolder, "incidence", sep = "/")
    
    saveRDS(incCal,file.path(incidenceFolder,paste0("incidenceCalData_cohortId_",cancerList$cohortId[i], ".rds" )))
    write.csv(incCal,file.path(incidenceFolder,paste0("incidenceCalData_cohortId_",cancerList$cohortId[i], ".csv" )))
    
    ggplot2::ggsave(file.path(incidenceFolder, paste0(paste0(cancerList$cohortName[[i]],"Cancer", "IncidenceProporByBirthyr"),".",imageExtension) ), bybirthPlot, width = 30,height = 15,units = "cm" )  
    ggplot2::ggsave(file.path(incidenceFolder, paste0(paste0(cancerList$cohortName[[i]],"Cancer", "IncidencePropAgeSpe"),".",imageExtension) ),ageSpePlot,  width = 30,height = 15,units = "cm" ) 
    ggplot2::ggsave(file.path(incidenceFolder, paste0(paste0(cancerList$cohortName[[i]],"Cancer", "IncidencePropAgeAdj"),".",imageExtension) ), ageAdjPlot, width = 30,height = 15,units = "cm" ) 
}