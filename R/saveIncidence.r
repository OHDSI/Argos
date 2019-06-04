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
saveIncidence<-function(){
    
    outputFolder = outputFolder
    bybirthPlot = bybirthPlot
    ageSpePlot = ageSpePlot
    ageAdjPlot = ageAdjPlot
    bybirthTable = bybirthTable
    ageSpeTable = ageSpeTable
    ageAdjTable = ageAdjTable
    diseaseList = diseaseList
    imageExtension = "png"
    
    ifelse(!dir.exists(file.path(outputFolder, "incidence")), dir.create(file.path(outputFolder, "incidence")), print("saving in incidence folder"))
    incidenceFolder<-paste(outputFolder, "incidence", sep = "/")
    
    ggplot2::ggsave(file.path(incidenceFolder, paste0(paste0(diseaseList$cohortName[[i]], "IncidenceProporByBirthyr"),".",imageExtension) ), bybirthPlot, width = 25,height = 12,units = "cm" )  
    ggplot2::ggsave(file.path(incidenceFolder, paste0(paste0(diseaseList$cohortName[[i]], "IncidencePropAgeSpe"),".",imageExtension) ),ageSpePlot,  width = 25,height = 12,units = "cm" ) 
    ggplot2::ggsave(file.path(incidenceFolder, paste0(paste0(diseaseList$cohortName[[i]], "IncidencePropAgeAdj"),".",imageExtension) ), ageAdjPlot, width = 25,height = 12,units = "cm" ) 
    write.csv(bybirthTable, file.path(incidenceFolder, paste0(paste0(diseaseList$cohortName[[i]], "IncidenceProporByBirthyr"),".csv") ) )
    write.csv(ageSpeTable, file.path(incidenceFolder, paste0(paste0(diseaseList$cohortName[[i]], "IncidencePropAgeSpe"),".csv") ) )
    write.csv(ageAdjTable, file.path(incidenceFolder, paste0(paste0(diseaseList$cohortName[[i]], "IncidencePropAgeAdj"),".csv") ) )
}
