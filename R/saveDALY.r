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

#'saving plot images which are the results DALY calculate
#'@import ggplot2
#'@export
saveDALY<-function(){
    
    outputFolder = outputFolder
    plotDALY = plotDALY
    plotDALYratio = plotDALYratio
    DALY = DALY
    imageExtension = "png"
    
    ifelse(!dir.exists(file.path(outputFolder, "DALY")), dir.create(file.path(outputFolder, "DALY")), print("saving in DALY folder"))
    DALYFolder<-paste(outputFolder, "DALY", sep = "/")
    
    ggsave(file.path(DALYFolder, paste0(cancerList$cohortName[[i]], "Cancer", "DALY", ".", imageExtension)), plotDALY, height = 10, width = 10, units = "cm")
    ggsave(file.path(DALYFolder, paste0(cancerList$cohortName[[i]], "Cancer", "DALY_ratio", ".", imageExtension)), plotDALYratio, height = 10, width = 10, units = "cm")
    write.csv(DALY,file.path(DALYFolder, paste0(cancerList$cohortName[[i]], "Cancer", "DALYresult", ".csv")) )
}
