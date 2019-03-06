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

#'DALY value plot according to diagnosis year
#'@param DALYdata   outcome of Argos package DALY code 
#'@import dplyr
#'@import ggplot2
#'@export
plotforDALY<-function(DALYdata){
    DALYdata$startYear<-as.numeric(row.names(DALYdata))
    DALYdata <-DALYdata %>%
        mutate( yllSumPop = yllSum*(1/(samplingPop*500)),
                yldSumPop = yldSum*(1/(samplingPop*500)),
                dalySumPop = dalySum*(1/(samplingPop*500))) %>%
        select( startYear, yllSumPop, yldSumPop )
    DALYforplot<-reshape2::melt(data = DALYdata, id = "startYear") %>%
        mutate(variable = factor(variable, levels = c("yllSumPop", "yldSumPop"), labels = c("YLL", "YLD")))
    
    DALYplot<- ggplot(data = DALYforplot, aes(x = as.factor(startYear), y = value, fill = variable))+
        geom_bar(stat = "identity", width = .5)+
        ggplot2::xlab("diagnosis year")+
        ggplot2::ylab("DALY (YLL + YLD) per 100,000 persons")+
        ggplot2::ggtitle(paste(cancerList$cohortName[[i]], "Cancer DALY (YLL + YLD) according to diagnosis year", sep = " "))+
        ggplot2::theme(legend.title = element_blank(),
                       legend.text = element_text(size = 15),
                       plot.title = element_text(size = 17),
                       axis.text.x = element_text(size = 12),
                       axis.title.x = element_text(size = 15),
                       axis.text.y = element_text(size = 12),
                       axis.title.y = element_text(size = 15))+
        ggplot2::theme_bw()
        
    return(DALYplot)
        
}

#'YLL:YLD ratio according to diagnosis year
#'@param DALYdata   outcome of Argos package DALY code 
#'@import dplyr
#'@import ggplot2
#'@export
plotforDALYratio<-function(DALYdata){
    DALYdata$startYear<-as.numeric(row.names(DALYdata))
    DALYdata <-DALYdata %>%
        mutate( yllSumPop = yllSum*(1/(samplingPop*500)),
                yldSumPop = yldSum*(1/(samplingPop*500)),
                dalySumPop = dalySum*(1/(samplingPop*500))) %>%
        select( startYear, yllSumPop, yldSumPop )
    DALYforplot<-reshape2::melt(data = DALYdata, id = "startYear") %>%
        mutate(variable = factor(variable, levels = c("yllSumPop", "yldSumPop"), labels = c("YLL", "YLD")))
    
    DALYratioplot<- ggplot(data = DALYforplot, aes(x = as.factor(startYear), y = value, fill = variable))+
        geom_bar(stat = "identity", position = "fill", width = .5)+
        ggplot2::xlab("diagnosis year")+
        ggplot2::ylab("YLL:YLD ratio")+
        ggplot2::ggtitle(paste(cancerList$cohortName[[i]], "Cancer YLL:YLD ratio according to diagnosis year", sep = " "))+
        ggplot2::theme(legend.title = element_blank(),
                       legend.text = element_text(size = 15),
                       plot.title = element_text(size = 17),
                       axis.text.x = element_text(size = 12),
                       axis.title.x = element_text(size = 15),
                       axis.text.y = element_text(size = 12),
                       axis.title.y = element_text(size = 15))+
        ggplot2::theme_bw()
    
    return(DALYratioplot)
    
}
