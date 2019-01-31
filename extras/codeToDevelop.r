####Make a package skeleton####
#Make a package in the existing local git directory.
package.skeleton(name = "Argos", encoding = "UTF-8",path = file.path(Sys.getenv("gitFolder"),"ABMI"), force=TRUE)
#devtools::create_description()
#usethis::use_description()
roxygen2::roxygenise()

#####edit R environment to keep secrets####
#https://cran.r-project.org/web/packages/httr/vignettes/secrets.html
file.edit("~/.Renviron")
#Note that .Renviron is only processed on startup, so you’ll need to restart R to see changes.

####set import and suggest package####
usethis::use_package("dplyr")


####Write mid population csv file####
file.path(Sys.getenv("gitFolder"),"ABMI/Argos/inst/census/KOR_mid_year_population_old.csv")

df<-data.frame()
for(i in 3:length(basePopulation)){
    if(colnames(basePopulation)[i] %in% c("X100.","X80.") )next
    age<-as.numeric(sub("X","",colnames(basePopulation)[i]))
    
    
    tempDf<-data.frame(startYear = basePopulation[,c(1)], 
                       endYear = basePopulation[,c(1)],
                       genderConceptId = ifelse(basePopulation[,c(2)]=="male", 8507,8532), 
                       startAge = as.numeric(sub("X","",colnames(basePopulation)[i])),
                       endAge = as.numeric(sub("X","",colnames(basePopulation)[i])),
                       population = basePopulation[,i])
    df<-rbind(df,tempDf)
}
df<-na.omit(df)
df$country='KOR'
df$location='KOR'

write.csv(df,file.path(Sys.getenv("gitFolder"),"ABMI/Argos/inst/census/KOR_mid_year_population.csv"),row.names = FALSE)

####Write Life Expectancy csv file####
lifeExp<-read.csv(file.path(Sys.getenv("gitFolder"),"ABMI/Argos/extras/KOR_life_expectancy.csv"),stringsAsFactors=FALSE)
library(dplyr)

lifeExp<-lifeExp%>%filter(item=="expectation_of_life_at_age") %>%filter(gender !="both")
colnames(lifeExp)[1]<-"year"

df<-data.frame()
for(i in 4:length(lifeExp)){
    if(colnames(lifeExp)[i] %in% c("X100.","X80.") )next
    age<-as.numeric(sub("X","",colnames(lifeExp)[i]))
    
    
    tempDf<-data.frame(startYear = lifeExp[,c(1)], 
                       endYear = lifeExp[,c(1)],
                       genderConceptId = ifelse(lifeExp[,c(3)]=="male", 8507,8532), 
                       startAge = as.numeric(sub("X","",colnames(lifeExp)[i])),
                       endAge = as.numeric(sub("X","",colnames(lifeExp)[i])),
                       expectedLifeRemained = lifeExp[,i])
    df<-rbind(df,tempDf)
}
df$country='KOR'
df$location='KOR'

write.csv(df,file.path(Sys.getenv("gitFolder"),"ABMI/Argos/inst/census/KOR_life_expectancy.csv"),row.names = FALSE)
