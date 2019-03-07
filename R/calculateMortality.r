
for (i in seq(cancerList$cohortId)){
    ##get incidence Data
    i<-1
    cancerList$cohortId[[i]]
    incidencePlpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails, 
                                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                                        cohortDatabaseSchema = cohortDatabaseSchema,
                                                        cohortTable = cohortTable,
                                                        cohortId = targetCohortId,
                                                        covariateSettings = covariateSettings,
                                                        outcomeDatabaseSchema = cohortDatabaseSchema,
                                                        outcomeTable = cohortTable,
                                                        outcomeIds = targetCohortId,
                                                        sampleSize = NULL)
    deathPlpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails, 
                                                       cdmDatabaseSchema = cdmDatabaseSchema,
                                                       cohortDatabaseSchema = cohortDatabaseSchema,
                                                       cohortTable = cohortTable,
                                                       cohortId = outcomeId,
                                                       covariateSettings = covariateSettings,
                                                       outcomeDatabaseSchema = cohortDatabaseSchema,
                                                       outcomeTable = cohortTable,
                                                       outcomeIds = outcomeId,
                                                       sampleSize = NULL)
    
    mortalitycohorts<- deathPlpData$cohorts %>%
        filter(subjectId %in% incidencePlpData$cohorts$subjectId)
    mortalitycovariates<-ff::as.ram(deathPlpData$covariates) %>% 
        inner_join(mortalitycohorts, by = "rowId") %>%
        select(rowId, covariateId, covariateValue)
    mortalitycovariateRef<- deathPlpData$covariateRef
    
    df<-list(cohorts = mortalitycohorts,
                   covariates = mortalitycovariates,
                   covariateRef = mortalitycovariateRef)
    nrow(deathPlpData$covariates)
        filter(subjectId )
    mortalityResultData<-calculateNumberPerCovTime(plpData = df,
                                                   population = NULL,
                                                   minDateUnit = minDateUnit)
    sum(mortalityResultData$aggregatedNum)
    deathResultData<-calculateNumberPerCovTime(plpData = deathPlpData,
                                               population = NULL,
                                               minDateUnit = minDateUnit)
    
    incidenceData <- Argos::getIncidenceData(connectionDetails = connectionDetails, 
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             cohortDatabaseSchema = cohortDatabaseSchema,
                                             cohortTable = cohortTable,
                                             outcomeDatabaseSchema = cohortDatabaseSchema ,
                                             targetCohortId = cancerList$cohortId[i],
                                             minDateUnit = "year")
    
    deathData <- Argos::getIncidenceData(connectionDetails = connectionDetails, 
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortTable = cohortTable,
                                         outcomeDatabaseSchema = cohortDatabaseSchema ,
                                         targetCohortId = outcomeId,
                                         minDateUnit = "year")
    
    
}