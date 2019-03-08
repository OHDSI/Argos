i<-1
for (i in seq(cancerList$cohortId)){
    ##get incidence Data
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
    
    mortalityPop <- PatientLevelPrediction::createStudyPopulation(plpData = deathPlpData,
                                                                  outcomeId = cancerList$cohortId[i],
                                                                  firstExposureOnly = FALSE,
                                                                  requireTimeAtRisk = FALSE,
                                                                  removeSubjectsWithPriorOutcome = FALSE,
                                                                  verbosity = "DEBUG")
    
    
    mortalityData<- deathPlpData$cohorts %>%
        filter(subjectId %in% incidencePlpData$cohorts$subjectId)
    deathPlpData$covariates %>%
        filter(subjectId )
    mortalityResultData<-calculateNumberPerCovTime(plpData = mortalityData,
                                          population = NULL,
                                          minDateUnit = minDateUnit)
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
    
       
    deathData$data
    mortalityData <- Argos::getIncidenceData(connectionDetails = connectionDetails, 
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             cohortDatabaseSchema = cohortDatabaseSchema,
                                             cohortTable = cohortTable,
                                             outcomeDatabaseSchema = cohortDatabaseSchema ,
                                             targetCohortId = outcomeId,
                                             minDateUnit = "year")
    ##calculate the incidence
    incCal<-Argos::calculateIncidence(incidenceData = incidenceData,
                                      basePopulation = basePop,
                                      refPopulation = refPop,
                                      standardization = "direct",
                                      Agestandardization = TRUE,
                                      genderStandardization = TRUE,
                                      startYearStandardization = TRUE,
                                      AgeSet = list(30:39,
                                                    40:49,
                                                    50:59,
                                                    60:69,
                                                    70:79,
                                                    80:99),
                                      genderSet = list(8507,8532),
                                      startYearSet = startYearSet,
                                      birthYearSet = list(1910:1919, 1920:1929,
                                                          1930:1939, 1940:1949,
                                                          1950:1959, 1960:1964, 
                                                          1965:1969, 1970:1974, 
                                                          1975:1979, 1980:1989))
    saveRDS(incCal,file.path(outputFolder,paste0("incidenceCalData_cohortId_",cancerList$cohortId[i], ".rds" )))
    write.csv(incCal,file.path(outputFolder,paste0("incidenceCalData_cohortId_",cancerList$cohortId[i], ".csv" )))
    
    # bybirthPlot<-Argos::PlotByBirthInc(incidencePropdata = incCal)
    # ageSpePlot<-Argos::PlotByDiagnosisIncAgeS(incidencePropdata = incCal)
    # ageAdjPlot<-Argos::PlotByDiagnosisIncAgeAd(incidencePropdata = incCal)
    # Argos::saveIncidence(outputFolder,
    #                      bybirthPlot,
    #                      ageSpePlot,
    #                      ageAdjPlot,
    #                      imageExtension = "png")
}
PlotByBirthInc(incCal)
PlotByDiagnosisIncAgeS(incCal)
PlotByDiagnosisIncAgeAd(incCal)