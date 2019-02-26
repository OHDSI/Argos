####special Settings####
samplingPop = 0.02 ##sampling proportion (for NHIS-NSC : 0.02 , HIRA : 1)
startYearSetHIRA = list(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)
startYearSetNHIS = list(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013)
startYearSet=startYearSetNHIS

##Settings
cdmDatabaseSchema <-"NHIS_NSC.dbo"
vocabularyDatabaseSchema  <- "NHIS_NSC.dbo"
cohortDatabaseSchema <- "ONCOACHILLES.dbo"

cohortTable <- "argos_cohort"
outputFolder <- "D:/outputFolder"
options(fftempdir = "D:/fftemp")

survivalTime<-c(365,365*2,365*3,365*4,365*5)

connectionDetails<-DatabaseConnector::createConnectionDetails(dbms = 'sql server',
                                                              server = Sys.getenv("server53"),
                                                              user = Sys.getenv("userID"),
                                                              password = Sys.getenv("userPW"))

cancerList<-list(cohortId = c(1,2,3,4,5,6),
                 cohortName = c("colon", 'lung', 'stomach','breast','liver','thyroid'),
                 conceptIdSet = list(c(4089661,4180790,4180791,4180792,4181344,435754,443381,443382,443383,443384,443391),
                                     c(442139,4092217,4094876,4151250,4157333,258375),
                                     c(4149838,197803,4095320,4149837,4095319,4094856,4092061,443387,4095317),
                                     c(137809,4188544,4158563,4162253,4155292,4187850,4188545,432845#,81251
                                       ),
                                     c(201519,4001171,4001172,4001664,4003021,4095432,4246127),
                                     c(4178976)),
                 representConceptId = c())
outcomeId <- 99

conditionTypeConceptIds<-c(45756835,45756843,44786627)#primary diagnosis or first position diagnosis

#set base population
basePop<-loadMidYearPopulation('KOR')
basePop$population<-round(basePop$population*samplingPop,0)

#set reference population as population in 2007
refPop<-basePop[basePop$startYear==2007,]
refPop<-refPop[,c("startAge","endAge", "genderConceptId","population")]
colnames(refPop)[4]<-"standardPopulation"


#start log
ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))

#Connection
connection<-DatabaseConnector::connect(connectionDetails)
####Create cohort####
#create the cohort table
ParallelLogger::logInfo("Creating table for the cohorts")
sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CreateCohortTable.sql",
                                         packageName = "Argos",
                                         dbms = attr(connection,"dbms"),
                                         oracleTempSchema = oracleTempSchema,
                                         cohort_database_schema = cohortDatabaseSchema,
                                         cohort_table = cohortTable)
DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)

#create the target cohort
ParallelLogger::logInfo("Creating target cohorts")
for (i in seq(cancerList$cohortId)){
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "firstEventCohort.sql",
                                             packageName = "Argos",
                                             dbms = attr(connection,"dbms"),
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             vocabulary_database_schema = vocabularyDatabaseSchema,
                                             target_database_schema = cohortDatabaseSchema,
                                             target_cohort_table = cohortTable,
                                             include_descendant = F,
                                             prior_observation_period = 365,
                                             specific_condition_type = T,
                                             condition_type_concept_ids = paste0(conditionTypeConceptIds,collapse=","),
                                             condition_concept_ids = paste(cancerList$conceptIdSet[[i]],collapse=","),
                                             target_cohort_id = cancerList$cohortId[i])
    # fileCon<-file(file.path(outputFolder,"output.txt"))
    # writeLines(sql,fileCon)
    # close(fileCon)
    DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)
}

ParallelLogger::logInfo("Creating outcome cohort (any death)")
#create the outcome cohort (any death)
sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "anyDeath.sql",
                                         packageName = "Argos",
                                         dbms = attr(connection,"dbms"),
                                         oracleTempSchema = oracleTempSchema,
                                         cdm_database_schema = cdmDatabaseSchema,
                                         vocabulary_database_schema = vocabularyDatabaseSchema,
                                         target_database_schema = cohortDatabaseSchema,
                                         target_cohort_table = cohortTable,
                                         target_cohort_id = outcomeId)
# fileCon<-file(file.path(outputFolder,"output.txt"))
# writeLines(sql,fileCon)
# close(fileCon)
DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)


####Get incidence data####
##create setting for covariates
covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE, 
                                                                useDemographicsAge = TRUE
)
i<-4
for (i in seq(cancerList$cohortId)){
    ##get incidence Data
    incidenceData <- Argos::getIncidenceData(connectionDetails = connectionDetails, 
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             cohortDatabaseSchema = cohortDatabaseSchema,
                                             cohortTable = cohortTable,
                                             outcomeDatabaseSchema = cohortDatabaseSchema ,
                                             targetCohortId = cancerList$cohortId[i],
                                             minDateUnit = "year")
    saveRDS(incidenceData,file.path(outputFolder,paste0("incidenceData_cohortId_",cancerList$cohortId[i], ".rds" )))
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
    
    bybirthPlot<-Argos::PlotByBirthInc(incidencePropdata = incCal)
    ageSpePlot<-Argos::PlotByDiagnosisIncAgeS(incidencePropdata = incCal)
    ageAdjPlot<-Argos::PlotByDiagnosisIncAgeAd(incidencePropdata = incCal)
    Argos::saveIncidence(outputFolder,
                         imageExtension = "png")
}

####calculate the mortality####
for (i in seq(cancerList$cohortId)){
    for (j in seq(survivalTime)){
        ##get outcome Data
        outcomeData <- Argos::getOutcomeData(connectionDetails = connectionDetails, 
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             cohortDatabaseSchema = cohortDatabaseSchema,
                                             cohortTable = cohortTable,
                                             covariateSettings = covariateSettings,
                                             outcomeDatabaseSchema = cohortDatabaseSchema ,
                                             targetCohortId = cancerList$cohortId[[i]],
                                             outcomeId = outcomeId,
                                             requireTimeAtRisk = TRUE,
                                             riskWindowStart = 0,
                                             riskWindowEnd = survivalTime[j],
                                             removeSubjectsWithPriorOutcome = TRUE,
                                             minDateUnit = "year")
        saveRDS(outcomeData,file.path(outputFolder,paste0("OutcomeData_cohortId_",cancerList$cohortId[[i]],"survivalTime_",as.character(survivalTime[j]),".rds" )))
        
        #head(outcomeData$plpData$cohorts)
        #head(outcomeData$population)  
        
        
        outCal<-Argos::calculateOutcome(outcomeData=outcomeData,
                                        refPopulation = refPop,
                                        standardization = "direct",
                                        Agestandardization = TRUE,
                                        genderStandardization = TRUE,
                                        startYearStandardization = TRUE,
                                        AgeSet = list(20:29,30:39,
                                                      40:49,50:59,
                                                      60:69,70:79,
                                                      80:99),
                                        genderSet = list(8507,8532),
                                        startYearSet = startYearSet[seq(length(startYearSet)-j)],
                                        birthYearSet = list(1910:1919, 1920:1929,
                                                            1930:1939, 1940:1949,
                                                            1950:1959, 1960:1964, 
                                                            1965:1969, 1970:1974, 
                                                            1975:1979, 1980:1989))
        saveRDS(outCal,file.path(outputFolder,paste0("OutcomeCalData_cohortId_",cancerList$cohortId[[i]],"survivalTime_",as.character(survivalTime[j]),".rds" )))
        write.csv(outCal,file.path(outputFolder,paste0("OutcomeCalData_cohortId_",cancerList$cohortId[[i]],"survivalTime_",as.character(survivalTime[j]),".csv" )))
        
        PlotByBirthMort(mortalityPropdata = outCal,
                        title = paste(cancerList$cohortName[[i]],"Cancer", "MortalityPropbyBirth", survivalTime[j], "Duration", sep = " "),
                        outputFolder = outputFolder,
                        fileName = paste0(cancerList$cohortName[[i]],"Cancer", "MortalityPropbyBirth", "_", survivalTime[j], "Duration"),
                        imageExtension = "png")
        
        PlotByDiagnosisMort(mortalityPropdata = outCal,
                            ageSpetitle = paste(cancerList$cohortName[[i]],"Cancer", "MortalityProportion Age Specified", survivalTime[j], "Duration", sep = " "),
                            ageAdjtitle = paste(cancerList$cohortName[[i]],"Cancer", "MortalityProportion Age Adjusted", survivalTime[j], "Duration", sep = " "),
                            outputFolder = outputFolder,
                            ageSpefileName = paste0(cancerList$cohortName[[i]],"Cancer", "MortalityPropAgeSpe", "_", survivalTime[j], "Duration"),
                            ageAdjfileName = paste0(cancerList$cohortName[[i]],"Cancer", "MortalityPropAgeAdj", "_", survivalTime[j], "Duration"),
                            imageExtension = "png")
        
        
    }
}
i<-2
##Extract Cost Data
for (i in seq(cancerList$cohortId)){
    costMtData<-Argos::extractVisitCost(connectionDetails=connectionDetails, 
                                      cdmDatabaseSchema=cdmDatabaseSchema,
                                      cohortDatabaseSchema=cohortDatabaseSchema,
                                      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                      cohortTable=cohortTable,
                                      cohortId=cancerList$cohortId[[i]],
                                      costWindowStart = -60,
                                      costWindowEnd =365,
                                      minCostDateUnit = 'month',
                                      specifyCondition = FALSE,
                                      conditionConceptIds=paste0(cancerList$conceptIdSet[[i]],collapse=","))
    
    saveRDS(costMtData,file.path(outputFolder,paste0("costData_cohortId_",cancerList$cohortId[[i]],"costWindowEnd_","365",".rds" )))
    write.csv(costMtData,file.path(outputFolder,paste0("costData_cohortId_",cancerList$cohortId[[i]],"costWindowEnd_","365",".csv" )))
    
    plottotalCostperMt<-plotforCostPerMt(costData = costMtData)
        
    costYrData<-Argos::extractVisitCost(connectionDetails=connectionDetails, 
                                      cdmDatabaseSchema=cdmDatabaseSchema,
                                      cohortDatabaseSchema=cohortDatabaseSchema,
                                      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                      cohortTable=cohortTable,
                                      cohortId=cancerList$cohortId[[i]],
                                      costWindowStart = -60,
                                      costWindowEnd =365*5,
                                      minCostDateUnit = 'year',
                                      specifyCondition = FALSE,
                                      conditionConceptIds=paste0(cancerList$conceptIdSet[[i]],collapse=","))
    saveRDS(costYrData,file.path(outputFolder,paste0("costData_cohortId_",cancerList$cohortId[[i]],"costWindowEnd_","1825",".rds" )))
    write.csv(costYrData,file.path(outputFolder,paste0("costData_cohortId_",cancerList$cohortId[[i]],"costWindowEnd_","1825",".csv" )))

    PlottotalcostperYrdiv<-Argos::plotforCostPerYrdiv(costData = costYrData)
    plotperYrBarplotPayer<-Argos::plotforCostPerYrBarPay(costData = costYrData)
    plotperYrBarplotPatient<-Argos::plotforCostPerYrBarPat(costData = costMtData)
    
    Argos::savecost(outputFolder,
                    imageExtension = "png")
}

disabilityWeight <- loadDisabilityWeight('KOR',2012)
lifeExp <- loadLifeExpectancy('KOR')


