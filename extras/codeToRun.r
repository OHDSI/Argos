##Settings
cdmDatabaseSchema <-""
vocabularyDatabaseSchema  <- cdmDatabaseSchema
cohortDatabaseSchema <- "ONCOACHILLES.dbo"
cohortTable <- "argos_cohort"
outputFolder <- "D:/onco_achilles"

survivalTime<-c(365,365*2,365*3,365*4,365*5)
i=1
j=1

connectionDetails<-DatabaseConnector::createConnectionDetails(dbms = 'sql server',
                                                              server = '',
                                                              user = '',
                                                              password = '')

cancerList<-list(cohortId = c(1,2,3,4,5,6),
                 cohortName = c("colon", 'lung', 'stomach','breast','liver','thyroid'),
                 conceptIdSet = list(c(4089661,4180790,4180791,4180792,4181344,435754,443381,443382,443383,443384,443391),
                                     c(442139,4092217,4094876,4151250,4157333,258375),
                                     c(4149838,197803,4095320,4149837,4095319,4094856,4092061,443387,4095317),
                                     c(137809,4188544,4158563,4162253,4155292,4187850,4188545,81251,432845),
                                     c(201519,4001171,4001172,4001664,4003021,4095432,4246127),
                                     c(4178976)))
outcomeId <- 99

#start log

ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))

#create the cohort table
connection<-DatabaseConnector::connect(connectionDetails)
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
for (i in cancerList$cohortId){
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "firstEventCohort.sql",
                                             packageName = "Argos",
                                             dbms = attr(connection,"dbms"),
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             vocabulary_database_schema = vocabularyDatabaseSchema,
                                             target_database_schema = cohortDatabaseSchema,
                                             target_cohort_table = cohortTable,
                                             include_descendant = F,
                                             outcome_ids = paste(cancerList$conceptIdSet[[i]],collapse=","),
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
                                         include_descendant = F,
                                         target_cohort_id = outcomeId)
# fileCon<-file(file.path(outputFolder,"output.txt"))
# writeLines(sql,fileCon)
# close(fileCon)
DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)

##get plp data

covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE, 
                                             useDemographicsAge = TRUE
                                             )
plpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails, 
                                              cdmDatabaseSchema = cdmDatabaseSchema,
                                              cohortDatabaseSchema = cohortDatabaseSchema,
                                              cohortTable = cohortTable,
                                              cohortId = cancerList$cohortId[i],
                                              covariateSettings = covariateSettings,
                                              outcomeDatabaseSchema = cohortDatabaseSchema,
                                              outcomeTable = cohortTable,
                                              outcomeIds = outcomeId,
                                              sampleSize = NULL)
PatientLevelPrediction::savePlpData(plpData,file.path(outputFolder,"plpData"))

population <- PatientLevelPrediction::createStudyPopulation(plpData = plpData, 
                                                            outcomeId = outcomeId,
                                                            washoutPeriod = 0,
                                                            firstExposureOnly = TRUE,
                                                            removeSubjectsWithPriorOutcome = FALSE,
                                                            priorOutcomeLookback = 99999,
                                                            riskWindowStart = 0,
                                                            riskWindowEnd = survivalTime[j],
                                                            addExposureDaysToStart = FALSE,
                                                            addExposureDaysToEnd = FALSE,
                                                            minTimeAtRisk = survivalTime[j]-1,
                                                            requireTimeAtRisk = TRUE,
                                                            includeAllOutcomes = TRUE,
                                                            verbosity = "DEBUG")