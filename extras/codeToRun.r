##Settings
cdmDatabaseSchema <-Sys.getenv("NHIS_DB")
vocabularyDatabaseSchema  <- cdmDatabaseSchema
cohortDatabaseSchema <- "ONCOACHILLES.dbo"
cohortTable <- "argos_cohort"
outputFolder <- "D:/onco_achilles"
options(fftempdir = Sys.getenv("local_fftempdir"))

survivalTime<-c(365,365*2,365*3,365*4,365*5)
i=1
j=1

connectionDetails<-DatabaseConnector::createConnectionDetails(dbms = 'sql server',
                                                              server = Sys.getenv("server53"),
                                                              user = Sys.getenv("userID"),
                                                              password = Sys.getenv("userPW"))

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


####Create cohort####
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


####Get incidence data####
##create setting for covariates
covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE, 
                                                                useDemographicsAge = TRUE
)
i=1
##get incidence Data
incidenceData <- Argos::getIncidenceData(connectionDetails = connectionDetails, 
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortTable = cohortTable,
                                         covariateSettings = covariateSettings,
                                         outcomeDatabaseSchema = cohortDatabaseSchema ,
                                         cohortId = cancerList$cohortId[i],
                                         minDateUnit = "year")

basePop<-loadMidYearPopulation('KOR')
basePop$population<-round(basePop$population*0.02,0)

#set reference population as population in 2007
refPop<-basePop[basePop$startYear==2007,]
refPop<-refPop[,c("startAge","endAge", "genderConceptId","population")]
colnames(refPop)[4]<-"standardPopulation"


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
                                  startYearSet = list(2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012),
                                  birthYearSet = list(1910:1919, 1920:1929,
                                                     1930:1939, 1940:1949,
                                                     1950:1959, 1960:1964, 
                                                     1965:1969, 1970:1974, 
                                                     1975:1979, 1980:1989))

####calculate the mortality####

##get outcome Data
outcomeData <- Argos::getOutcomeData(connectionDetails = connectionDetails, 
                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                     cohortDatabaseSchema = cohortDatabaseSchema,
                                     cohortTable = cohortTable,
                                     covariateSettings = covariateSettings,
                                     outcomeDatabaseSchema = cohortDatabaseSchema ,
                                     cohortId = cancerList$cohortId[i],
                                     outcomeId = outcomeId,
                                     requireTimeAtRisk = TRUE,
                                     riskWindowStart = 0,
                                     riskWindowEnd = 365,
                                     removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
                                     minDateUnit = "year")

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
                                startYearSet = list(2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012),
                                birthYearSet = list(1910:1919, 1920:1929,
                                                    1930:1939, 1940:1949,
                                                    1950:1959, 1960:1964, 
                                                    1965:1969, 1970:1974, 
                                                    1975:1979, 1980:1989)
                                )

outCal