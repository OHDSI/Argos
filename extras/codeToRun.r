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
options(fftempdir = "D:/FFtemp")

survivalTime<-c(365,365*2,365*3,365*4,365*5)

connectionDetails<-DatabaseConnector::createConnectionDetails(dbms = 'sql server',
                                                              server = Sys.getenv("server53"),
                                                              user = Sys.getenv("userID"),
                                                              password = Sys.getenv("userPW"))

cancerList<-list(cohortId = c(1,2,3,4,5,6),
                 cohortName = c("colon", 'lung', 'stomach','breast','liver','thyroid'),
                 conceptIdSet = list(c(435754,443381,443382,443383,443384,443390,443391,4089661,4180790,4180791,4180792,4181344),
                                     #c(4089661,4180790,4180791,4180792,4181344,435754,443381,443382,443383,443384,443391),
                                     #c(4157333,4092217,4151250,442139),                
                                     c(442139,4092217,4094876,4151250,4157333,258375),
                                     #c(443387,4094856,4095319,4095320),
                                     c(4149838,197803,4095320,4149837,4095319,4094856,4092061,443387,4095317),
                                     #c(81251,432845,4158563,4162253               
                                     c(137809,4188544,4158563,4162253,4155292,4187850,4188545,432845#,81251
                                     ),
                                     #c(201519,4001171,4001172,4001664,4003021,4095432,4246127),
                                     c(201519,4001171,4001172,4001664,4003021,4095432,4246127),
                                     c(4178976)) ,
                 
                 representConceptId = c())
outcomeId <- 99

conditionTypeConceptIds<-c(45756835,45756843,44786627)#primary diagnosis or first position diagnosis

#set base population
basePop<-loadMidYearPopulation('KOR')
basePop$population<-round(basePop$population*samplingPop,0)

#set reference population as population in 2007
refPop<-basePop[basePop$startYear==2000,]
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
                                             #include_descendant = F,
                                             prior_observation_period = 365*2,
                                             #specific_condition_type = T,
                                             #condition_type_concept_ids = paste0(conditionTypeConceptIds,collapse=","),
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
#i<-1
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
    
    ##calculate the age adjusted incidence rate
    incCal<-calculateIncidence(incidenceData = incidenceData,
                               basePopulation = basePop,
                               refPopulation = refPop,
                               standardization = "direct",
                               Agestandardization = TRUE,
                               genderStandardization = TRUE,
                               startYearStandardization = TRUE,
                               AgeSet = list(0:4,5:9,10:14,15:19,20:24,25:29,30:34,35:39,40:44,
                                             45:49,50:54,55:59,60:64,65:69,70:74,75:79,80:84,85:100),
                               genderSet = list(8507,8532),
                               startYearSet = startYearSet,
                               birthYearSet = list(1910:2005))
    
    ageSpecified<-agespe(incCal)
    ageadjInc<-ageadjust(ageSpecified, alpha = 0.05) %>%
        arrange(genderConceptId, startYear)
    
    write.csv(ageadjInc, file.path(outputFolder, paste0("ageadjustedInc_cohortId_", cancerList$cohortId[[i]], ".csv")))
    saveRDS(ageadjInc,file.path(outputFolder,paste0("ageadjustedInc_cohortId_",cancerList$cohortId[i], ".rds" )))
    
    ##calculate the age specified incidence rate
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
    
    ageSpecifiedIncData <- agespe(incidencePropdata = incCal)
    birthcohortIncData<-bybirth(incidencePropdata = incCal)
    
    saveRDS(ageSpecifiedIncData,file.path(outputFolder,paste0("ageSpecifiedIncData_cohortId_",cancerList$cohortId[i], ".rds" )))
    write.csv(ageSpecifiedIncData,file.path(outputFolder,paste0("ageSpecifiedIncData_cohortId_",cancerList$cohortId[i], ".csv" )))
    saveRDS(birthcohortIncData,file.path(outputFolder,paste0("birthcohortIncData_cohortId_",cancerList$cohortId[i], ".rds" )))
    write.csv(birthcohortIncData,file.path(outputFolder,paste0("birthcohortIncData_cohortId_",cancerList$cohortId[i], ".csv" )))
    
    bybirthPlot<-Argos::PlotByBirthInc(birthcohortIncData = birthcohortIncData)
    ageSpePlot<-Argos::PlotByDiagnosisIncAgeS(agespecifiedIncData = ageSpecifiedIncData)
    ageAdjPlot<-Argos::PlotByDiagnosisIncAgeAd(ageadjustIncData = ageadjInc)
    Argos::saveIncidence(outputFolder,
                         bybirthPlot,
                         ageSpePlot,
                         ageAdjPlot,
                         imageExtension = "png")
}
#i<-2
###calculate the survival####
for (i in seq(cancerList$cohortId)){
    SurvData<-Argos::readySurvData(connectionDetails = connectionDetails, 
                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                   outcomeDatabaseSchema = cohortDatabaseSchema ,
                                   cohortTable = cohortTable,
                                   covariateSettings = covariateSettings,
                                   targetCohortId = cancerList$cohortId[i],
                                   outcomeId = outcomeId,
                                   requireTimeAtRisk = FALSE,
                                   riskWindowStart = 0,
                                   riskWindowEnd = 365*5,
                                   removeSubjectsWithPriorOutcome = TRUE,
                                   minDateUnit = "year")
    
    agedivSurvCal<-calculateSurvival(survivalData = SurvData,
                                     Agedivided = TRUE,
                                     AgeSet = list(30:39,
                                                   40:49,
                                                   50:59,
                                                   60:69,
                                                   70:79,
                                                   80:99),
                                     genderSet = list(8507,8532),
                                     startYearSet = startYearSet,
                                     observationEndYear = 2013)
    
    totalSurvCal<-calculateSurvival(survivalData = SurvData,
                                    Agedivided = FALSE,
                                    AgeSet = list(0:100),
                                    genderSet = list(8507,8532),
                                    startYearSet = startYearSet,
                                    observationEndYear = 2013)
    
    totalSurvCal_validation<-calculateSurvival(survivalData = SurvData,
                                               Agedivided = FALSE,
                                               AgeSet = list(0:100),
                                               genderSet = list(8507,8532),
                                               startYearSet = list(2004:2005,2006:2008),
                                               observationEndYear = 2013)
    
    saveRDS(agedivSurvCal,file.path(outputFolder,paste0("survivalData_cohortId_",cancerList$cohortId[[i]],".rds" )))
    write.csv(agedivSurvCal,file.path(outputFolder,paste0("survivalData_cohortId_",cancerList$cohortId[[i]],".csv" )))
    saveRDS(totalSurvCal,file.path(outputFolder,paste0("survivalData_Total_cohortId_",cancerList$cohortId[[i]],".rds" )))
    write.csv(totalSurvCal,file.path(outputFolder,paste0("survivalData_Total_cohortId_",cancerList$cohortId[[i]],".csv" )))
    saveRDS(totalSurvCal_validation,file.path(outputFolder,paste0("survivalData_Total_validation_cohortId_",cancerList$cohortId[[i]],".rds" )))
    write.csv(totalSurvCal_validation,file.path(outputFolder,paste0("survivalData_Total_validation_cohortId_",cancerList$cohortId[[i]],".csv" )))
    
    
    #plotting
    plot1yrsurvival<-plotSurvival1Yr(agedivSurvCal = agedivSurvCal)
    plot3yrsurvival<-plotSurvival3Yr(agedivSurvCal = agedivSurvCal)
    plot5yrsurvival<-plotSurvival5Yr(agedivSurvCal = agedivSurvCal)
    plottotalsurvival<-plotSurvivalTotal(totalSurvCal = totalSurvCal)

    Argos::saveSurvival(outputFolder,
                        plot1yrsurvival,
                        plot3yrsurvival,
                        plot5yrsurvival,
                        plottotalsurvival,
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
                                             requireTimeAtRisk = FALSE,
                                             riskWindowStart = 0,
                                             riskWindowEnd = survivalTime[j],
                                             removeSubjectsWithPriorOutcome = TRUE,
                                             minDateUnit = "year")
        saveRDS(outcomeData,file.path(outputFolder,paste0("OutcomeData_cohortId_",cancerList$cohortId[[i]],"survivalTime_",as.character(survivalTime[j]),".rds" )))
        
        #head(outcomeData$plpData$cohorts)
        #head(outcomeData$population)  
        
        
        # outCal<-Argos::calculateOutcome(outcomeData=outcomeData,
        #                                 refPopulation = refPop,
        #                                 standardization = "direct",
        #                                 Agestandardization = TRUE,
        #                                 genderStandardization = TRUE,
        #                                 startYearStandardization = TRUE,
        #                                 AgeSet = list(0:9,10:19,
        #                                               20:29,30:39,
        #                                               40:49,50:59,
        #                                               60:69,70:79,
        #                                               80:99),
        #                                 genderSet = list(8507,8532),
        #                                 startYearSet = startYearSet[seq(length(startYearSet)-j)],
        #                                 birthYearSet = list(1910:1919, 1920:1929,
        #                                                     1930:1939, 1940:1949,
        #                                                     1950:1959, 1960:1964, 
        #                                                     1965:1969, 1970:1974, 
        #                                                     1975:1979, 1980:1989))
        # saveRDS(outCal,file.path(outputFolder,paste0("OutcomeCalData_cohortId_",cancerList$cohortId[[i]],"survivalTime_",as.character(survivalTime[j]),".rds" )))
        # write.csv(outCal,file.path(outputFolder,paste0("OutcomeCalData_cohortId_",cancerList$cohortId[[i]],"survivalTime_",as.character(survivalTime[j]),".csv" )))
        
        # PlotByBirthMort(mortalityPropdata = outCal,
        #                 title = paste(cancerList$cohortName[[i]],"Cancer", "MortalityPropbyBirth", survivalTime[j], "Duration", sep = " "),
        #                 outputFolder = outputFolder,
        #                 fileName = paste0(cancerList$cohortName[[i]],"Cancer", "MortalityPropbyBirth", "_", survivalTime[j], "Duration"),
        #                 imageExtension = "png")
        # 
        # PlotByDiagnosisMort(mortalityPropdata = outCal,
        #                     ageSpetitle = paste(cancerList$cohortName[[i]],"Cancer", "MortalityProportion Age Specified", survivalTime[j], "Duration", sep = " "),
        #                     ageAdjtitle = paste(cancerList$cohortName[[i]],"Cancer", "MortalityProportion Age Adjusted", survivalTime[j], "Duration", sep = " "),
        #                     outputFolder = outputFolder,
        #                     ageSpefileName = paste0(cancerList$cohortName[[i]],"Cancer", "MortalityPropAgeSpe", "_", survivalTime[j], "Duration"),
        #                     ageAdjfileName = paste0(cancerList$cohortName[[i]],"Cancer", "MortalityPropAgeAdj", "_", survivalTime[j], "Duration"),
        #                     imageExtension = "png")
        
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
    
    PlottotalcostperYrdiv<-plotforCostPerYrdiv(costData = costYrData)
    plotperYrBarplotPayer<-plotforCostPerYrBarPay(costData = costYrData)
    plotperYrBarplotPatient<-plotforCostPerYrBarPat(costData = costYrData)

    Argos::savecost(outputFolder,
                    PlottotalcostperYrdiv,
                    plotperYrBarplotPayer,
                    plotperYrBarplotPatient,
                    imageExtension = "png")
}

##get DALY (disabilityweight has not been readied yet for livercancer)

for (i  in seq(cancerList$cohortId)){
    disabilityWeight <- loadDisabilityWeight('KOR',2012) %>%
        filter(condition_concept_id %in% cancerList$conceptIdSet[[i]]) 
    lifeExp <- loadLifeExpectancy('KOR')
    outcomeData <- Argos::getOutcomeData(connectionDetails = connectionDetails, 
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortTable = cohortTable,
                                         covariateSettings = covariateSettings,
                                         outcomeDatabaseSchema = cohortDatabaseSchema ,
                                         targetCohortId = cancerList$cohortId[[i]],
                                         outcomeId = outcomeId,
                                         requireTimeAtRisk = FALSE,
                                         riskWindowStart = 0,
                                         riskWindowEnd = 365*5,
                                         removeSubjectsWithPriorOutcome = TRUE,
                                         minDateUnit = "year")
    disabilityWeightData <- loadDisabilityWeight('KOR',2012) %>%
            filter(condition_concept_id %in% cancerList$conceptIdSet[[i]]) 
    disabilityWeight<-disabilityWeightData$disability_weight
    
    lifeExp <- loadLifeExpectancy('KOR')
    DALY<-calculateDALY(outcomeData,
                        refLifeExpectancy,
                        disabilityWeight=disabilityWeight,
                        outcomeDisabilityWeight = 1,
                        minTimeAtRisk=1,
                        observeStartYr = 2003,
                        observeEndYr = 2008,
                        discount = 0.3,
                        ageWeighting =TRUE,
                        outputFolder)
    saveRDS(DALY,file.path(outputFolder,paste0("DALY_cohortId_",cancerList$cohortId[[i]],".rds" )))
    write.csv(DALY,file.path(outputFolder,paste0("DALY_cohortId_",cancerList$cohortId[[i]],".csv" )))
    
    plotDALY<-plotforDALY(DALYdata = DALY)
    plotDALYratio<-plotforDALYratio(DALYdata = DALY)
    saveDALY(outputFolder,
             plotDALY = plotDALY,
             plotDALYratio = plotDALYratio,
             imageExtension = "png")
}
