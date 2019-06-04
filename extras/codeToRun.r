####special Settings####
samplingPop = 0.02 ##sampling proportion (for NHIS-NSC : 0.02 , HIRA : 1)
startYearSetHIRA = list(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)
startYearSetNHIS = list(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013)
startYearSet=startYearSetNHIS

#Settings
cdmDatabaseSchema <-"NHIS_NSC.dbo"
vocabularyDatabaseSchema  <- "NHIS_NSC.dbo"
cohortDatabaseSchema <- "ONCOACHILLES.dbo"

cohortTable <- "argos_cohort"
outputFolder <- "C:/Users/youjin/Documents/project/output"
options(fftempdir = "C:/Users/youjin/Documents/project/FFtemp")
Sys.setlocale(category = "LC_ALL", locale = "us")
survivalTime<-c(365,365*2,365*3,365*4,365*5)

connectionDetails<-DatabaseConnector::createConnectionDetails(dbms = 'sql server',
                                                              server = Sys.getenv("server53"),
                                                              user = Sys.getenv("userID"),
                                                              password = Sys.getenv("userPW"))
diseaseList <- psychoticList
#########set disease set####################################################################
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
psychoticList<-list(cohortId = c(50),
                    cohortName = c("schizophrenia"),
                    conceptIdSet = list(c(435218,40321760,45463320,45463350,45486775,45519906,45920699,45953562,35207128,45426627,45429978,45436594,45436596,45531922,45552471,438724,4103253,35207126,40321759,40338421,44824113,45456546,45503241,45513269,45516677,45523212,436947,4102659,40391473,44825289,45466666,40356766,44832230,45453173,45523215,45914555,45954145,439004,4194671,40391485,40589679,45480122,45925010,45941586,44828825,45516709,45547694,45616917,35207131,40321764,44797232,44820713,44829920,44835787,45490030,45913841,45918355,45938794,45940164,40391881,45466708,45516679,45921820,4101152,4105330,44821816,44832225,435783,45531921,4102658,44826496,45423364,45914578,45940663,45950240,432299,4100368,44825291,44832226,45470110,45936386,436673,45429977,45439919,45493310,45591128,45908089,45943703,436944,4213979,44820712,44832227,45490029,434332,44822971,44827651,45453172,45466710,45619321,45955684,435219,4244059,4310121,35207129,40387836,44822970,44832223,45509872,45926880,45936758,434318,441538,44826497,44829918,44832224,45480123,45496635,45908908,45921740,45923548,45937009,433996,435782,439275,45525921,45528894,45945387,44821819,44831086,45483406,45523241,45927290,45956004,1568207,4101151,4321694,35207127,44813605,45429979,45439889,45926882,45927082,40391490,40391907,44826493,44827646,45918404,45943089,436385,40391882,44835779,44835780,45513300,45547696,45595896,44836963,440373,40387832,44822969,44825290,44829921,45576533,45449844,45466709,45516678,45933086,40321763,44834588,45473404,45908664,45932353,45956003,4102661,45423399,45480121,45926881,45926954,45955400,45955659,4008566,44826494,45430016,45581445,436384,44828826,45910340,435217,436067,4103251,45523213,4103252,45426664,45926182,45927468,45955298,4085662,45490028,45955019,4100366,44820715,44831084,45430017,436071,4102662,40321761,44826491,45496595,45930222,433450,440368,4285597,40321762,40387835,40391486,45423367,45523214,437243,4103255,44821818,45493308,45526918,45616916,35207130,40356765,40391468,44819538,45426625,45493309,45513270,45614516,45945556,433442,4219539,40480879,44831085,44834586,45513271,4100365,40387841,40391885,40421358,45433287,45499928,45944973,44824112,45466707,45921819,45938418,4098177,40387829,44821815,44826495,45420221,45920691,438399,4100367,40387830,44827647,45463321,45581447,45918403,45925591,432864,4098176,44826492,45908088,45931979,45948064,434901,441828,40637452,44828829,44835781,45426626,45542835,45908964,45926891,1568206,4101150,44824109,44825288,45931913,45954146,432598,440686,444396,4103254,4152049,44833399,45429976,45496594,45516707,45921726,433990,4102660,45496593,45533093,45927291)
                    ) ,
                    representConceptId = c())
outcomeId <- 99

conditionTypeConceptIds<-c(45756835,45756843,44786627)#primary diagnosis or first position diagnosis
################################################################################

#set base population
basePop<-loadMidYearPopulation('KOR')
basePop$population<-round(basePop$population*samplingPop,0)

#set reference population as population in 2007
refPop<-basePop[basePop$startYear==2000,]
refPop<-refPop[,c("startAge","endAge", "genderConceptId","population")]
colnames(refPop)[4]<-"standardPopulation"

#set expected survival rate
expSurv<-loadSurvivalExpectancy('KOR')
#start log
ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))

####Connection
connection<-DatabaseConnector::connect(connectionDetails)
####Create cohort
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
for (i in seq(diseaseList$cohortId)){
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "firstEventCohort.sql",
                                             packageName = "Argos",
                                             dbms = attr(connection,"dbms"),
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             vocabulary_database_schema = vocabularyDatabaseSchema,
                                             target_database_schema = cohortDatabaseSchema,
                                             target_cohort_table = cohortTable,
                                             #include_descendant = F,
                                             prior_observation_period = 365,
                                             #specific_condition_type = T,
                                             #condition_type_concept_ids = paste0(conditionTypeConceptIds,collapse=","),
                                             condition_concept_ids = paste(diseaseList$conceptIdSet[[i]],collapse=","),
                                             target_cohort_id = diseaseList$cohortId[i])
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
for (i in seq(diseaseList$cohortId)){
    ##get incidence Data
    incidenceData <- Argos::getIncidenceData(connectionDetails = connectionDetails, 
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             cohortDatabaseSchema = cohortDatabaseSchema,
                                             cohortTable = cohortTable,
                                             outcomeDatabaseSchema = cohortDatabaseSchema ,
                                             targetCohortId = diseaseList$cohortId[i],
                                             minDateUnit = "year")
    # saveRDS(incidenceData,file.path(outputFolder,paste0("incidenceData_cohortId_",diseaseList$cohortId[i], ".rds" )))
    
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
    
    ageSpecified <- agespe(incCal)
    ageadjInc <- ageadjust(ageSpecified, alpha = 0.05) 
    ageAdjTable <- tableAgeAdjusted(ageadjIncData = ageadjInc)
    
    # write.csv(ageadjInc, file.path(outputFolder, paste0("ageadjustedInc_cohortId_", diseaseList$cohortId[[i]], ".csv")))
    # saveRDS(ageadjInc,file.path(outputFolder,paste0("ageadjustedInc_cohortId_",diseaseList$cohortId[i], ".rds" )))
    
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
    ageSpeTable <- tableAgeSpecified(ageSpecifiedData = ageSpecifiedIncData)
    birthcohortIncData<-bybirth(incidencePropdata = incCal)
    bybirthTable <- tablebirthCohortInc(birthIncData = birthcohortIncData)
    # saveRDS(ageSpecifiedIncData,file.path(outputFolder,paste0("ageSpecifiedIncData_cohortId_",diseaseList$cohortId[i], ".rds" )))
    # write.csv(ageSpecifiedIncData,file.path(outputFolder,paste0("ageSpecifiedIncData_cohortId_",diseaseList$cohortId[i], ".csv" )))
    # saveRDS(birthcohortIncData,file.path(outputFolder,paste0("birthcohortIncData_cohortId_",diseaseList$cohortId[i], ".rds" )))
    # write.csv(birthcohortIncData,file.path(outputFolder,paste0("birthcohortIncData_cohortId_",diseaseList$cohortId[i], ".csv" )))
     
    bybirthPlot<-PlotByBirthInc(birthcohortIncData = birthcohortIncData)
    ageSpePlot<-PlotByDiagnosisIncAgeS(agespecifiedIncData = ageSpecifiedIncData)
    ageAdjPlot<-PlotByDiagnosisIncAgeAd(ageadjustIncData = ageadjInc)
    
    saveIncidence()
}
#i<-1
###calculate the survival####
for (i in seq(diseaseList$cohortId)){
    SurvData<-Argos::readySurvData(connectionDetails = connectionDetails, 
                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                   outcomeDatabaseSchema = cohortDatabaseSchema ,
                                   cohortTable = cohortTable,
                                   covariateSettings = covariateSettings,
                                   targetCohortId = diseaseList$cohortId[i],
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
    
    # saveRDS(agedivSurvCal,file.path(outputFolder,paste0("survivalData_cohortId_",diseaseList$cohortId[[i]],".rds" )))
    # write.csv(agedivSurvCal,file.path(outputFolder,paste0("survivalData_cohortId_",diseaseList$cohortId[[i]],".csv" )))
    # saveRDS(totalSurvCal,file.path(outputFolder,paste0("survivalData_Total_cohortId_",diseaseList$cohortId[[i]],".rds" )))
    # write.csv(totalSurvCal,file.path(outputFolder,paste0("survivalData_Total_cohortId_",diseaseList$cohortId[[i]],".csv" )))
    # saveRDS(totalSurvCal_validation,file.path(outputFolder,paste0("survivalData_Total_validation_cohortId_",diseaseList$cohortId[[i]],".rds" )))
    # write.csv(totalSurvCal_validation,file.path(outputFolder,paste0("survivalData_Total_validation_cohortId_",diseaseList$cohortId[[i]],".csv" )))
    
    #table
    tableSurvivalRate <- tableSurvTotal(totalSurvData = totalSurvCal)
    
    #plotting
    plot1yrsurvival<-plotSurvival1Yr(agedivSurvCal = agedivSurvCal)
    plot3yrsurvival<-plotSurvival3Yr(agedivSurvCal = agedivSurvCal)
    plot5yrsurvival<-plotSurvival5Yr(agedivSurvCal = agedivSurvCal)
    plottotalsurvival<-plotSurvivalTotal(totalSurvCal = totalSurvCal)

    saveSurvival()
}

####calculate the mortality####
for (i in seq(diseaseList$cohortId)){
    for (j in seq(survivalTime)){
        ##get outcome Data
        outcomeData <- Argos::getOutcomeData(connectionDetails = connectionDetails, 
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             cohortDatabaseSchema = cohortDatabaseSchema,
                                             cohortTable = cohortTable,
                                             covariateSettings = covariateSettings,
                                             outcomeDatabaseSchema = cohortDatabaseSchema ,
                                             targetCohortId = diseaseList$cohortId[[i]],
                                             outcomeId = outcomeId,
                                             requireTimeAtRisk = FALSE,
                                             riskWindowStart = 0,
                                             riskWindowEnd = survivalTime[j],
                                             removeSubjectsWithPriorOutcome = TRUE,
                                             minDateUnit = "year")
        saveRDS(outcomeData,file.path(outputFolder,paste0("OutcomeData_cohortId_",diseaseList$cohortId[[i]],"survivalTime_",as.character(survivalTime[j]),".rds" )))
        
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
        # saveRDS(outCal,file.path(outputFolder,paste0("OutcomeCalData_cohortId_",diseaseList$cohortId[[i]],"survivalTime_",as.character(survivalTime[j]),".rds" )))
        # write.csv(outCal,file.path(outputFolder,paste0("OutcomeCalData_cohortId_",diseaseList$cohortId[[i]],"survivalTime_",as.character(survivalTime[j]),".csv" )))
        
        # PlotByBirthMort(mortalityPropdata = outCal,
        #                 title = paste(diseaseList$cohortName[[i]],"Cancer", "MortalityPropbyBirth", survivalTime[j], "Duration", sep = " "),
        #                 outputFolder = outputFolder,
        #                 fileName = paste0(diseaseList$cohortName[[i]],"Cancer", "MortalityPropbyBirth", "_", survivalTime[j], "Duration"),
        #                 imageExtension = "png")
        # 
        # PlotByDiagnosisMort(mortalityPropdata = outCal,
        #                     ageSpetitle = paste(diseaseList$cohortName[[i]],"Cancer", "MortalityProportion Age Specified", survivalTime[j], "Duration", sep = " "),
        #                     ageAdjtitle = paste(diseaseList$cohortName[[i]],"Cancer", "MortalityProportion Age Adjusted", survivalTime[j], "Duration", sep = " "),
        #                     outputFolder = outputFolder,
        #                     ageSpefileName = paste0(diseaseList$cohortName[[i]],"Cancer", "MortalityPropAgeSpe", "_", survivalTime[j], "Duration"),
        #                     ageAdjfileName = paste0(diseaseList$cohortName[[i]],"Cancer", "MortalityPropAgeAdj", "_", survivalTime[j], "Duration"),
        #                     imageExtension = "png")
        
    }
}
i<-1
####Extract Cost Data####
for (i in seq(diseaseList$cohortId)){
    costMtData<-Argos::extractVisitCost(connectionDetails=connectionDetails, 
                                        cdmDatabaseSchema=cdmDatabaseSchema,
                                        cohortDatabaseSchema=cohortDatabaseSchema,
                                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                        cohortTable=cohortTable,
                                        cohortId=diseaseList$cohortId[[i]],
                                        costWindowStart = -60,
                                        costWindowEnd =365,
                                        minCostDateUnit = 'month',
                                        specifyCondition = FALSE,
                                        conditionConceptIds=paste0(diseaseList$conceptIdSet[[i]],collapse=","))
    
    # saveRDS(costMtData,file.path(outputFolder,paste0("costData_cohortId_",diseaseList$cohortId[[i]],"costWindowEnd_","365",".rds" )))
    # write.csv(costMtData,file.path(outputFolder,paste0("costData_cohortId_",diseaseList$cohortId[[i]],"costWindowEnd_","365",".csv" )))
    
    plottotalCostperMt<-plotforCostPerMt(costData = costMtData)
    
    costYrData<-Argos::extractVisitCost(connectionDetails=connectionDetails, 
                                        cdmDatabaseSchema=cdmDatabaseSchema,
                                        cohortDatabaseSchema=cohortDatabaseSchema,
                                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                        cohortTable=cohortTable,
                                        cohortId=diseaseList$cohortId[[i]],
                                        costWindowStart = -60,
                                        costWindowEnd =365*5,
                                        minCostDateUnit = 'year',
                                        specifyCondition = FALSE,
                                        conditionConceptIds=paste0(diseaseList$conceptIdSet[[i]],collapse=","))
    # saveRDS(costYrData,file.path(outputFolder,paste0("costData_cohortId_",diseaseList$cohortId[[i]],"costWindowEnd_","1825",".rds" )))
    # write.csv(costYrData,file.path(outputFolder,paste0("costData_cohortId_",diseaseList$cohortId[[i]],"costWindowEnd_","1825",".csv" )))
    
    PlottotalcostperYrdiv<-plotforCostPerYrdiv(costData = costYrData)
    plotperYrBarplotPayer<-plotforCostPerYrBarPay(costData = costYrData)
    plotperYrBarplotPatient<-plotforCostPerYrBarPat(costData = costYrData)

    Argos::savecost(outputFolder,
                    PlottotalcostperYrdiv,
                    plotperYrBarplotPayer,
                    plotperYrBarplotPatient,
                    imageExtension = "png")
}

####get DALY####
DW<-loadDisabilityWeight('KOR',2012)
for (i  in seq(diseaseList$cohortId)){
    lifeExp <- loadLifeExpectancy('KOR')
    outcomeData <- Argos::getOutcomeData(connectionDetails = connectionDetails, 
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortTable = cohortTable,
                                         covariateSettings = covariateSettings,
                                         outcomeDatabaseSchema = cohortDatabaseSchema ,
                                         targetCohortId = diseaseList$cohortId[[i]],
                                         outcomeId = outcomeId,
                                         requireTimeAtRisk = FALSE,
                                         riskWindowStart = 0,
                                         riskWindowEnd = 365*5,
                                         removeSubjectsWithPriorOutcome = TRUE,
                                         minDateUnit = "year")
    disabilityWeightData <- DW[which(DW$condition_concept_id %in% diseaseList$conceptIdSet[[i]]),] 
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
    # saveRDS(DALY,file.path(outputFolder,paste0("DALY_cohortId_",diseaseList$cohortId[[i]],".rds" )))
    # write.csv(DALY,file.path(outputFolder,paste0("DALY_cohortId_",diseaseList$cohortId[[i]],".csv" )))
    
    plotDALY<-plotforDALY(DALYdata = DALY)
    plotDALYratio<-plotforDALYratio(DALYdata = DALY)
    
    saveDALY()
}

make_report()