# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of EmaCovidFeasibility
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

#' Execute the feasibility study
#'
#' @details
#' This function executes the feasibility study.
#'
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortTable          The name of the table that will be created in the work database schema.
#'                             This table will hold the exposure and outcome cohorts used in this
#'                             study.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param databaseId           A short string for identifying the database (e.g.
#'                             'Synpuf').
#' @param databaseName         The full name of the database (e.g. 'Medicare Claims
#'                             Synthetic Public Use Files (SynPUFs)').
#' @param databaseDescription  A short description (several sentences) of the database.
#' @param createCohorts        Create the cohortTable table with the exposure and outcome cohorts?
#' @param runExtraCharacterization    Generate and export additional characterizations?
#' @param runAdHocChecks              Generate and export ad hoc queries?
#' @param runCohortDiagnostics        Generate and export cohort diagnostics?
#' @param runInclusionStatistics      Generate and export statistic on the cohort incusion rules?
#' @param runIncludedSourceConcepts   Generate and export the source concepts included in the cohorts?
#' @param runOrphanConcepts           Generate and export potential orphan concepts?
#' @param runTimeDistributions        Generate and export cohort time distributions?
#' @param runBreakdownIndexEvents     Generate and export the breakdown of index events?
#' @param runIncidenceRates      Generate and export the cohort incidence rates?
#' @param runCohortOverlap            Generate and export the cohort overlap?
#' @param runCohortCharacterization   Generate and export the cohort characterization?
#' @param minCellCount         The minimum number of subjects contributing to a count before it can be included 
#'                             in packaged results.
#'
#' @export
execute <- function(
  connectionDetails,
  cdmDatabaseSchema,
  cohortDatabaseSchema = cdmDatabaseSchema,
  cohortTable = "cohort",
  oracleTempSchema = cohortDatabaseSchema,
  outputFolder,
  databaseId = "Unknown",
  databaseName = "Unknown",
  databaseDescription = "Unknown",
  createCohorts = TRUE,
  runExtraCharacterization = TRUE,
  runAdHocChecks = TRUE,
  runCohortDiagnostics = TRUE,
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeDistributions = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRates = TRUE,
  runCohortOverlap = TRUE,
  runCohortCharacterization = TRUE,
  minCellCount = 5
  ) {
  
  if (!file.exists(outputFolder))
    dir.create(outputFolder, recursive = TRUE)
  
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, "errorReportR.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)
  
  if (createCohorts) {
    
    ParallelLogger::logInfo("Creating cohorts")
    
    connection <- DatabaseConnector::connect(connectionDetails)
    
    .createCohorts(
      connection = connection,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      oracleTempSchema = oracleTempSchema,
      outputFolder = outputFolder
      )
    
    DatabaseConnector::disconnect(connection)
  }
  
  if(runExtraCharacterization){
    
    ParallelLogger::logInfo("Running characterization")
    
    temporalCovariateSettings <- FeatureExtraction::createTemporalCovariateSettings(
      useDemographicsGender = TRUE,
      useDemographicsAge = TRUE,
      useDemographicsAgeGroup = TRUE,
      useDemographicsRace = TRUE,
      useDemographicsEthnicity = TRUE,
      useDemographicsPriorObservationTime = TRUE,
      useDemographicsPostObservationTime = TRUE,
      useDemographicsIndexYearMonth = TRUE,
      useConditionEraGroupOverlap = TRUE,
      useDrugEraOverlap = TRUE,
      useDrugEraGroupOverlap = TRUE,
      useProcedureOccurrence = TRUE,
      useDeviceExposure = TRUE,
      useMeasurement = TRUE,
      useMeasurementValue = TRUE,
      useObservation = TRUE,
      useVisitConceptCount = TRUE,
      temporalStartDays = c(-730,0),
      temporalEndDays = c(-1,30)
    )
    
    counts <- readr::read_csv(file.path(outputFolder, "CohortCounts.csv"))
    
    characteristics <- purrr::map(
      split(counts, counts$cohortDefinitionId),
      ~getCohortCharacteristics(
        connectionDetails = connectionDetails,
        connection = NULL,
        cdmDatabaseSchema = cdmDatabaseSchema,
        oracleTempSchema = NULL,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        cohortId = .x$cohortDefinitionId,
        covariateSettings = temporalCovariateSettings,
        minCellCount
      )
    )
    
    purrr::map(characteristics, ~.x$continuousCovs) %>% 
      bind_rows(.id = "cohortDefinitionId") %>%
      readr::write_csv(file.path(outputFolder, "continuousCovs.csv"))
    
    purrr::map(characteristics, ~.x$binaryCovs) %>% 
      bind_rows(.id = "cohortDefinitionId") %>%
      readr::write_csv(file.path(outputFolder, "binaryCovs.csv"))
    
    purrr::map(characteristics, ~.x$covariateRef) %>% 
      bind_rows %>% 
      distinct %>%
      readr::write_csv(file.path(outputFolder, "covariateRef.csv"))
  }
  
  if(runAdHocChecks){
    
    ParallelLogger::logInfo("Running database analysis")
    
    adHocChecks <- adHocCecks(connectionDetails, cdmDatabaseSchema, outputFolder, minCellCount)
    
    readr::write_csv(adHocChecks$table_linkages, file.path(outputFolder, "tableLinkages.csv"))
    readr::write_csv(adHocChecks$during_life_plausibility, file.path(outputFolder, "duringLifePlausibility.csv"))  
    readr::write_csv(adHocChecks$cause_of_death, file.path(outputFolder, "causeOfDeath.csv"))
  }
  
  if(runCohortDiagnostics){
    
    ParallelLogger::logInfo("Running cohort diagnostics")
    
    outputFolderDiagnostics <-  file.path(outputFolder, "diagnostics")
    
    if (!file.exists(outputFolderDiagnostics))
      dir.create(outputFolderDiagnostics, recursive = TRUE)
    
    for(f in list.files(outputFolder,"cohort*",full.names = T)){
      file.copy(f,outputFolderDiagnostics)
    }
    
    CohortDiagnostics::runCohortDiagnostics(
      packageName = "EmaCovidFeasibility",
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      oracleTempSchema = oracleTempSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      inclusionStatisticsFolder = outputFolderDiagnostics,
      exportFolder = file.path(outputFolderDiagnostics, "diagnosticsExport"),
      databaseId = databaseId,
      databaseName = databaseName,
      databaseDescription = databaseDescription,
      runInclusionStatistics = runInclusionStatistics,
      runIncludedSourceConcepts = runIncludedSourceConcepts,
      runOrphanConcepts = runOrphanConcepts,
      runTimeDistributions = runTimeDistributions,
      runBreakdownIndexEvents = runBreakdownIndexEvents,
      runIncidenceRate = runIncidenceRates,
      runCohortOverlap = runCohortOverlap,
      runCohortCharacterization = runCohortCharacterization,
      runTemporalCohortCharacterization = FALSE,
      temporalCovariateSettings = NULL,
      minCellCount = minCellCount
    )
    
    ## Censor low cohort counts
    counts <- readr::read_csv(file.path(outputFolder, "CohortCounts.csv"))
    
    counts %>%
      mutate(count = case_when(count <= minCellCount ~ -minCellCount, TRUE ~ count)) %>%
      readr::write_csv(file.path(outputFolder, "CohortCounts.csv"))
  }
}
