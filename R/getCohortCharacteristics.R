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

#' Create characterization of a cohort
#'
#' @description
#' Computes features using all drugs, conditions, procedures, etc. observed on or prior to the cohort
#' index date.
#'
#' @param Connection connection
#'
#' @param CdmDatabaseSchema cdmDatabaseSchema
#'
#' @param OracleTempSchema OracleTempSchema
#'
#' @param CohortTable CohortTable
#'
#' @param cohortId            The cohort definition ID used to reference the cohort in the cohort
#'                            table.
#' @param covariateSettings   Either an object of type \code{covariateSettings} as created using one of
#'                            the createCovariate functions in the FeatureExtraction package, or a list
#'                            of such objects.
#' @param minCellCount        The minimum cell count that should be returned
#'                        
#'
#' @return
#' A list of data frames with cohort characteristics.
#'
#' @export
getCohortCharacteristics <- function(connectionDetails = NULL,
                                     connection = NULL,
                                     cdmDatabaseSchema,
                                     oracleTempSchema = NULL,
                                     cohortDatabaseSchema = cdmDatabaseSchema,
                                     cohortTable = "cohort",
                                     cohortId,
                                     covariateSettings,
                                     minCellCount) {
  start <- Sys.time()
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  data <- FeatureExtraction::getDbCovariateData(connection = connection,
                                                oracleTempSchema = oracleTempSchema,
                                                cdmDatabaseSchema = cdmDatabaseSchema,
                                                cohortDatabaseSchema = cohortDatabaseSchema,
                                                cohortTable = cohortTable,
                                                cohortId = cohortId,
                                                covariateSettings = covariateSettings,
                                                aggregated = TRUE)
  
  if (!is.null(data$covariates)) {
    n <- attr(x = data, which = "metaData")$populationSize
    if (FeatureExtraction::isTemporalCovariateData(data)) {
      counts <- data$covariates %>% 
        dplyr::collect() %>% 
        dplyr::mutate(sd = sqrt(((n * .data$sumValue) + .data$sumValue)/(n^2)))
      
      binaryCovs <- data$covariates %>% 
        dplyr::select(.data$timeId, .data$covariateId, .data$averageValue) %>% 
        dplyr::rename(mean = .data$averageValue) %>% 
        dplyr::collect() %>% 
        dplyr::left_join(counts, by = c("covariateId", "timeId")) %>% 
        dplyr::select(-.data$sumValue)
    } else {
      counts <- data$covariates %>% 
        dplyr::collect() %>% 
        dplyr::mutate(sd = sqrt(((n * .data$sumValue) + .data$sumValue)/(n^2)))
      
      binaryCovs <- data$covariates %>% 
        dplyr::select(.data$covariateId, .data$averageValue) %>% 
        dplyr::rename(mean = .data$averageValue) %>% 
        dplyr::collect() %>% 
        dplyr::left_join(counts, by = "covariateId") %>% 
        dplyr::select(-.data$sumValue)
    }
    
    if (nrow(binaryCovs) > 0) {
      if (FeatureExtraction::isTemporalCovariateData(data)) {
        binaryCovs <- binaryCovs %>% 
          dplyr::left_join(y = data$timeRef %>% dplyr::collect(), by = "timeId") %>% 
          dplyr::rename(startDayTemporalCharacterization = .data$startDay,
                        endDayTemporalCharacterization = .data$endDay) %>%
          mutate(mean = case_when(mean <= minCellCount/attr(data, "metaData")$populationSize ~ -minCellCount/attr(data, "metaData")$populationSize, TRUE ~ mean))
      }
    }
  }else{
    binaryCovs <- NULL}
  
  if (!is.null(data$covariatesContinuous)) {
    if (FeatureExtraction::isTemporalCovariateData(data)) {
      continuousCovs <- data$covariatesContinuous %>% 
        dplyr::select(.data$timeId, .data$countValue, .data$covariateId, .data$averageValue, .data$standardDeviation, .data$minValue, .data$maxValue, .data$p10Value, .data$p25Value, .data$medianValue, .data$p75Value, .data$p90Value) %>% 
        dplyr::rename(sd = .data$standardDeviation) %>% 
        dplyr::collect() %>%
        dplyr::mutate(mean = .data$countValue /  attr(data, "metaData")$populationSize) 
    } else {
      continuousCovs <- data$covariatesContinuous %>% 
        dplyr::select(.data$timeId, .data$countValue, .data$covariateId, .data$averageValue, .data$standardDeviation, .data$minValue, .data$maxValue, .data$p10Value, .data$p25Value, .data$medianValue, .data$p75Value, .data$p90Value) %>% 
        dplyr::rename(sd = .data$standardDeviation) %>% 
        dplyr::collect() %>%
        dplyr::mutate(mean = .data$countValue /  attr(data, "metaData")$populationSize) 
    }
    
    if (nrow(continuousCovs) > 0) {
      if (FeatureExtraction::isTemporalCovariateData(data)) {
        continuousCovs <- continuousCovs %>% 
          dplyr::left_join(y = data$timeRef %>% dplyr::collect(), by = "timeId") %>% 
          dplyr::rename(startDayTemporalCharacterization = .data$startDay,
                        endDayTemporalCharacterization = .data$endDay)  %>%
          filter(countValue >= pmax(minCellCount, 100))  
      }
    }
    
  }else{
    continuousCovs <- NULL
  }
  
  delta <- Sys.time() - start
  if (FeatureExtraction::isTemporalCovariateData(data)) {
    ParallelLogger::logInfo(paste("Temporal Cohort characterization took",
                                  signif(delta, 3),
                                  attr(delta, "units")))
  } else {
    ParallelLogger::logInfo(paste("Cohort characterization took",
                                  signif(delta, 3),
                                  attr(delta, "units")))
  }

  return(list(binaryCovs = binaryCovs, continuousCovs = continuousCovs, covariateRef = data$covariateRef %>% collect()))
}
