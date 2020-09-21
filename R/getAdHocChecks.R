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

adHocCecks <- function(connectionDetails, cdmDatabaseSchema, outputFolder, minCellCount){
  
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  
  during_life_plausibility <- purrr::map2(
    c('CONDITION_OCCURRENCE', 'DRUG_EXPOSURE', 'MEASUREMENT'), 
    c('CONDITION_START_DATE', 'DRUG_EXPOSURE_START_DATE', 'MEASUREMENT_DATE'),
    function(cdmTableName, cdmFieldName){
      
      writeLines(paste("Generating during life plausibility:", cdmTableName))
      
      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "field_plausible_during_life.sql", 
        packageName = "EmaCovidFeasibility", 
        dbms = connectionDetails$dbms,
        cdmDatabaseSchema = cdmDatabaseSchema, 
        cdmTableName = cdmTableName, 
        cdmFieldName = cdmFieldName,
        cohort = FALSE
      )
      
      DatabaseConnector::querySql(connection = connection, sql) %>%
        mutate(TABLE_NAME = cdmTableName)
    }
  ) %>%
    bind_rows 
  
  table_linkages <- purrr::map2(
    c('PERSON','CONDITION_OCCURRENCE', 'DRUG_EXPOSURE', 'MEASUREMENT', 'VISIT_OCCURRENCE'), 
    c('PERSON_ID','CONDITION_OCCURRENCE_ID', 'DRUG_EXPOSURE_ID', 'MEASUREMENT_ID', 'VISIT_OCCURRENCE_ID'),
    function(cdmTableName, cdmFieldName){
      
      writeLines(paste("Generating table linkage counts:", cdmTableName))
      
      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "table_linkage.sql", 
        packageName = "EmaCovidFeasibility", 
        dbms = connectionDetails$dbms,
        cdmDatabaseSchema = cdmDatabaseSchema, 
        cdmTableName = cdmTableName, 
        cdmFieldName = cdmFieldName
      )
      
      DatabaseConnector::querySql(connection = connection, sql) %>%
        mutate(RECORDS = case_when(RECORDS <= minCellCount ~ -minCellCount, TRUE ~ RECORDS),
               PERSONS = case_when(PERSONS <= minCellCount ~ -minCellCount, TRUE ~ PERSONS))
      
    }
  ) %>%
    bind_rows
  
  sql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "cause_of_death.sql", 
    packageName = "EmaCovidFeasibility", 
    dbms = connectionDetails$dbms,
    cdmDatabaseSchema = cdmDatabaseSchema
  )
  
  cause_of_death <- DatabaseConnector::querySql(connection = connection, sql) %>%
    mutate(RECORDS = case_when(RECORDS <= minCellCount ~ -minCellCount, TRUE ~ RECORDS))
  
  return(list(table_linkages = table_linkages, during_life_plausibility = during_life_plausibility, cause_of_death = cause_of_death))
  
}
