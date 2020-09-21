# # Make sure to install all dependencies (not needed if already done):
# install.packages("SqlRender")
# install.packages("DatabaseConnector")
# install.packages("ggplot2")
# install.packages("ParallelLogger")
# install.packages("readr")
# install.packages("tibble")
# install.packages("dplyr")
# install.packages("RJSONIO")
# install.packages("devtools")
#devtools::install_github("OHDSI/ROhdsiWebApi")

# Please update to latest versions of the following packages:
devtools::install_github("OHDSI/FeatureExtraction", ref="v3.0.1")
devtools::install_github("OHDSI/DatabaseConnectoR", ref="v3.0.0")
devtools::install_github("OHDSI/CohortDiagnostics", ref="v1.1.1")
devtools::install_github("OHDSI/SqlRender", ref="v1.6.8")
devtools::install_github("OHDSI/Andromeda", ref="develop")

# Load the package
library(EmaCovidFeasibility)
library(dplyr)

# Optional: specify where the temporary files will be created. This folder should already exist:
options(andromedaTempFolder = "~/tmp")

# Maximum number of cores to be used:
maxCores <- parallel::detectCores()

# Details for connecting to the server:
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = ,
  server = ,
  user = ,
  password = ,
  port = 
  )

# For Oracle: define a schema that can be used to emulate temp tables:
oracleTempSchema <- NULL

# Details specific to the database:
cdmDatabaseSchema <-  
cohortDatabaseSchema <- 
cohortTable <- 
databaseId <- 
databaseName <- 
databaseDescription <- 

# Specify te output folder
#dir.create(file.path(getwd(),"output"))
#outputFolder <- file.path(getwd(),"output")
outputFolder <- 

# Use this to run the cohorttDiagnostics. The results will be stored in the diagnosticsExport subfolder of the outputFolder. This can be shared between sites.
execute(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  oracleTempSchema = oracleTempSchema,
  outputFolder = outputFolder,
  databaseId = databaseId,
  databaseName = databaseName,
  databaseDescription = databaseDescription,
  createCohorts = TRUE,
  runExtraCharacterization = TRUE,
  runAdHocChecks = TRUE,
  runCohortDiagnostics = FALSE, # no need to run full cohort diagnostics for now
  minCellCount = 5
)
