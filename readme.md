EMA COVID Fesaibility and Characterization
==============================================

1. Open the .RProj file in RStudio
2. Install the package using 'Build' --> 'Install and restart' in the top right hand pane  
3. Open the file 'CodeToRun.R' which is found in the 'extras' folder of the package  
4. Check you have all the required packages versions installed (lines 1 to 22)  
5. Fill in the parameters relating to your OMOP database (lines 25 to 53)  

connectionDetails: Please see https://rdrr.io/cran/DatabaseConnector/man/createConnectionDetails.html  
cdmDatabaseSchema: The name of the schema where your OMOP data is stored  
cohortDatabaseSchema: The name of a schema where the user has write access. Cohorts and other incidental data will be written here  
cohortTable: A table name of your choosing where cohort information will be stored in cohortDatabaseSchema  
databaseId: A string id of your choosing  
databaseName: A name of your choosing  
databaseId: A description of your choosing
outputFolder: The full path to a folder where the package will store outputs

6. Execute the package by running the execute function on line 56.






