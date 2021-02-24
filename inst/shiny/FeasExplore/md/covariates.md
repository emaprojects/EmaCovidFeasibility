This section shows the frequency of covariates recorded for patients in a given cohort before (between day -730 and day -1 inclusive) and after (between day 0 and day 30 inclusive) the index date. Covariates are classified into a domain according to origin of the information in the Common Data Model:

- **condition**: The frequency of active condition eras recorded in the [condition_era](https://github.com/OHDSI/CommonDataModel/wiki/CONDITION_ERA) table, aggregated according to the SNOMED heirarchy.  
- **drug**: The frequency of active drug eras recorded in the [drug_era](https://github.com/OHDSI/CommonDataModel/wiki/DRUG_ERA) table, aggregated to the ingredient and according to the ATC classificaiton heirarchy.  
- **procedure**: The frequency of procedures recorded in the [procedure_occurrence](https://github.com/OHDSI/CommonDataModel/wiki/PROcEDURE_OCCURRENCE) table.  
- **measurement**: The frequency of measurements recorded in the [measurement](https://github.com/OHDSI/CommonDataModel/wiki/PROcEDURE_OCCURRENCE) table.  
- **meas value**: The frequency of measurements with a corresponding unit and value recorded in the [measurement](https://github.com/OHDSI/CommonDataModel/wiki/MEASUREMENT) table.  
- **observation**: The frequency of observations recorded in the [observation](https://github.com/OHDSI/CommonDataModel/wiki/OBSERVATION) table.  
