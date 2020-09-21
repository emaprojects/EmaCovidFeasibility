/*********
  LINKAGE_CHECKS
get number of persons and events in cdm table
Parameters used in this template:
cdmDatabaseSchema = @cdmDatabaseSchema
cdmTableName = @cdmTableName
cdmFieldName = @cdmFieldName
**********/
SELECT 'PERSON.@cdmTableName' AS linking_table, COUNT_BIG(DISTINCT p.person_id) AS PERSONS, COUNT_BIG(DISTINCT l.@cdmFieldName) AS RECORDS
FROM @cdmDatabaseSchema.PERSON p
INNER JOIN @cdmDatabaseSchema.@cdmTableName l ON l.person_id = p.person_id