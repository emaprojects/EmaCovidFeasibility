/*********
  CAUSE OF DEATH
get a summary of death causes
Parameters used in this template:
cdmDatabaseSchema = @cdmDatabaseSchema
cdmTableName = @cdmTableName
cdmFieldName = @cdmFieldName
**********/
SELECT d.cause_concept_id, c.concept_name, COUNT_BIG(*) AS RECORDS
FROM @cdmDatabaseSchema.DEATH d
LEFT JOIN @cdmDatabaseSchema.CONCEPT c on c.concept_id = d.cause_concept_id
GROUP BY d.cause_concept_id, c.concept_name