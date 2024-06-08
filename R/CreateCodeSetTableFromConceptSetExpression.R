#' Resolve Concept Set Expression
#'
#' This function resolves a given concept set expression (R object, not JSON) to 
#' output a temp table with concept IDs.
#'
#' @template Connection
#' @template VocabularyDatabaseSchema
#' @template TempEmulationSchema
#' @template ConceptSetExpression
#' @template ConceptSetTable
#'
#' @return A tibble containing the concept IDs sorted in ascending order.
createCodeSetTableFromConceptSetExpression <-
  function(connection,
           vocabularyDatabaseSchema,
           conceptSetExpression,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           conceptSetTable = "#concept_sets") {
    
    conceptSetSql <-
      CirceR::buildConceptSetQuery(conceptSetJSON = 
                                     conceptSetExpression |> 
                                     RJSONIO::toJSON(digits = 23))
    
    conceptSetSql <- paste0(
      "DROP TABLE IF EXISTS @concept_set_table;

       SELECT concept_id
       INTO @concept_set_table
       FROM
       ( ",
      conceptSetSql,
      " ) f;"
    )
    
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = conceptSetSql,
      tempEmulationSchema = tempEmulationSchema,
      vocabulary_database_schema = vocabularyDatabaseSchema,
      concept_set_table = conceptSetTable
    )
  }
