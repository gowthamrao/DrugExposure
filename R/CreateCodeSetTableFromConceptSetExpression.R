#' Resolve Concept Set Expression
#'
#' This function resolves a given concept set expression (R object, not JSON) to retrieve relevant concept IDs
#' from a specified vocabulary database schema. It builds and executes a SQL query to extract concept IDs
#' based on the provided concept set expression.
#'
#' @param conceptSetExpression The JSON string of the concept set expression to be resolved.
#' @param connection An optional database connection object. If not provided, a connection will be established using the provided connection details.
#' @param vocabularyDatabaseSchema The name of the database schema containing the vocabulary tables.
#' @param tempEmulationSchema The name of the schema used for SQL emulation of temporary tables.
#'
#' @return A tibble containing the concept IDs sorted in ascending order.
createCodeSetTableFromConceptSetExpression <-
  function(conceptSetExpression,
           connection,
           vocabularyDatabaseSchema,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
    
    conceptSetSql <-
      CirceR::buildConceptSetQuery(conceptSetJSON = conceptSetExpression |> RJSONIO::toJSON(digits = 23))
    
    conceptSetSql <- paste0(
      "DROP TABLE IF EXISTS #concept_sets;

       SELECT concept_id
       INTO #concept_sets
       FROM
       ( ",
      conceptSetSql,
      " ) f;"
    )
    
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = conceptSetSql,
      tempEmulationSchema = tempEmulationSchema,
      vocabulary_database_schema = vocabularyDatabaseSchema
    )
  }
