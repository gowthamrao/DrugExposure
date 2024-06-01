#' Resolve Concept Set Expression
#'
#' This function resolves a given concept set expression (R object, not JSON) to retrieve relevant concept IDs 
#' from a specified vocabulary database schema. It builds and executes a SQL query to extract concept IDs 
#' based on the provided concept set expression.
#'
#' @param conceptSetExpression The JSON string of the concept set expression to be resolved.
#' @param connection An optional database connection object. If not provided, a connection will be established using the provided connection details.
#' @param connectionDetails An optional list specifying the details necessary to establish a database connection. If no connection is provided, this parameter must be specified.
#' @param vocabularyDatabaseSchema The name of the database schema containing the vocabulary tables.
#' @param tempEmulationSchema The name of the schema used for SQL emulation of temporary tables.
#'
#' @return A tibble containing the concept IDs sorted in ascending order.
resolveConceptSetExpression <- function(conceptSetExpression,
                                        connection = NULL,
                                        connectionDetails = NULL,
                                        vocabularyDatabaseSchema,
                                        tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  conceptSetSql <-
    CirceR::buildConceptSetQuery(conceptSetJSON = conceptSetExpression |> RJSONIO::toJSON(digits = 23))
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  conceptIds <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = conceptSetSql,
      snakeCaseToCamelCase = TRUE,
      tempEmulationSchema = tempEmulationSchema,
      vocabulary_database_schema = vocabularyDatabaseSchema
    ) |>
    dplyr::tibble() |>
    dplyr::arrange(conceptId)
  
  return(conceptIds)
}
