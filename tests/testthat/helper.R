#' utility function to make sure connection is closed after usage
with_dbc_connection <- function(connection, code) {
  on.exit({
    DatabaseConnector::disconnect(connection)
  })
  eval(substitute(code), envir = connection, enclos = parent.frame())
}


loadConceptSetJson <- function() {
  if (grepl("testthat", getwd())) {
    testFolderPath <- "testfiles"
  } else {
    testFolderPath <- file.path("tests", "testthat", "testfiles")
  }
  
  conceptSetJson <- SqlRender::readSql(file.path(testFolderPath, "ConceptSet.json"))
  
  return(conceptSetJson)
}
