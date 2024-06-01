library(testthat)
library(DrugExposure)
library(dplyr)


dbms <- getOption("dbms", default = "postgresql")
message("************* Testing on ", dbms, " *************")

if (dir.exists(Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"))) {
  jdbcDriverFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
} else {
  jdbcDriverFolder <- tempfile("jdbcDrivers")
  dir.create(jdbcDriverFolder, showWarnings = FALSE)
  DatabaseConnector::downloadJdbcDrivers("postgresql", pathToDriver = jdbcDriverFolder)

  if (!dbms %in% c("postgresql")) {
    DatabaseConnector::downloadJdbcDrivers(dbms, pathToDriver = jdbcDriverFolder)
  }

  withr::defer(
    {
      unlink(jdbcDriverFolder, recursive = TRUE, force = TRUE)
    },
    testthat::teardown_env()
  )
}

folder <- tempfile()
dir.create(folder, recursive = TRUE)
skipCdmTests <- FALSE


if (dbms == "postgresql") {
  dbUser <- Sys.getenv("CDM5_POSTGRESQL_USER")
  dbPassword <- Sys.getenv("CDM5_POSTGRESQL_PASSWORD")
  dbServer <- Sys.getenv("CDM5_POSTGRESQL_SERVER")
  cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  vocabularyDatabaseSchema <-
    Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  tempEmulationSchema <- NULL
  cohortDatabaseSchema <-
    Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA")
} else if (dbms == "oracle") {
  dbUser <- Sys.getenv("CDM5_ORACLE_USER")
  dbPassword <- Sys.getenv("CDM5_ORACLE_PASSWORD")
  dbServer <- Sys.getenv("CDM5_ORACLE_SERVER")
  cdmDatabaseSchema <- Sys.getenv("CDM5_ORACLE_CDM_SCHEMA")
  vocabularyDatabaseSchema <- Sys.getenv("CDM5_ORACLE_CDM_SCHEMA")
  tempEmulationSchema <- Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")
  cohortDatabaseSchema <- Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")
  options(sqlRenderTempEmulationSchema = tempEmulationSchema)
} else if (dbms == "redshift") {
  dbUser <- Sys.getenv("CDM5_REDSHIFT_USER")
  dbPassword <- Sys.getenv("CDM5_REDSHIFT_PASSWORD")
  dbServer <- Sys.getenv("CDM5_REDSHIFT_SERVER")
  cdmDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA")
  vocabularyDatabaseSchema <-
    Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA")
  tempEmulationSchema <- NULL
  cohortDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_OHDSI_SCHEMA")
} else if (dbms == "sql server") {
  dbUser <- Sys.getenv("CDM5_SQL_SERVER_USER")
  dbPassword <- Sys.getenv("CDM5_SQL_SERVER_PASSWORD")
  dbServer <- Sys.getenv("CDM5_SQL_SERVER_SERVER")
  cdmDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
  vocabularyDatabaseSchema <-
    Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
  tempEmulationSchema <- NULL
  cohortDatabaseSchema <-
    Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA")
}

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = dbms,
  user = dbUser,
  password = URLdecode(dbPassword),
  server = dbServer,
  pathToDriver = jdbcDriverFolder
)

if (cdmDatabaseSchema == "" || dbServer == "") {
  skipCdmTests <- TRUE
}


withr::defer(
  {
  },
  testthat::teardown_env()
)


conceptSetExpressionDataFrame <- dplyr::tibble(
  CONCEPT_CLASS_ID = "Ingredient",
  CONCEPT_CODE = "140587",
  CONCEPT_ID = 1118084,
  CONCEPT_NAME = "celecoxib",
  DOMAIN_ID = "Drug",
  INVALID_REASON = "V",
  INVALID_REASON_CAPTION = "Valid",
  STANDARD_CONCEPT = "S",
  STANDARD_CONCEPT_CAPTION = "Standard",
  VOCABULARY_ID = "RxNorm",
  VALID_START_DATE = as.Date("1969-12-31"),
  VALID_END_DATE = as.Date("2099-12-30")
)

conceptSetExpression <- list()
conceptSetExpression$items <- list()
conceptSetExpression$items[[1]] <- list()
conceptSetExpression$items[[1]]$concept <-
  conceptSetExpressionDataFrame[1, ] |>
  as.list()
conceptSetExpression$items[[1]]$isExcluded <- FALSE
conceptSetExpression$items[[1]]$includeDescendants <- TRUE
conceptSetExpression$items[[1]]$includeMapped <- FALSE


conceptSetJson <- conceptSetExpression |> RJSONIO::toJSON(digits = 23, pretty = TRUE)