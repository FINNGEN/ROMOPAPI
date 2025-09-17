#
# COPY FROM HadesExtras v1.1.2 (https://github.com/FINNGEN/HadesExtras/blob/master/R/CDMdbHandler.R 16.09.2025), to avoid dependency on HadesExtras
#

#' CDM Database Handler Class
#'
#' @description
#' A class for handling CDM database connections and operations
#'
#' @export
#' @importFrom R6 R6Class
#'
#' @field databaseId The ID of the database
#' @field databaseName The name of the database
#' @field databaseDescription Description of the database
#' @field connectionHandler Handler for database connections
#' @field vocabularyDatabaseSchema Schema name for the vocabulary database
#' @field cdmDatabaseSchema Schema name for the CDM database
#' @field resultsDatabaseSchema Schema name for the results database
#' @field connectionStatusLog Log of connection status and operations
#' @field vocabularyInfo Information about the vocabulary tables
#' @field CDMInfo Information about the CDM structure
#' @field getTblVocabularySchema Function to get vocabulary schema table
#' @field getTblCDMSchema Function to get CDM schema table
#'
#' @section Methods:
#' \describe{
#'   \item{`initialize(databaseId)`}{Initialize a new CDM database handler}
#'   \item{`loadConnection(loadConnectionChecksLevel)`}{Load database connection with specified check level}
#' }
#'
#' @param databaseId ID of the database to connect to
#' @param loadConnectionChecksLevel Level of connection checks to perform
CDMdbHandler <- R6::R6Class(
  classname = "CDMdbHandler",
  private = list(
    .databaseId = NULL,
    .databaseName = NULL,
    .databaseDescription = NULL,
    # database parameters
    .connectionHandler = NULL,
    .vocabularyDatabaseSchema = NULL,
    .cdmDatabaseSchema = NULL,
    .resultsDatabaseSchema = NULL,
    .connectionStatusLog = NULL,
    #
    .vocabularyInfo = NULL,
    .CDMInfo = NULL,
    #
    .getTblVocabularySchema = NULL,
    .getTblCDMSchema = NULL,
    # Finalize method
    finalize = function() {
      private$.connectionHandler$finalize()
    }
  ),
  active = list(
    databaseId = function() {
      return(private$.databaseId)
    },
    databaseName = function() {
      return(private$.databaseName)
    },
    databaseDescription = function() {
      return(private$.databaseDescription)
    },
    # database parameters
    connectionHandler = function() {
      return(private$.connectionHandler)
    },
    vocabularyDatabaseSchema = function() {
      return(private$.vocabularyDatabaseSchema)
    },
    cdmDatabaseSchema = function() {
      return(private$.cdmDatabaseSchema)
    },
    resultsDatabaseSchema = function() {
      return(private$.resultsDatabaseSchema)
    },
    connectionStatusLog = function() {
      return(private$.connectionStatusLog$logTibble |>
        dplyr::mutate(databaseId = private$.databaseId, databaseName = private$.databaseName) |>
        dplyr::relocate(databaseId, databaseName, .before = 1))
    },
    #
    vocabularyInfo = function() {
      return(private$.vocabularyInfo)
    },
    CDMInfo = function() {
      return(private$.CDMInfo)
    },
    #
    getTblVocabularySchema = function() {
      return(private$.getTblVocabularySchema)
    },
    getTblCDMSchema = function() {
      return(private$.getTblCDMSchema)
    }
  ),
  public = list(
    #'
    #' @param databaseId             A text id for the database the it connects to
    #' @param databaseName           A text name for the database the it connects to
    #' @param databaseDescription    A text description for the database the it connects to
    #' @param connectionHandler             A ConnectionHandler object
    #' @param cdmDatabaseSchema             Name of the CDM database schema
    #' @param vocabularyDatabaseSchema      (Optional) Name of the vocabulary database schema (default is cdmDatabaseSchema)
    #' @param resultsDatabaseSchema         (Optional) Name of the results database schema (default is cdmDatabaseSchema)
    #' @param loadConnectionChecksLevel     (Optional) Level of checks to perform when loading the connection (default is "allChecks")
    initialize = function(databaseId,
                          databaseName,
                          databaseDescription,
                          connectionHandler,
                          cdmDatabaseSchema,
                          vocabularyDatabaseSchema = cdmDatabaseSchema,
                          resultsDatabaseSchema = cdmDatabaseSchema,
                          loadConnectionChecksLevel = "allChecks") {
      checkmate::assertString(databaseId)
      checkmate::assertString(databaseName, )
      checkmate::assertString(databaseDescription)
      checkmate::assertClass(connectionHandler, "ConnectionHandler")
      checkmate::assertString(cdmDatabaseSchema)
      checkmate::assertString(vocabularyDatabaseSchema)
      checkmate::assertString(resultsDatabaseSchema)
      private$.databaseId <- databaseId
      private$.databaseName <- databaseName
      private$.databaseDescription <- databaseDescription
      private$.connectionHandler <- connectionHandler
      private$.vocabularyDatabaseSchema <- vocabularyDatabaseSchema
      private$.cdmDatabaseSchema <- cdmDatabaseSchema
      private$.resultsDatabaseSchema <- resultsDatabaseSchema

      self$loadConnection(loadConnectionChecksLevel)
    },

    #' Reload connection
    #' @description
    #' Updates the connection status by checking the database connection, vocabulary database schema, and CDM database schema.
    loadConnection = function(loadConnectionChecksLevel) {
      checkmate::assertString(loadConnectionChecksLevel)
      checkmate::assertSubset(loadConnectionChecksLevel, c("dontConnect", "basicChecks", "allChecks"))

      connectionStatusLog <- LogTibble$new()

      if (loadConnectionChecksLevel == "dontConnect") {
        private$.connectionStatusLog <- connectionStatusLog
        return()
      }

      # Check db connection
      errorMessage <- ""
      tryCatch(
        {
          private$.connectionHandler$initConnection()
        },
        error = function(error) {
          errorMessage <<- error$message
        },
        warning = function(warning) {}
      )

      if (errorMessage != "" | !private$.connectionHandler$dbIsValid()) {
        connectionStatusLog$ERROR("Check database connection", errorMessage)
      } else {
        connectionStatusLog$SUCCESS("Check database connection", "Valid connection")
      }

      # Check can create temp tables
      if (loadConnectionChecksLevel == "allChecks") {
        errorMessage <- ""
        tryCatch(
          {
            private$.connectionHandler$getConnection() |>
              DatabaseConnector::insertTable(
                tableName = "test_temp_table",
                data = dplyr::tibble(x = 1),
                tempTable = TRUE
              )
            private$.connectionHandler$getConnection() |>
              DatabaseConnector::dropEmulatedTempTables()
          },
          error = function(error) {
            errorMessage <<- error$message
          }
        )

        if (errorMessage != "") {
          connectionStatusLog$ERROR("Check temp table creation", errorMessage)
        } else {
          connectionStatusLog$SUCCESS("Check temp table creation", "can create temp tables")
        }
      } else {
        connectionStatusLog$WARNING("Check temp table creation", "skipped")
      }

      # Check vocabularyDatabaseSchema and populates getTblVocabularySchema
      getTblVocabularySchema <- list()
      vocabularyInfo <- NULL
      errorMessage <- ""
      tryCatch(
        {
          # create dbplyr object for all tables in the vocabulary
          vocabularyTableNames <- c(
            "concept",
            "vocabulary",
            "domain",
            "concept_class",
            "concept_relationship",
            "relationship",
            "concept_synonym",
            "concept_ancestor",
            "source_to_concept_map",
            "drug_strength"
          )

          tablesInVocabularyDatabaseSchema <- DatabaseConnector::getTableNames(private$.connectionHandler$getConnection(), private$.vocabularyDatabaseSchema)

          vocabularyTablesInVocabularyDatabaseSchema <- intersect(vocabularyTableNames, tablesInVocabularyDatabaseSchema)

          for (tableName in vocabularyTablesInVocabularyDatabaseSchema) {
            text <- paste0('function() { private$.connectionHandler$tbl( "', tableName, '", "', private$.vocabularyDatabaseSchema, '")}')
            getTblVocabularySchema[[tableName]] <- eval(parse(text = text))
          }

          vocabularyInfo <- getTblVocabularySchema$vocabulary() |>
            dplyr::filter(vocabulary_id == "None") |>
            dplyr::select(vocabulary_name, vocabulary_version) |>
            dplyr::collect(n = 1)
        },
        error = function(error) {
          errorMessage <<- error$message
        }
      )

      if (errorMessage != "") {
        connectionStatusLog$ERROR("vocabularyDatabaseSchema connection", errorMessage)
      } else {
        connectionStatusLog$SUCCESS(
          "vocabularyDatabaseSchema connection",
          "Connected to vocabulary:", vocabularyInfo$vocabulary_name,
          "Version: ", vocabularyInfo$vocabulary_version
        )
      }


      # Check cdmDatabaseSchema and populates getTblCDMSchema
      getTblCDMSchema <- list()
      CDMInfo <- NULL
      errorMessage <- ""
      tryCatch(
        {
          cdmTableNames <- c(
            "person",
            "observation_period",
            "visit_occurrence",
            "visit_detail",
            "condition_occurrence",
            "drug_exposure",
            "procedure_occurrence",
            "device_exposure",
            "measurement",
            "observation",
            "death",
            "note",
            "note_nlp",
            "specimen",
            "fact_relationship",
            "location",
            "care_site",
            "provider",
            "payer_plan_period",
            "cost",
            "drug_era",
            "dose_era",
            "condition_era",
            "episode",
            "episode_event",
            "metadata",
            "cdm_source"
          )

          tablesInCdmDatabaseSchema <- DatabaseConnector::getTableNames(private$.connectionHandler$getConnection(), private$.cdmDatabaseSchema)

          vocabularyTablesInCdmDatabaseSchema <- intersect(cdmTableNames, tablesInCdmDatabaseSchema)

          for (tableName in vocabularyTablesInCdmDatabaseSchema) {
            text <- paste0('function() { private$.connectionHandler$tbl( "', tableName, '", "', private$.cdmDatabaseSchema, '")}')
            getTblCDMSchema[[tableName]] <- eval(parse(text = text))
          }

          CDMInfo <- getTblCDMSchema$cdm_source() |>
            dplyr::select(cdm_source_name, cdm_source_abbreviation, cdm_version) |>
            dplyr::collect(n = 1)
        },
        error = function(error) {
          errorMessage <<- error$message
        }
      )

      if (errorMessage != "") {
        connectionStatusLog$ERROR("cdmDatabaseSchema connection", errorMessage)
      } else {
        connectionStatusLog$SUCCESS(
          "cdmDatabaseSchema connection",
          "Connected to cdm:", CDMInfo$cdm_source_name, "Version: ", CDMInfo$ cdm_version
        )
      }


      # update status
      private$.vocabularyInfo <- vocabularyInfo
      private$.CDMInfo <- CDMInfo
      private$.connectionStatusLog <- connectionStatusLog
      private$.getTblVocabularySchema <- getTblVocabularySchema
      private$.getTblCDMSchema <- getTblCDMSchema
    }
  )
)


#' createCDMdbHandlerFromList
#'
#' A function to create a CDMdbHandler object from a list of configuration settings.
#'
#' @param config A list containing configuration settings for the CDMdbHandler.
#'   - databaseName: The name of the database.
#'   - connection: A list of connection details settings.
#'   - cdm: A list of CDM database schema settings.
#'   - cohortTable: The name of the cohort table.
#' @param loadConnectionChecksLevel The level of checks to perform when loading the connection.
#'
#' @return A CDMdbHandler object.
#'
#' @importFrom checkmate assertList assertNames
#'
#' @export
HadesExtras_createCDMdbHandlerFromList <- function(
    config,
    loadConnectionChecksLevel = "allChecks") {
  # check parameters
  config |> checkmate::assertList()
  config |>
    names() |>
    checkmate::assertNames(must.include = c("database", "connection", "cdm"))

  # create connectionHandler
  connectionHandler <- connectionHandlerFromList(config$connection)

  # create CDMdbHandler
  CDMdb <- CDMdbHandler$new(
    databaseId = config$database$databaseId,
    databaseName = config$database$databaseName,
    databaseDescription = config$database$databaseDescription,
    connectionHandler = connectionHandler,
    cdmDatabaseSchema = config$cdm$cdmDatabaseSchema,
    vocabularyDatabaseSchema = config$cdm$vocabularyDatabaseSchema,
    resultsDatabaseSchema = if (!is.null(config$cdm$resultsDatabaseSchema)) config$cdm$resultsDatabaseSchema else config$cdm$cdmDatabaseSchema,
    loadConnectionChecksLevel = loadConnectionChecksLevel
  )

  return(CDMdb)
}

#
# COPY FROM HadesExtras v1.1.2 (https://github.com/FINNGEN/HadesExtras/blob/master/R/connectionHandler.R 16.09.2025), to avoid dependency on HadesExtras
#


#' Create a ConnectionHandler from a configuration list
#'
#' @param configConnection A list containing connection configuration settings:
#'   - tempEmulationSchema: Schema to use for temp table emulation
#'   - connectionDetailsSettings: Settings for creating connection details
#'
#' @return A ConnectionHandler object
#'
#' @importFrom checkmate assertList assertNames
#' @importFrom rlang exec
#' @importFrom DatabaseConnector createDbiConnectionDetails createConnectionDetails
#' @importFrom ResultModelManager ConnectionHandler
#'
#' @export

connectionHandlerFromList <- function(configConnection) {
  # check parameters
  configConnection |> checkmate::assertList()
  configConnection |>
    names() |>
    checkmate::assertNames(must.include = c( "connectionDetailsSettings"))

  # set tempEmulationSchema if in config
  if (!is.null(configConnection$tempEmulationSchema)) {
    options(sqlRenderTempEmulationSchema = configConnection$tempEmulationSchema)
  } else {
    options(sqlRenderTempEmulationSchema = NULL)
  }

  # create connectionHandler
  connectionDetailsSettings <- configConnection$connectionDetailsSettings
  if (!is.null(connectionDetailsSettings$drv)) {
    # IBD connection details
    eval(parse(text = paste0("tmpDriverVar <- ", connectionDetailsSettings$drv)))
    connectionDetailsSettings$drv  <- tmpDriverVar
    connectionDetails <- rlang::exec(DatabaseConnector::createDbiConnectionDetails, !!!connectionDetailsSettings)
  } else {
    # JDBC connection details
    connectionDetails <- rlang::exec(DatabaseConnector::createConnectionDetails, !!!connectionDetailsSettings)
  }

  connectionHandler <- ResultModelManager::ConnectionHandler$new(
    connectionDetails = connectionDetails,
    loadConnection = FALSE
  )

  return(connectionHandler)

}

#
# COPY FROM HadesExtras v1.1.2 (https://github.com/FINNGEN/HadesExtras/blob/master/R/ConfigurationHelpers.R 16.09.2025), to avoid dependency on HadesExtras
#

#' Read and Parse YAML File with Placeholder Replacement
#'
#' Reads a YAML file from a given path, replaces specified placeholders with provided values,
#' and returns the parsed content. If any provided placeholders are not found in the YAML file,
#' the function throws an error.
#'
#' @param pathToYalmFile A string representing the file path to the YAML file.
#' @param ... Named arguments to replace placeholders in the format `<name>` within the YAML file.
#'
#' @return A parsed list representing the contents of the modified YAML file.
#'
#' @importFrom yaml yaml.load as.yaml
#'
#' @export
HadesExtras_readAndParseYaml <- function(pathToYalmFile, ...) {
  # read the yaml file
  yalmFile <- readLines(pathToYalmFile)
  # get the names of the parameters
  names <- names(list(...))

  # check for missing placeholders
  missingParams <- names[!sapply(names, function(name) any(grepl(paste0("<", name, ">"), yalmFile)))]

  # if any placeholders are not found, throw an error
  if (length(missingParams) > 0) {
    stop(paste("Error: The following placeholders were not found in the YAML file:", paste(missingParams, collapse = ", ")))
  }

  # replace the values in the yaml file
  for (name in names) {
    yalmFile <- gsub(paste0("<", name, ">"), list(...)[[name]], yalmFile, fixed = TRUE)
  }

  # parse the yaml file
  yalmFile <- yaml::yaml.load(yalmFile)
  return(yalmFile)
}



#
# COPY FROM HadesExtras v1.1.2 (https://github.com/FINNGEN/HadesExtras/blob/master/R/LogTibble.R 16.09.2025), to avoid dependency on HadesExtras
#

#'
#' LogTibble
#'
#' @description
#' Class for managing log messages as a tibble.
#'
#' @field logTibble get as tibble
#'
#' @importFrom R6 R6Class
#' @importFrom tibble tibble add_row
#' @importFrom dplyr add_row
#'
#'
#' @export
#'
LogTibble <- R6::R6Class(
  classname = "LogTibble",
  private = list(
    log = NULL
  ),
  public = list(
    #'
    #' @description
    #' Initializes a new LogTibble object.
    initialize = function() {
      private$log <- tibble::tibble(
        type = factor(NA, levels = c("INFO", "WARNING", "ERROR", "SUCCESS")),
        step = character(0),
        message = character(0),
        .rows = 0L
      )
    },

    #' addLog
    #' @description
    #' Adds a log message to the log tibble.
    #'
    #' @param type     Type of log message ("INFO", "WARNING", "ERROR", "SUCCESS")
    #' @param step     Step or description associated with the log message
    #' @param message  Log message content
    #' @param ...      Additional parameters for message formatting
    addLog = function(type, step, message, ...) {
      private$log <- private$log |>
        dplyr::add_row(
          type = factor(type, levels = c("INFO", "WARNING", "ERROR", "SUCCESS")),
          step = step,
          message = paste(message, ...)
        )
    },

    #' INFO
    #' @description
    #' Adds an informational log message to the log tibble.
    #'
    #' @param step     Step or description associated with the log message
    #' @param message  Log message content
    #' @param ...      Additional parameters for message formatting
    INFO = function(step, message, ...) {
      self$addLog("INFO", step, message, ...)
    },

    #' WARNING
    #' @description
    #' Adds a warning log message to the log tibble.
    #'
    #' @param step     Step or description associated with the log message
    #' @param message  Log message content
    #' @param ...      Additional parameters for message formatting
    WARNING = function(step, message, ...) {
      self$addLog("WARNING", step, message, ...)
    },

    #' ERROR
    #' @description
    #' Adds an error log message to the log tibble.
    #'
    #' @param step     Step or description associated with the log message
    #' @param message  Log message content
    #' @param ...      Additional parameters for message formatting
    ERROR = function(step, message, ...) {
      self$addLog("ERROR", step, message, ...)
    },

    #' SUCCESS
    #' @description
    #' Adds an error log message to the log tibble.
    #'
    #' @param step     Step or description associated with the log message
    #' @param message  Log message content
    #' @param ...      Additional parameters for message formatting
    SUCCESS = function(step, message, ...) {
      self$addLog("SUCCESS", step, message, ...)
    },

    #' print
    #' @description
    #' prints log.
    print = function() {
      print(private$log)
    }
  ),
  active = list(
    logTibble = function() {
      return(private$log)
    }
  )
)