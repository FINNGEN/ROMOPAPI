
helper_FinnGen_getDatabaseFile <- function(){
   if( Sys.getenv("EUNOMIA_DATA_FOLDER") == "" ){
    message("EUNOMIA_DATA_FOLDER not set. Please set this environment variable to the path of the Eunomia data folder.")
    stop()
  }

  urlToFinnGenEunomiaZip <- "https://raw.githubusercontent.com/FINNGEN/EunomiaDatasets/main/datasets/FinnGenR12/FinnGenR12_v5.4.zip"
  eunomiaDataFolder <- Sys.getenv("EUNOMIA_DATA_FOLDER")

  # Download the database if it doesn't exist
  if (!file.exists(file.path(eunomiaDataFolder, "FinnGenR12_v5.4.zip")) | !file.exists(file.path(eunomiaDataFolder, "FinnGenR12_v5.4.sqlite"))){

    result <- utils::download.file(
      url = urlToFinnGenEunomiaZip,
      destfile = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.zip"),
      mode = "wb"
    )

    Eunomia::extractLoadData(
      from = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.zip"),
      to = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.sqlite"),
      cdmVersion = '5.4',
      verbose = TRUE
    )
  }

  # copy to a temp folder
  file.copy(
    from = file.path(eunomiaDataFolder, "FinnGenR12_v5.4.sqlite"),
    to = file.path(tempdir(), "FinnGenR12_v5.4.sqlite"),
    overwrite = TRUE
  )

  return(file.path(tempdir(), "FinnGenR12_v5.4.sqlite"))
}