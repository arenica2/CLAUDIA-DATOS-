
source("~/PETM-shiny/global.R")
source("~/PETM-shiny/loadSaveMethods.R")
#' Methods to get and put data
#' - MySQL
#' - Dropbox
#' - csv
library(DBI)
library(RMySQL)

createDropboxToken <- function() {
  file.copy(from = "dropboxOAuthToken",
            to = ".httr-oauth",
            overwrite = TRUE)
  
  #Dropbox directory name and token
  outputDir <- "PETM-Shiny"
  token <- drop_auth()
}

#'  gets the user params
get_user_params_mysql <- function(databaseName, username) {
  users_table   <- "search_users"
  groups_table   <- "search_groups"
  
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = dbGlobalConfig$host,
      port = dbGlobalConfig$port,
      user = dbGlobalConfig$user,
      password = dbGlobalConfig$password
    )
  userQuery <- sprintf("SELECT * FROM %s WHERE user_name COLLATE latin1_general_cs LIKE '%s'", users_table, username)
  userResult <- dbGetQuery(db, userQuery)
  
  groups_table <- "search_groups"
  groupQuery <- sprintf("SELECT * FROM %s WHERE GROUP_NAME COLLATE latin1_general_cs LIKE '%s'", groups_table, userResult$group_name)
  #print(groupQuery)
  groupResult <- dbGetQuery(db, groupQuery)
  dbDisconnect(db)
  
  if(nrow(groupResult) == 0) {
    cat("Group not found", groupResult$group_name, '\n')
    return(NULL)
  } 
  return(groupResult)
}

#' Insert a new group into the DB
#' e.g.
#' insert_group(databaseName = dbGlobalConfig$authDatabaseName, GROUP_NAME = "real_new", DATA_ID="RESULT_MELGAR_NEW", DIAMONDS = "1", CIRCLES = "1", CERTAINTY_CLOUD = "0")
insert_group <- function(databaseName, GROUP_NAME="", DATA_ID="", DIAMONDS="", CIRCLES="", CERTAINTY_CLOUD="") {  #RESULT_MELGAR
  groups_table   <- "search_groups"
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = dbGlobalConfig$host,
      port = dbGlobalConfig$port,
      user = dbGlobalConfig$user,
      password = dbGlobalConfig$password
    )
  groupQuery <- sprintf("INSERT into %s (GROUP_NAME, DATA_ID, DIAMONDS, CIRCLES, CERTAINTY_CLOUD) VALUES ('%s','%s','%s','%s','%s')", groups_table, GROUP_NAME, DATA_ID, DIAMONDS, CIRCLES, CERTAINTY_CLOUD)
  print(groupQuery)
  res <- dbGetQuery(db, groupQuery)
  print(res)
  dbDisconnect(db)
}


#' Insert a new user into the DB
#' e.g.
#' insert_user(databaseName = dbGlobalConfig$authDatabaseName, USER_NAME="test2", GROUP_NAME = "real_new")
insert_user <- function(databaseName, USER_NAME, GROUP_NAME) {  
  users_table   <- "search_users"
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = dbGlobalConfig$host,
      port = dbGlobalConfig$port,
      user = dbGlobalConfig$user,
      password = dbGlobalConfig$password
    )
  queryString <- sprintf("INSERT into %s (USER_NAME, GROUP_NAME) VALUES ('%s','%s')", users_table, USER_NAME, GROUP_NAME)
  print(queryString)
  res <- dbGetQuery(db, queryString)
  print(res)
  dbDisconnect(db)
}

#' Read all the files into a list
loadData <- function() {
  filesInfo <- drop_dir(outputDir, dtoken = token)
  filePaths <- filesInfo$path
  data <-
    lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}

#' logs the user in
log_in_username <- function(databaseName, username) {
  users_table   <- "search_users"
  # Connect to the database
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = dbGlobalConfig$host,
      port = dbGlobalConfig$port,
      user = dbGlobalConfig$user,
      password = dbGlobalConfig$password
    )
  
  # Construct the fetching query
  userQuery <- sprintf("SELECT * FROM %s WHERE user_name COLLATE latin1_general_cs LIKE '%s'", users_table, username)
  #print(userQuery)
  userResult <- dbGetQuery(db, userQuery)
  dbDisconnect(db)
  if(nrow(userResult) == 0) {
    return(FALSE) #no user found
  } else if (nrow(userResult) > 1) {
    cat("Duplicate entry for user", username, '\n')
    return(FALSE)
  }
  return(TRUE)
}

#' Load the map information from tableName
load_map_data_mysql <- function(databaseName, tableName) {
  # Connect to the database
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = dbGlobalConfig$host,
      port = dbGlobalConfig$port,
      user = dbGlobalConfig$user,
      password = dbGlobalConfig$password
    )
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", tableName)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

#' Saves data using .csv into a unique file name
save_search_data <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <-
    sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = outputDir)
}

#' Saves data from the 'Search Details Form' into a table
save_search_data_mysql <- function(data, DB_NAME="Chagas_Arequipa", TABLE_NAME="APP_INSPECTIONS") {
  db <-
    dbConnect(
      MySQL(),
      dbname = DB_NAME,
      host = dbGlobalConfig$host,
      port = dbGlobalConfig$port,
      user = dbGlobalConfig$user,
      password = dbGlobalConfig$password
    )
  #agregado franco
  print(paste(data[1,], collapse = "', '"))
  #############
  query <-
    sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      TABLE_NAME,
      paste(names(data), collapse = ", "),
      paste(data, collapse = "', '")
    )
  print(query)
  dbGetQuery(db, query)
  dbDisconnect(db)
}

#' Checks if the user is in the table search_users
validate_user_login_mysql <- function(username) {
  user_table <- "search_users"
  # Connect to the database
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = dbGlobalConfig$host,
      port = dbGlobalConfig$port,
      user = dbGlobalConfig$user,
      password = dbGlobalConfig$password
    )
  
  # Construct the fetching query
  query <- 
    sprintf("SELECT * FROM (SELECT CASE WHEN COUNT(*) = 1 THEN 'Success' ELSE 'Failure' END AS `Result` FROM %s WHERE user_name COLLATE latin1_general_cs  LIKE '%s')tbl"
            , user_table, username)
  # Submit the fetch query and disconnect
  #print(query)
  
  queryResult <- dbGetQuery(db, query)
  dbDisconnect(db)
  print(queryResult)
  return(queryResult)
}



#' Update data following the submit of inspection form
#' TODO: the table should not be hard-coded
#' 
update_inspected_data_mysql <- function(unicode, insFlag, TABLE_NAME='RESULT_MELGAR_NEW') {  #RESULT_MELGAR
  DB_NAME <- "Chagas_Arequipa"
  db <-
    dbConnect(
      MySQL(),
      dbname = DB_NAME,
      host = dbGlobalConfig$host,
      port = dbGlobalConfig$port,
      user = dbGlobalConfig$user,
      password = dbGlobalConfig$password
    )
  query <-
    sprintf(
      "UPDATE %s SET inspected = %s WHERE UNICODE = '%s'",
      TABLE_NAME,
      insFlag,
      unicode
    )
  print(query)
  dbGetQuery(db, query)
  dbDisconnect(db)
}

#' Update data in mysql
#' @param KEY = the private key of the row
#' @param NAME = the name of the row
#' @param COL = the column to update
#' @param VAK = the new val of the col
#' update_table('search_users', 'user_name', 'test1', 'group_name', 'real_model1')
#' update_table('search_groups', 'group_name', 'real_model2', 'group_name', 'real_model1')
#' 
update_table <- function(TABLE, KEY, NAME, COL, VAL, DB_NAME=dbGlobalConfig$databaseName) {  
  db <-
    dbConnect(
      MySQL(),
      dbname = DB_NAME,
      host = dbGlobalConfig$host,
      port = dbGlobalConfig$port,
      user = dbGlobalConfig$user,
      password = dbGlobalConfig$password
    )
  query <-
    sprintf(
      "UPDATE %s SET %s = '%s' WHERE %s = '%s'",
      TABLE,
      COL,
      VAL,
      KEY,
      NAME
    )
  print(query)
  dbGetQuery(db, query)
  dbDisconnect(db)
}


#' Upload a csv file as a table
#' WARNING: destructive
#' @param fpath = the names of the table
#' @param table_name = the name of the table
#' 
#' For example:
#' fpath <- 'C:/Users/agutf/encuesta_minsa_2017-01-10.csv'
#' upload_csv_table(fpath, table_name='MY_RESULTS', sep=',')
upload_csv_table <- function(fpath, table_name, sep=',', DB_NAME=dbGlobalConfig$databaseName) {  
  #cat('Loading ', fpath, '..\n')
  new_data <- read.csv(fpath, sep = sep)
  db <-
    dbConnect(
      MySQL(),
      dbname = DB_NAME,
      host = dbGlobalConfig$host,
      port = dbGlobalConfig$port,
      user = dbGlobalConfig$user,
      password = dbGlobalConfig$password
    )
  
  print('Deleting any existing table...')
  queryDrop <-
    sprintf(
      "DROP TABLE %s",
      table_name
    )
  print(queryDrop)
  tryCatch(dbGetQuery(db, queryDrop), finally=function(){})
  
  print('Inserting table...')
  success <- dbWriteTable(db, name=table_name, value=new_data, row.names=F)
  cat('Success: ', success, '\n')
  
  dbDisconnect(db)
}