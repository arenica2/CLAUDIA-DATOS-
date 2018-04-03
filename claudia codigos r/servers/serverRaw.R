#' VectorPoint 
#' Copyright (c) 2015-2017  VectorPoint team
#' See LICENSE.TXT for details
#' 
#' Server
#' 
#' Intruction:
#' 1. Before running the scripts, set the home directory with a command like
#' setwd('~/PETM-shiny') 

#install.packages(c(
#  'shiny', 'datasets', 'RColorBrewer', 'leaflet', 
#  'htmltools', 'dplyr', 'rgdal', 'rdrop2', 'data.table',
#  'RMySQL', 'png', 'rdrop2', 'binr', 'lattice',
#  'shinyjs', 'shinyBS', 'shinythemes'
#))

library(shiny)
library(datasets)
library(RColorBrewer)
library(leaflet)
library(htmltools)
library(lattice)
library(dplyr)
library(rgdal)
library(rdrop2)
library(data.table)
library(RMySQL)
library(shinyjs)
library(deldir)
library(sp)
library(data.table)
library(DBI)

#Set the file upload limit to 30 MB (Not Applicable) Dropbox only?
options(shiny.maxRequestSize = 30 * 1024 ^ 2)
set.seed(100)

shinyServer(function(input, output, session) {
  source("loadSaveMethods.R")
  #createDropboxToken()
  #' Loads the data from the DB and updates the 'locality' options
  
  #The authenatication system:
  groupParameters <- reactiveValues() #configuration of the app for the given group
  sessionData     <- reactiveValues()
  
  reconnected <- FALSE
  loginSuccess <- FALSE
  observeEvent(input$userLogin,{
    
    loginSuccess <- log_in_username(databaseName=dbGlobalConfig$authDatabaseName, input$username)
    reconnected <<- (input$reconnected == "true")
    
    if(loginSuccess) {
      usernameCheck <- 'Success'
      admin_user <- "Failure"
      
      #Variable para dar permiso al administrador
      #if ("ADMIN_UPCH"== toupper(input$username)) {
      
      #admin_user <- 'Success'   
      #data_mysql <- Report();
      
      #output$casa_regular <- renderText({paste("Casa regular:",data_mysql[["casa_regular"]])})
      #output$des <- renderText({paste("Deshabitadas:",data_mysql[["des"]])})
      #output$lp <- renderText({paste("LP:",data_mysql[["lp"]])})
      #output$lv <- renderText({paste("LV:",data_mysql[["lv"]])})
      #output$c <- renderText({paste("Cerradas:",data_mysql[["c"]])})
      #output$inspeccion <- renderText({paste("Inspeccionadas:",data_mysql[["inspeccion"]])})
      #output$r <- renderText({paste("Renuentes:",data_mysql[["r"]])})
      #output$v <- renderText({paste("V1:",data_mysql[["v"]])})
      #output$num_elements <- renderText({paste("No DE REGISTROS:",data_mysql[["num_elements"]])})
      
      #Eliminar
      #aux <- data_mysql[["data"]][-c(450),]
      
      #output$valores <- renderDataTable({aux}) 
      
      #localidades
      #code_localities <- unique(data_mysql[["data"]]$CODE_LOCALITY)
      #updateSelectizeInput(session, "locality2", choices = code_localities, server = TRUE)
      
      #if( !reconnected  )
      #updateSelectizeInput(session, "locality2", choices = code_localities, server = TRUE)
      #}
      
      output$activeUser <- renderUI({
        #strMsg <- paste("Conectado como <b> <i>",input$username)
        #HTML(paste(strMsg))
      })
      
      groupParametersTmp <- get_user_params_mysql(databaseName=dbGlobalConfig$authDatabaseName, input$username)
      groupParameters$GROUP_NAME      <- groupParametersTmp$GROUP_NAME
      groupParameters$DATA_ID         <- groupParametersTmp$DATA_ID
      groupParameters$DIAMONDS        <- groupParametersTmp$DIAMONDS
      groupParameters$CERTAINTY_CLOUD <- groupParametersTmp$CERTAINTY_CLOUD
      groupParameters$SIM_SEARCHES    <- groupParametersTmp$SIM_SEARCHES
      groupParameters$WRITE_ACCESS    <- (!is.na(groupParametersTmp$WRITE_ACCESS)) & (groupParametersTmp$WRITE_ACCESS == '1')
      print(as.list(groupParameters))
      
      #after the log-in, updates the loc selector for this user
      #wishlist: use updateSessionDate(session, sessionData)
      sessionData$searchdata <- loadSearchData(tableName=groupParameters$DATA_ID) 
      sessionData$localities <- unique(sessionData$searchdata$codeLoc)
      sessionData$houseId    <- '' #set when the user selects a house
      sessionData$palForRisk <- function(probab){return('#dummy')}
      if(!reconnected)
        updateSelectizeInput(session, "locality", choices = sessionData$localities, server = TRUE)
      
    } else {
      usernameCheck <- 'Failure'
      admin_user <- "Failure"
      # observe({
      #   session$sendCustomMessage(type = 'validation-message',
      #                             message = "Identificatión incognido")
      # })
    }
    
    output$validUser <- renderText({usernameCheck})
    output$adminUser <- renderText({admin_user})
    outputOptions(output, 'validUser', suspendWhenHidden = FALSE)  #keeps on top
    outputOptions(output, 'adminUser', suspendWhenHidden = FALSE)  #keeps on top
    
  })
  
  output$inspectButton <- renderUI({})
  
  #' Submit of inspection report
  observeEvent(input$inputSubmit, {
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    #Llenado campos obligatorios
    if (input$P == '' ||
        input$D == '' ||
        input$L == '' ||
        input$V == '' ) {
      observe({
        session$sendCustomMessage(type = 'confirm-message',
                                  message = "El unicode de la vivienda es obligatorio.")
        #output$out <- renderPrint(input$midata)
      })
      #Campo Obs_text de unicode equivocado  
    } else if ( input$observaciones == TRUE && input$obs_unicode == 5 && input$obs_text1=="") {
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "Ingrese el codigo correcto en la caja de texto")
      })
      #Campo Obs_text de unicode equivocado
    } else if ( input$observaciones == TRUE && input$obs_unicode == 8 && input$obs_text2=="") {
      
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "Ingrese la observación sobre el codigo en la caja de texto")
      })
      #Campo tipo de local publico es obligatorio cuando se selecciona LP
    } else if (input$caract_predio=="LP" && input$tipo_lp=="") {
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "Ingrese por favor el tipo de local publico")
      })
      #Campo motivo es obligatorio cuando se selecciona V
    } else if (input$status_inspec == 'V' && input$motivo_volver=="") {
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "Ingrese por favor el motivo por el cual se tiene que volver")
      }) 
      #Campo de texto es obligatorio cuando se selecciona Renuente R6
    } else if (input$status_inspec == 'R' && input$renuente=="R6" && input$renuente_otro=="") {
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "Explique por favor la causa de renuencia")
      }) 
    } else if (input$status_inspec == 'inspeccion' && input$lugar_inspeccion_intra==0 && input$lugar_inspeccion_peri==0) {
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "Ingrese por favor el lugar donde se realizo la inspeccion")
      })
    } else if ( (input$chiris_intra==1 || input$rastros_intra==1) && input$lugar_inspeccion_intra == 0 ) {
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "No puede marcar en chiris o rastros sin haber seleccionado el lugar de inspección adecuado ")
      })
    } else if ( (input$chiris_peri==1 || input$rastros_peri==1) && input$lugar_inspeccion_peri == 0 ) {
      observe({
        session$sendCustomMessage(type = 'validation-message',
                                  message = "No puede marcar en chiris o rastros sin haber seleccionado el lugar de inspección adecuado ")
      })
    } else {
      #wishlist: we want to run something like https://github.com/AugustT/shiny_geolocation
      inputData <- recordInspectionFields()
      
      inputData$USER_NAME      = input$username
      inputData$GROUP_NAME     = groupParameters$GROUP_NAME
      inputData$DATA_ACTION    = 'INSPECTION_NEW'  
      #future: lat and lon during this operation
      
      #record the model-based probability of infestation
      PREDICTED_PROBAB <- sessionData$searchdata[UNICODE==inputData$UNI_CODE, probability]
      PREDICTED_PROBAB <- PREDICTED_PROBAB[1] #in case there are multiple matches - wishlist: raise an error
      inputData$PREDICTED_PROBAB      <- PREDICTED_PROBAB
      inputData$PREDICTED_PROBAB_MEAN <- mean(sessionData$searchdata[, probability], na.rm = T) #reference probability for this dataset
      
      inputData$PREDICTED_COLOR       <- which(sessionData$riskColors == sessionData$palForRisk(PREDICTED_PROBAB))
      inputData$PREDICTED_COLOR       <- sessionData$palForRisk(PREDICTED_PROBAB)
      
      inputData$HORA_FIN <- as.character(Sys.time())
      
      if (! log_in_username(databaseName=dbGlobalConfig$authDatabaseName, username = input$username)) {
        cat('Cannot log in username: ', input$username, '\n')
        session$sendCustomMessage(type = 'action-message',
                                  message = "buscando_false")
        return(NULL)
      }
      if(groupParameters$WRITE_ACCESS) {
        
        save_search_data_mysql(inputData, TABLE_NAME = dbGlobalConfig$inspectionsTable)
        cache_table_IO(operation = 'delete', USER_NAME = input$username, CACHE_TYPE = 'inspection')
        
        observe({
          session$sendCustomMessage(type = 'validation-message',
                                    message = "Los datos fueron GUARDADOS con éxito. Gracias.")
        })
        cleanData()
        
        houseinLoc <- getLocalityData()
        leafletProxy("map", data = houseinLoc) %>% 
          addCircleMarkers(
            radius = 8
            ,color = sessionData$palForTime(houseinLoc[, time])
            ,stroke = FALSE
            ,fillOpacity = ifelse(groupParameters$CERTAINTY_CLOUD == 1, .3, .0)
            ,layerId = ~ UNICODE
            ,popup = paste(
              "<b>", houseinLoc[, UNICODE], "</b><br>",
              #Cambiando color antes era "blue" ahora es "black"
              "Ult. visita:","<b style='color: black;'>", houseinLoc[, inspectionText],"</b>"
            )
          )%>%
          addCircleMarkers(
            fillColor = YlOrRd.q(houseinLoc[, probability]),
            radius = 4,
            stroke = TRUE,
            color = "black",
            weight = .4,
            fillOpacity = 1
          )
        
      } else {
        observe({
          session$sendCustomMessage(type = 'validation-message',
                                    message = "ALERTA!! Los datos no pudieron ser guardados con éxito, por favor revise")
        })
      }
      
    }
    
    #toggleModal(session, "inputForm")
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
  })
  
  #' Limpiando valores
  observeEvent(input$inputClear, {
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    cleanData()
    cache_table_IO(operation = 'delete', USER_NAME = input$username, CACHE_TYPE = 'inspection')
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
  })
  
  
  #TODO: warn if fecha does not comply with the format YYYY-MM-DD
  # observeEvent(input$fecha, {
  #   fecha <- as.Date(input$fecha, format='%Y-%m-%d')
  #   session$sendCustomMessage(type = 'validation-message',
  #                             message = "Entra fecha e.g. 1998-12-31")
  # })
  
  #' graba los valores de input durante o despues de inspeccion
  recordInspectionFields <- function() {
    inputData <-
      list(
        #Almacenando campos
        "UNI_CODE" = paste(input$P,input$D,toupper(input$L),toupper(input$V),sep = "."),
        "CODE_LOCALITY" = paste(input$P,input$D,toupper(input$L),sep = "."),
        "OBS_UNICODE" = ifelse(input$observaciones == TRUE,input$obs_unicode, NA),
        "OBS_TEXT" = NA,
        "FECHA" = as.character(input$fecha),
        "CARACT_PREDIO" = input$caract_predio,
        "TIPO_LP" = NA,
        "STATUS_INSPECCION" = NA,
        "ENTREVISTA" = NA,
        "MOTIVO_VOLVER" = NA,
        "RENUENTE" = NA,
        "INTRA_INSPECCION" = NA,
        "INTRA_CHIRIS" = NA,
        "INTRA_RASTROS" = NA,
        "PERI_INSPECCION" = NA,
        "PERI_CHIRIS" = NA,
        "PERI_RASTROS" = NA,
        #eliminar luego de la base de datos, por que ya no se esta usando
        "LUGAR_INSPECCION" = NA,
        #-------------------------
        #eliminar luego
        "TOT_INTRA" = NA,
        "TOT_PERI" = NA,
        "RASTROS" = NA,
        #--------------
        "PERSONAS_PREDIO" = NA,
        
        "CANT_PERROS" = NA,
        "CANT_GATOS" = NA,
        "CANT_AVES_CORRAL" = NA,
        "CANT_CUYES" = NA,
        "CANT_CONEJOS" = NA,
        "TEXT_OTROS" = NA,
        "CANT_OTROS" = NA,
        "TEST_DATA"      = ifelse(input$testData, 1, 0),
        "HORA_INICIO" = as.character(date_start)
      )
    
    #Almacenar obs_text
    if (input$observaciones == TRUE) { 
      if (input$obs_unicode==5) {
        inputData$OBS_TEXT = input$obs_text1
      }else if (input$obs_unicode==8) {
        inputData$OBS_TEXT = input$obs_text2
      }
    }
    
    if(input$caract_predio == 'casa_regular' || input$caract_predio == 'LP' || input$caract_predio == 'DES'){
      inputData$STATUS_INSPECCION <- input$status_inspec
      
      #Si es local publico se pone que tipo es
      if (input$caract_predio == 'LP') {
        inputData$TIPO_LP <- (input$tipo_lp)
      }
      
      #Campo ENTREVISTA
      if(input$status_inspec =='entrevista'){
        inputData$ENTREVISTA <- input$entrevista
      }
      
      #Campo MOTIVOS_VOLVER
      if (input$status_inspec == 'V') {
        inputData$MOTIVO_VOLVER <- input$motivo_volver
      }
      
      #Campo RENUENTE
      if (input$status_inspec == "R") { 
        if (input$renuente=="R6") {
          inputData$RENUENTE = input$renuente_otro
        }else {
          inputData$RENUENTE = input$renuente
        }
      }
      
      if(input$status_inspec == 'inspeccion') {
        #Lugar de inspeccion
        inputData$INTRA_INSPECCION <- as.integer(input$lugar_inspeccion_intra)
        inputData$PERI_INSPECCION <- as.integer(input$lugar_inspeccion_peri)
        
        #Chiris en intra o peri
        inputData$INTRA_CHIRIS <- as.integer(input$chiris_intra)
        inputData$PERI_CHIRIS <- as.integer(input$chiris_peri)
        
        #Rastros en intra o peri
        inputData$INTRA_RASTROS <- as.integer(input$rastros_intra)
        inputData$PERI_RASTROS <- as.integer(input$rastros_peri)
        
        #Personas predio
        inputData$PERSONAS_PREDIO <- (input$personas_predio)
        
        inputData$CANT_PERROS <- 0
        inputData$CANT_GATOS <- 0
        inputData$CANT_AVES_CORRAL <- 0
        inputData$CANT_CUYES <- 0
        inputData$CANT_CONEJOS <- 0
        inputData$TEXT_OTROS <- 0
        inputData$CANT_OTROS <- 0
        
        if (input$perros) {
          inputData$CANT_PERROS <- (input$cant_perros)
        }
        if (input$gatos) {
          inputData$CANT_GATOS <- (input$cant_gatos)
        }
        if (input$aves_corral) {
          inputData$CANT_AVES_CORRAL <- (input$cant_aves_corral)
        }
        if (input$cuyes) {
          inputData$CANT_CUYES <- (input$cant_cuyes)
        }
        if (input$conejos) {
          inputData$CANT_CONEJOS <- (input$cant_conejos)
        }
        if (input$otros) {
          inputData$TEXT_OTROS <- (input$text_otros)
          inputData$CANT_OTROS <- (input$cant_otros)
        }
      }
    }
    return(inputData)
  }    
  
  #Funcion para limpiar datos
  cleanData <- function(){
    #Almacena el tiempo en que se empieza a ingresar la información
    date_start <<- Sys.time()
    
    updateNumericInput(session, "P", value = 1)
    updateNumericInput(session, "D", value = "")
    updateTextInput(session, "L", value = "")
    updateTextInput(session, "V", value = "")
    updateCheckboxInput(session,"observaciones", "Observaciones", FALSE)
    updateSelectInput(session, "obs_unicode", selected = "1")
    updateTextAreaInput(session, "obs_text1", value = "")
    updateTextAreaInput(session, "obs_text2", value = "")
    updateDateInput(session, "fecha", value = Sys.Date())
    updateSelectInput(session, "caract_predio", selected = "casa_regular")
    updateTextInput(session, "tipo_lp", value = "")
    updateSelectInput(session, "status_inspec", selected = "C")
    updateRadioButtons(session, "entrevista", selected = "cree_no_tiene")
    updateTextAreaInput(session, "motivo_volver", value = "")
    updateTextAreaInput(session, "renuente_otro", value = "")
    updateCheckboxInput(session,"lugar_inspeccion_intra", NULL, FALSE)
    updateCheckboxInput(session,"lugar_inspeccion_peri", NULL, FALSE)
    updateCheckboxInput(session,"chiris_intra", NULL, FALSE)
    updateCheckboxInput(session,"chiris_peri", NULL, FALSE)
    updateCheckboxInput(session,"rastros_intra", NULL, FALSE)
    updateCheckboxInput(session,"rastros_peri", NULL, FALSE)
    updateNumericInput(session, "personas_predio", value = 1)
    updateCheckboxInput(session,"perros", "Perros", FALSE)
    updateCheckboxInput(session,"gatos", "Gatos", FALSE)
    updateCheckboxInput(session,"aves_corral", "Aves de corral", FALSE)
    updateCheckboxInput(session,"cuyes", "Cuyes", FALSE)
    updateCheckboxInput(session,"conejos", "Conejos", FALSE)
    updateCheckboxInput(session,"otros", "Otros", FALSE)
    #warning: they have default value of 1, but, unless a box is checked, the recorded value is 0
    #Esto es para que el ingreso de datos sea mas rapido.
    updateNumericInput(session, "cant_perros", value = 1)
    updateNumericInput(session, "cant_gatos", value = 1)
    updateNumericInput(session, "cant_aves_corral", value = 1)
    updateNumericInput(session, "cant_cuyes", value = 1)
    updateNumericInput(session, "cant_conejos", value = 1)
    updateTextInput(session, "text_otros", value = "")
    updateNumericInput(session, "cant_otros", value = 1)
    #Limpiando prueba
    #updateCheckboxInput(session, "testData", "Prueba", FALSE)
  }
  
  #' load inspection data from cache into input fields
  #' if no cache, does nothing
  uncacheInspectionData <- function(){
    
    inputData <- cache_table_IO(operation = 'load',  USER_NAME = input$username,  CACHE_TYPE = 'inspection')
    
    if(is.null(inputData)) {
      return(FALSE)
    }
    
    UNI_CODE <- strsplit(inputData[['UNI_CODE']], split = '\\.')[[1]]
    updateNumericInput(session, "P", value = UNI_CODE[[1]])
    updateNumericInput(session, "D", value = UNI_CODE[[2]])
    updateTextInput(session, "L", value = UNI_CODE[[3]])
    updateTextInput(session, "V", value = UNI_CODE[[4]])
    updateCheckboxInput(session,"observaciones", value = ifelse(is.na(inputData[["OBS_UNICODE"]]), FALSE, TRUE))
    updateSelectInput(session, "obs_unicode", selected = ifelse(is.na(inputData[["OBS_UNICODE"]]), "1", inputData[["OBS_UNICODE"]]))#"1")
    updateTextAreaInput(session, "obs_text1", value = ifelse(inputData[["OBS_UNICODE"]][1] == "5", inputData[["OBS_TEXT"]],""))
    updateTextAreaInput(session, "obs_text2", value = ifelse(inputData[["OBS_UNICODE"]][1] == "8", inputData[["OBS_TEXT"]],""))
    updateDateInput(session, "fecha", value = inputData[["FECHA"]])
    updateSelectInput(session, "caract_predio", selected = inputData[["CARACT_PREDIO"]])# "casa_regular")
    updateTextInput(session, "tipo_lp", value = ifelse(is.na(inputData[["TIPO_LP"]]), "", inputData[["TIPO_LP"]]))
    updateSelectInput(session, "status_inspec", selected = ifelse(is.na(inputData[["STATUS_INSPECCION"]]),"C",inputData[["STATUS_INSPECCION"]]))#"C")
    updateRadioButtons(session, "entrevista", selected = ifelse(is.na(inputData[["ENTREVISTA"]]),"cree_no_tiene",inputData[["ENTREVISTA"]] ))#"cree_no_tiene")
    updateTextAreaInput(session, "motivo_volver", value = ifelse(is.na(inputData[["MOTIVO_VOLVER"]]), "", inputData[["MOTIVO_VOLVER"]]))
    updateSelectInput(session, "renuente", selected = ifelse(is.na(inputData[["RENUENTE"]]), "R1", ifelse( inputData[["RENUENTE"]]=="R1" || inputData[["RENUENTE"]]=="R2" || inputData[["RENUENTE"]]=="R3" || inputData[["RENUENTE"]]=="R4" || inputData[["RENUENTE"]]=="R5", inputData[["RENUENTE"]], "R6" ) ))
    updateTextAreaInput(session, "renuente_otro", value = ifelse(inputData[["RENUENTE"]]!="R1" || inputData[["RENUENTE"]]!="R2" || inputData[["RENUENTE"]]!="R3" || inputData[["RENUENTE"]]!="R4" || inputData[["RENUENTE"]]!="R5" , inputData[["RENUENTE"]], ""))
    updateCheckboxInput(session,"lugar_inspeccion_intra", NULL, value = ifelse(is.na(inputData[["INTRA_INSPECCION"]]), FALSE, as.logical(inputData[["INTRA_INSPECCION"]])))
    updateCheckboxInput(session,"chiris_intra", NULL, value = ifelse(is.na(inputData[["INTRA_CHIRIS"]]), FALSE, as.logical(inputData[["INTRA_CHIRIS"]])))
    updateCheckboxInput(session,"rastros_intra", NULL, value = ifelse(is.na(inputData[["INTRA_RASTROS"]]), FALSE, as.logical(inputData[["INTRA_RASTROS"]])))
    updateCheckboxInput(session,"lugar_inspeccion_peri", NULL, value = ifelse(is.na(inputData[["PERI_INSPECCION"]]), FALSE, as.logical(inputData[["PERI_INSPECCION"]])))
    updateCheckboxInput(session,"chiris_peri", NULL, value = ifelse(is.na(inputData[["PERI_CHIRIS"]]), FALSE, as.logical(inputData[["PERI_CHIRIS"]])))
    updateCheckboxInput(session,"rastros_peri", NULL, value = ifelse(is.na(inputData[["PERI_RASTROS"]]), FALSE, as.logical(inputData[["PERI_RASTROS"]])))
    updateNumericInput(session, "personas_predio", value = inputData[["PERSONAS_PREDIO"]])#1)
    updateCheckboxInput(session,"perros", "Perros", value = ifelse(is.na(inputData[["CANT_PERROS"]]) || inputData[["CANT_PERROS"]]==0, FALSE, TRUE))
    updateCheckboxInput(session,"gatos", "Gatos", value = ifelse(is.na(inputData[["CANT_GATOS"]]) || inputData[["CANT_GATOS"]]==0, FALSE, TRUE))
    updateCheckboxInput(session,"aves_corral", "Aves de corral", value = ifelse(is.na(inputData[["CANT_AVES_CORRAL"]]) || inputData[["CANT_AVES_CORRAL"]]==0, FALSE, TRUE))
    updateCheckboxInput(session,"cuyes", "Cuyes", value = ifelse(is.na(inputData[["CANT_CUYES"]]) || inputData[["CANT_CUYES"]]==0, FALSE, TRUE))
    updateCheckboxInput(session,"conejos", "Conejos", value = ifelse(is.na(inputData[["CANT_CONEJOS"]]) || inputData[["CANT_CONEJOS"]]==0, FALSE, TRUE))
    updateCheckboxInput(session,"otros", "Otros", value = ifelse(is.na(inputData[["CANT_OTROS"]]) || inputData[["CANT_OTROS"]]==0, FALSE, TRUE))
    updateNumericInput(session, "cant_perros", value = ifelse(is.na(inputData[["CANT_PERROS"]]) || inputData[["CANT_PERROS"]]==0, 1, inputData[["CANT_PERROS"]]))
    updateNumericInput(session, "cant_gatos", value = ifelse(is.na(inputData[["CANT_GATOS"]]) || inputData[["CANT_GATOS"]]==0, 1, inputData[["CANT_GATOS"]]))
    updateNumericInput(session, "cant_aves_corral", value = ifelse(is.na(inputData[["CANT_AVES_CORRAL"]]) || inputData[["CANT_AVES_CORRAL"]]==0, 1, inputData[["CANT_AVES_CORRAL"]]))
    updateNumericInput(session, "cant_cuyes", value = ifelse(is.na(inputData[["CANT_CUYES"]]) || inputData[["CANT_CUYES"]]==0, 1, inputData[["CANT_CUYES"]]))
    updateNumericInput(session, "cant_conejos", value = ifelse(is.na(inputData[["CANT_CONEJOS"]]) || inputData[["CANT_CONEJOS"]]==0, 1, inputData[["CANT_CONEJOS"]]))
    updateTextInput(session, "text_otros", value = ifelse(is.na(inputData[["TEXT_OTROS"]]), "", inputData[["TEXT_OTROS"]]))
    updateNumericInput(session, "cant_otros", value = ifelse(is.na(inputData[["CANT_OTROS"]]) || inputData[["CANT_OTROS"]]==0, 1, inputData[["CANT_OTROS"]]))
    return(TRUE)
  }
  
  observeEvent(input$enterData,{
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    var_bool <- uncacheInspectionData()
    if(!var_bool) {
      #Almacena el tiempo en que se empieza a ingresar la información
      date_start <<- Sys.time()
    }
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
    #shows the menu ...
    
  })
  
  observeEvent(input$inputCacheInspection,{
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    
    if (input$P == '' ||
        input$D == '' ||
        input$L == '' ||
        input$V == '' ) {
      observe({
        session$sendCustomMessage(type = 'confirm-message',
                                  message = "El unicode de la vivienda es obligatorio.")
        #output$out <- renderPrint(input$midata)
      })
    } else {
      inputData <- recordInspectionFields()
      cache_table_IO(operation = 'save', cached_data = inputData, USER_NAME = input$username,  CACHE_TYPE = 'inspection')
      observe({
        session$sendCustomMessage(type = 'confirm-message',
                                  message = "Los datos quedaran CONSERVADOS.")
      })
    }
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
  })
  
  #clean the fields. wishlist: merge with cleanup below
  observeEvent(input$logout,{
    updateTextInput(session, "username", value = "")
    
    output$userMessage <- renderText("")
    updateTextInput(session, "P", value = "")
    updateTextInput(session, "D", value = "")
    updateTextInput(session, "L", value = "")
    updateTextInput(session, "V", value = "")
    updateDateInput(session, "fecha", value = Sys.Date())
    updateSelectInput(session, "select", selected = 1)
    updateTextInput(session, "direccion", value = "")
    
    output$map <- renderLeaflet({
      leaflet() %>%  clearMarkerClusters() %>%
        clearShapes() %>% clearMarkers %>% clearControls()
    })
    output$houseId <- renderText({paste("")})
    output$userMessage <- renderText({paste("")})
    output$validUser <- renderText({""})
  })
  
  source("palettes.R")
  
  ## Interactive Map ##
  # Create the map
  output$map <- renderLeaflet({
    
    leaflet(options = leafletOptions(maxZoom=18)) %>%
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.mqap-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      #addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE) ) %>%
      setView(
        lng = mean(-71.55001667),
        lat = mean(-16.39249883),
        zoom = 16
      )
    
  })
  #wishlist: possibly try to prevent disappearing tiles when zooming
  #http://stackoverflow.com/questions/42438569/circles-disappear-on-mobile-when-map-is-zoomed
  
  getLocalityData <- function(){
    houseinLoc <- sessionData$searchdata[codeLoc %in% (input$locality)]
    
    #Tiempo (anos) : Colores de blanco hasta gris
    sessionData$palForTime <- colorFactor(c("white","gray90","gray80","gray75","gray70","gray65","gray60","gray55","gray40", "gray30", "gray20"), 
                                          domain = c(0,1,2,3,4,5,6,7,8,9,10))  #years.  wishlist: make this data-dependent with houseinLoc[, time]
    sessionData$palForRisk <- unique_colorQuantile(palette = "YlOrRd", domain=houseinLoc[, probability], n = 5)
    
    sessionData$riskNames  <- c("Mas Bajo", "Bajo", "Medio", "Alto", "Mas Alto")
    sessionData$riskColors <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026") # "#808080" #1..5, NA
    
    lastInspections <- read_past_inspections(databaseName=dbGlobalConfig$authDatabaseName, UNICODE=houseinLoc$UNICODE)
    lastInspections[,positive:=(TOT_INTRA>0)|(TOT_PERI>0)|(!RASTROS=='0')]
    #wishlist: protect the html from bad data in the fecha field
    
    #Completar el status a palabra entera
    lastInspections[lastInspections$STATUS_INSPECCION == "inspeccion",4] <- "inspección"
    lastInspections[lastInspections$STATUS_INSPECCION == "C",4] <- "cerrada"
    lastInspections[lastInspections$STATUS_INSPECCION == "R",4] <- "renuente"
    lastInspections[lastInspections$STATUS_INSPECCION == "V",4] <- "volver"
    
    lastInspections[, inspectionText:=paste0(FECHA, ': ', STATUS_INSPECCION)]
    #Se quito el color rojo y se puso color negro
    lastInspections[positive==T, inspectionText:=paste0('<font  color="black">', inspectionText, '</font>')]
    
    lastInspections[positive==F, inspectionText:=paste0('<font            >', inspectionText, '</font>')]
    houseinLoc <- merge(houseinLoc, lastInspections[,.(UNICODE,inspectionText)], all.x=T, by='UNICODE')
    houseinLoc[is.na(houseinLoc$inspectionText), inspectionText:='--']
    return(houseinLoc)
  }
  
  
  # Load the interactive map on click
  observeEvent(input$load, {
    source("delaunayTriangulationFunctions.R")
    
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    if (!is.null(input$locality)) {
      houseinLoc <- getLocalityData()
      
      ## CAUSED PROBLEMS BEFORE, SIMPLE WORKAROUND HERE
      # display best polygon (incentives experiment)
      copy <- loadSearchData(tableName=groupParameters$DATA_ID) 
      dataInspecciones <- findInspecciones(copy)
      listTriangulation <- getTriangulation(copy, dataInspecciones)
      polyDF <- listTriangulation[[1]]
      polyVertices <- listTriangulation[[2]]
      
      #iBestTri <- which(polyDF$nHouseUninspect == max(polyDF$nHouseUninspect))[1]
      
    } else {
      session$sendCustomMessage(type = 'action-message',
                                message = "buscando_false")
      return();
    }
    
    print(groupParameters$DATA_ID)
    print(groupParameters$DIAMONDS)
    print(groupParameters$CERTAINTY_CLOUD)
    
    #searchdata: previously loaded at log in
    
    output$houseId <- renderText({paste("")})
    output$userMessage <- renderText({paste("")})
    output$houseProbab <- renderText({paste("")})
    
    # Subset data based on the localities chosen
    if(groupParameters$DIAMONDS == 1) {
      #create gem icons
      ruby <- makeIcon( iconUrl = "Gem.png",
                        iconWidth = 20, iconHeight = 20,
                        iconAnchorX = 10, iconAnchorY = 5)
      
      diamond <- makeIcon( iconUrl = "Topaz.png",
                           iconWidth = 25, iconHeight = 25,
                           iconAnchorX = 10, iconAnchorY = 20 )
      #wishlist: create legend for gems
      #html_legend <- "<img src='http://icons.iconarchive.com/icons/aha-soft/jewelry/256/Gem-icon.png'height = '20' width = '20'>high uncertainty<br/>
      #<img src='http://icons.iconarchive.com/icons/aha-soft/jewelry/256/Topaz-icon.png'height = '20' width = '20'>high probability" 
      
    }
    
    leafletMap <- leafletProxy("map", data = houseinLoc) %>% 
      clearMarkerClusters() %>%
      addTiles() %>% 
      clearShapes() %>% clearMarkers %>% clearControls() %>%
      setView(
        lng = mean(houseinLoc[, LONGITUDE]),
        lat = mean(houseinLoc[, LATITUDE]),
        
        zoom = 16
      )
    
    for (z in 1:length(polyVertices)) {
      
      leafletMap %>% addPolygons(
        polyVertices[[z]]$y,
        polyVertices[[z]]$x,
        fill=TRUE,
        fillColor="transparent",
        weight=1,
        highlightOptions = highlightOptions(color = "white", weight = 1,
                                            fill=TRUE, fillColor="white",
                                            bringToFront = FALSE, fillOpacity = .4),
        
        label = paste0(as.character(polyDF$nHouseUninspect[z])),
        labelOptions = labelOptions(noHide = T, textsize = "15px")
      )
      
    }
    
    leafletMap %>%
      addCircleMarkers(
        radius = 8
        ,color = sessionData$palForTime(houseinLoc[, time])
        ,stroke = FALSE
        ,fillOpacity = ifelse(groupParameters$CERTAINTY_CLOUD == 1, .3, .0)
        ,layerId = ~ UNICODE
        ,popup = paste(
          "<b>", houseinLoc[, UNICODE], "</b><br>",
          #Cambiando color antes era "blue" ahora es "black"
          "Ult. visita:","<b style='color: black;'>", houseinLoc[, inspectionText],"</b>"
        )
      ) %>%
      addCircleMarkers(
        fillColor = YlOrRd.q(houseinLoc[, probability]),
        radius = 4,
        stroke = TRUE,
        color = "black",
        weight = .4,
        fillOpacity = 1
      ) %>%
      addLegend(
        "bottomleft",
        colors = sessionData$riskColors,
        labels = sessionData$riskNames,
        opacity = 1,
        title = "Riesgo de Infestacion"
      )
    
    # Old legend replaced October 20, 2017
    #    addLegend(
    #      "bottomleft",
    #      pal = sessionData$palForRisk,
    #      values = houseinLoc[, probability],
    #      #Poniendo nombres a los intervalos de la leyenda
    #      labFormat = labelLegend(sessionData$riskNames),
    #      opacity = 1,
    #      title = "Riesgo de Infestación"
    #    )
    
    if(groupParameters$DIAMONDS == 1) {
      leafletMap <- leafletMap %>%
        addMarkers(
          lat = subset(houseinLoc, probability >0.5)$LATITUDE,
          lng = subset(houseinLoc, probability >0.5)$LONGITUDE,
          icon = diamond
        )  %>%
        #add ruby markers for high uncertainty or time
        addMarkers(
          lat = subset(houseinLoc, uncertainty >.1)$LATITUDE, #wishlist - we are trying to switch uncertainty for time
          lng = subset(houseinLoc, uncertainty >.1)$LONGITUDE,
          icon = ruby
        )  %>%
        #wishlist - need to correct the data in the time column
        #addMarkers(
        #  lat = subset(houseinLoc, time >1)$LATITUDE,
        #  lng = subset(houseinLoc, time >1)$LONGITUDE,
        #  icon = ruby
        #)  %>%
        addControl(html = html_legend, position = "bottomleft")
    }
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
    return(leafletMap)
  })
  
  observeEvent(input$map_marker_click, {
    #http://stackoverflow.com/questions/28938642/marker-mouse-click-event-in-r-leaflet-for-shiny
    #MAPID_marker_click
    click<-input$map_marker_click
    if(is.null(click)) {
      return()
    }
    sessionData$houseId <- click$id
    output$houseId <- renderText(sessionData$houseId)
    output$inspectionUserMessage <- renderText("")
  })
  
  #' when a user selects a house
  observeEvent(sessionData$houseId, {
    if(sessionData$houseId == ''){
      return();
    }
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    #purge the cache - only direct return to "Enter Data" will load the cache
    cache_table_IO(operation = 'delete', USER_NAME = input$username, CACHE_TYPE = 'inspection')
    #cleanData()
    PDLV <- base::strsplit(sessionData$houseId, '\\.')[[1]]
    updateTextInput(session, "P", value = PDLV[1])
    updateTextInput(session, "D", value = PDLV[2])
    updateTextInput(session, "L", value = PDLV[3])
    updateTextInput(session, "V", value = PDLV[4])
    
    PREDICTED_PROBAB <- sessionData$searchdata[UNICODE==sessionData$houseId, probability]
    
    if(groupParameters$SIM_SEARCHES == 0) {
      output$inspectButton <- renderUI({})
      output$houseProbab <- renderText( sprintf('probab: %.2f', PREDICTED_PROBAB)  )
      #wishlist: show in popup
    } else {
      output$inspectButton <- renderUI(actionButton("sim_inspect_house_button", label="", icon = icon("search", "fa-.5x")))
      output$houseProbab <- renderText({paste("")})
    }
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
  })
  
  observeEvent(input$gps, {
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    if (!is.null(input$lat)) {
      output$userMessage <- renderText({ paste("") })
      leafletProxy("map") %>% addCircles(
        color = "blue",
        radius = 5,      #in meters
        lng = input$long,
        lat = input$lat
        #icon = icon("circle", "fa-.5x")
      ) %>% setView(lng = input$long,
                    lat = input$lat,
                    zoom = 20)
    } else {
      output$houseId <- renderText({paste("")})
      output$userMessage <- renderText({ paste("Location data unavailable") })
    }
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
  })
  
  observeEvent(input$sim_inspect_house_button, {
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    if(sessionData$houseId == ''){
      session$sendCustomMessage(type = 'action-message',
                                message = "buscando_false")
      return();
    }
    inputData <- list(
      'USER_NAME'      = input$username,
      'GROUP_NAME'     = groupParameters$GROUP_NAME,
      'DATA_ACTION'    = 'INSPECTION_CLICK',
      "UNI_CODE"       = sessionData$houseId,
      "FECHA"          = as.character(input$fecha),
      "INSPECTION_FLAG"= "inspected",
      "TEST_DATA"      = ifelse(input$testData, 1, 0)
      #wishlist: gps lat/lon
    )
    if (! log_in_username(databaseName=dbGlobalConfig$authDatabaseName, username = input$username)) {
      cat('Cannot log in username: ', input$username, '\n')
      session$sendCustomMessage(type = 'action-message',
                                message = "buscando_false")
      return(NULL)
    }
    #record the model-based probability of infestation
    PREDICTED_PROBAB <- sessionData$searchdata[UNICODE==inputData$UNI_CODE, probability]
    PREDICTED_PROBAB <- PREDICTED_PROBAB[1] #in case there are multiple matches - wishlist: raise an error
    inputData$PREDICTED_PROBAB      <- PREDICTED_PROBAB
    inputData$PREDICTED_PROBAB_MEAN <- mean(sessionData$searchdata[, probability], na.rm = T) #reference probability for this dataset
    
    if(groupParameters$WRITE_ACCESS) {
      save_search_data_mysql(inputData, TABLE_NAME = dbGlobalConfig$simulationsTable)
      #output$inspectionUserMessage <- renderText("Detalles grabados!")
    } else {
      #output$inspectionUserMessage <- renderText("Para grabar, pide acceso de escritura")
    }
    
    riskLevel <- which(sessionData$riskColors == sessionData$palForRisk(PREDICTED_PROBAB))
    output$houseProbab <- renderText(
      #(sprintf(  '<div>Riesgo: <font color="%s">%s</font></div>', sessionData$palForRisk(PREDICTED_PROBAB), sessionData$riskNames[riskLevel]))
      (sprintf(  '<div>Riesgo: <font           >%s</font></div>', sessionData$riskNames[riskLevel]))
      #sprintf('probab: %.2f', PREDICTED_PROBAB)
      #sprintf('%s probab: %.2f', inputData$UNI_CODE, PREDICTED_PROBAB)
    )
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
    #http://stackoverflow.com/questions/24265980/reset-inputs-button-in-shiny-app
  })
  
  #REPORTE para admin
  Report <- function(date=NULL){
    result <- list()
    result[["data"]] <- "No se encontraron datos"
    result[["casa_regular"]] <- 0
    result[["des"]] <- 0
    result[["lp"]] <- 0
    result[["lv"]] <- 0
    result[["c"]] <- 0
    result[["inspeccion"]] <- 0
    result[["r"]] <- 0
    result[["v"]] <- 0
    
    data <- load_data_mysql(databaseName = dbGlobalConfig$databaseName,
                            tableName    = dbGlobalConfig$inspectionsTable, date)
    
    num_elements <- nrow(data)
    if (num_elements != 0) {
      casa_regular <- nrow(data[data$CARACT_PREDIO == "casa_regular",])
      des <-   nrow(data[data$CARACT_PREDIO == "DES",])
      lp <- nrow(data[data$CARACT_PREDIO == "LP",])
      lv <- nrow(data[data$CARACT_PREDIO == "LV",])
      c <- nrow(data[data$STATUS_INSPECCION == "C",])
      inspeccion <- nrow(data[data$STATUS_INSPECCION == "inspeccion",])
      r <- nrow(data[data$STATUS_INSPECCION == "R",])
      v <- nrow(data[grepl("V1",data$MOTIVO_VOLVER, fixed = TRUE),])
      
      result[["data"]] <- data
      result[["casa_regular"]] <- casa_regular
      result[["des"]] <- des
      result[["lp"]] <- lp
      result[["lv"]] <- lv
      result[["c"]] <- c
      result[["inspeccion"]] <- inspeccion
      result[["r"]] <- r
      result[["v"]] <- v
      result[["num_elements"]] <- num_elements
    }
    
    return(result)
  }
  
  #FILTRO para administrador
  observeEvent(input$btn_filter, {
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    data_mysql <- Report(input$filter_start);
    
    output$casa_regular <- renderText({paste("Casa regular:",data_mysql[["casa_regular"]])})
    output$des <- renderText({paste("Deshabitadas:",data_mysql[["des"]])})
    output$lp <- renderText({paste("LP:",data_mysql[["lp"]])})
    output$lv <- renderText({paste("LV:",data_mysql[["lv"]])})
    output$c <- renderText({paste("Cerradas:",data_mysql[["c"]])})
    output$inspeccion <- renderText({paste("Inspeccionadas:",data_mysql[["inspeccion"]])})
    output$r <- renderText({paste("Renuentes:",data_mysql[["r"]])})
    output$v <- renderText({paste("V1:",data_mysql[["v"]])})
    output$num_elements <- renderText({paste("No DE REGISTROS:",data_mysql[["num_elements"]])})
    
    output$valores <- renderTable({data_mysql[["data"]]})
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
  })
  
  #Funcion que me devolveré las viviendas a las que se tiene que volver el dia en que se consulta
  HouseGoBack <- function(date, user=NULL){
    #Obteniendo los datos de una determinada fecha
    data <- load_data_mysql(databaseName = dbGlobalConfig$databaseName,
                            tableName    = dbGlobalConfig$inspectionsTable, date)
    
    #Viviendas que tienen "V" y no son "V1"
    data <- data[data$STATUS_INSPECCION=="V" & !grepl("V1",data$MOTIVO_VOLVER, fixed = TRUE),]
    
    if (!is.null(user)){
      data <- data[toupper(data$USER_NAME)== toupper(user),]
    }
    
    data$USER_NAME <- toupper(data$USER_NAME)
    
    return (data[,c("UNI_CODE", "USER_NAME")])
  }
  
  #Funcion que me devolveré las viviendas que se ingresaron en el dia
  HouseRegisterDay <- function(date, user){
    #Obteniendo los datos de una determinada fecha
    data <- load_data_mysql(databaseName = dbGlobalConfig$databaseName,
                            tableName    = dbGlobalConfig$inspectionsTable, date)
    #Filtrando por usuario
    data <- data[toupper(data$USER_NAME)== toupper(user),]
    
    data$USER_NAME <- toupper(data$USER_NAME)
    
    return (data[,c("UNI_CODE", "USER_NAME")])
  }
  
  #Accion al presionar el boton REPORTE
  observeEvent(input$reportUser,{
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_true")
    #VIVIENDAS INGRESADAS EN EL DIA
    viv_ingresadas <- HouseRegisterDay(Sys.Date(),input$username);
    #viv_ingresadas <- HouseRegisterDay("2017-08-24",input$username);
    
    if (nrow(viv_ingresadas)==0) {
      viv_ingresadas <- "Aun no se ingresaron viviendas el dia de hoy"
    }
    
    #Enviando los datos de volver a UI.R
    output$viv_ingresadas <- renderTable({ viv_ingresadas })
    
    #VIVIENDAS QUE SE TIENE QUE VOLVER
    viv_volver <- HouseGoBack(Sys.Date()-1,input$username);
    #viv_volver <- HouseGoBack("2017-08-23",input$username);
    
    if (nrow(viv_volver)==0) {
      viv_volver <- "No hay viviendas pendientes para volver el dia de hoy"
    } else {
      aux <- viv_ingresadas
      viv_volver$VIV_VOLVER <- 1
      aux$VIV_INGRESADAS <- 1
      viv_volver <- merge(viv_volver, aux, all.x=TRUE)
      viv_volver$VOLVER <- "pendiente"
      viv_volver$VOLVER[(1==viv_volver$VIV_VOLVER & 1==viv_volver$VIV_INGRESADAS)] <- "VISITADA"
      viv_volver <- viv_volver[,c("UNI_CODE", "USER_NAME", "VOLVER")]
    }
    
    #Enviando los datos de volver a UI.R
    output$viv_volver <- renderTable({ viv_volver })
    session$sendCustomMessage(type = 'action-message',
                              message = "buscando_false")
  })
  
})
