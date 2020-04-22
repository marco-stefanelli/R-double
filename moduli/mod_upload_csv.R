# APP MODULARE UPLOAD FILE CSV
# 


########################################################################################################
########################################################################################################
########################################################################################################
# MODULO


   ########################################################################################################
  # UI
  
  
  uploadModuleInput <- function(id) {
    ns <- NS(id)
    
    tagList(
      
      fluidRow(  
          wellPanel(
            h2("Import di un file in formato csv"),
            h4("Attenzione: Il campo data-ora deve essere il primo campo del dataset"),
            
            fileInput(ns("file"), "Select a csv file"),
            
            column(6,
                checkboxInput(ns("header"), "Importa nomi campi",TRUE),
                checkboxInput(ns("prima_col_serie_temporale"), 'Serie temporale sulla prima colonna', TRUE),
                checkboxInput(ns("elimina_negativi"), 'Elimina dati negativi', FALSE)          
                #checkboxInput(ns("strings"), "Trasforma stringhe in fattori"),
                #textInput(ns("na.string"), "Simbolo valore nullo", value = "NA")
            ),
            column(6,
              checkboxInput(ns("elimina_maggiore_di"), 'Elimina maggiori di', FALSE),
              numericInput(ns("valore_maggiore_di"), label = p(""), value = 1000)
            ),
            
           p(".")  
            
          )  
      ),  

      
      fluidRow(  
                column(4,  
                       wellPanel(            
                         radioButtons(ns('quote'), 'Apici',
                                      c(
                                        'Nessuno'='',
                                        'Doppio'='"',
                                        'Singolo'="'"),
                                      ''))
                       
                ),
                
                column(4,
                  wellPanel(
                    radioButtons(ns('sep'), 'Separatore',
                                 c('Virgola'=',',
                                   'Punto e virgola'=';',
                                   'Tab'='\t'),
                                 ';'  
                  ))
                  
               ),
                
               column(4,  
                wellPanel(
                  radioButtons(ns('carattere_decimale'), 'Carattere decimale',
                               c('Virgola'=',',
                                 'Punto'='.'
                                 )
                                
                  ,"."))
              )
          ),
      
        # nuova riga
        
        fluidRow( 
          
          
              
              column(4,        
                    wellPanel(            
                      radioButtons(ns('formato_data'), 'Formato Data',
                         c(
                           'Italiano DD/MM/AAAA H:M'='%d/%m/%Y %H:%M',
                           'Italiano DD-MM-AAAA H:M:S'='%d-%m-%Y %H:%M:%S',
                           'Italiano DD/MM/AAAA H:M:S'='%d/%m/%Y %H:%M:%S',
                           'Inglese  AAAA-MM-DD H:M:S'='%Y-%m-%d %H:%M:%S',
                           'Inglese  AAAA/MM/DD H:M:S'='%Y/%m/%d %H:%M:%S',
                           'Inglese  AAAA-MM-DD H:M:S'='%Y-%m-%d %H:%M:%S',
                           'Inglese  AAAA-MM-DD'='%Y-%m-%d',
                           'Definito da utente'='utente'
                         )),
                         textInput(ns('formato_utente'), label = p("Formato utente"), value = '%d/%m/%Y %H:%M')
                      )
              ),
              
              column(4,
                     wellPanel(
                       radioButtons(ns('aggregazione'), 'Aggregazione temporale',
                                    c(
                                      'Nessuna aggregazione'='aggr_no',
                                      'dati orari'='aggr_orari',
                                      'dati giornalieri'='aggr_giornalieri',
                                      'dati mensili'='aggr_mensili'
                                    ), 
                                    'aggr_no'
                       ),
                       checkboxInput(ns("i75validi"), 'Aggrega solo con 75% validi', TRUE)
                     )  
              ),
              
              column(4,  
                    wellPanel(
                      checkboxInput(ns("arrotonda"), 'Arrotonda', TRUE),
                      numericInput(ns("arrotondadecimnali"), label = p("Decimali arrotondamento"), value = 1)
                    )
              )
          
      )# fine riga    
      
   )
  }
  
  
  # FINE UI
  ########################################################################################################
  
  
  ########################################################################################################
  # SERVER MODULO
  #library(lubridate)
  #library(plyr)
  library(openair)
  
  
  uploadModule <- function(input, output, session) {
    
    userFile <- reactive({
      # If no file is selected, don't do anything
      req(input$file)
    })
    
    # The user's data, parsed into a data frame
    reactive({
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Elaborazione in corso...", value = 0)
      
      
      #dati_csv<-read.csv(file$datapath, header=input$header, sep=input$sep, quote=input$quote)
      
      dati_csv<-read.csv2(
          userFile()$datapath, 
          header=input$header, 
          sep=input$sep, 
          quote=input$quote,
          dec=input$carattere_decimale
          )
      
      formato_data_ora<-input$formato_data
      if (formato_data_ora=='utente') formato_data_ora<-input$formato_utente
      
      
      
      
      # controllo se devo eliminare i valori negativi
      if(input$elimina_negativi==TRUE){
        # sostituico negativi con NA
        dati_csv[dati_csv <0]<-NA
        
      }
      
      prima_col_data<- input$prima_col_serie_temporale
      
      
      # controllo se devo eliminare i valori maggiori di...
      if(input$elimina_maggiore_di==TRUE){
        # elimino valori maggiori di ....
        limite= input$valore_maggiore_di
        
        # applico a tutte le colonne eccetto la prima che contiene la data
        numero_colonne<-ncol(dati_csv)
        
        #salvo colonna data
        if(prima_col_data==TRUE) colonna_data<-dati_csv$Date
        
        # elimino dati maggiori del limite
        dati_csv[dati_csv >limite]<-NA
        
        #aggiungo colonna date
        if(prima_col_data==TRUE) dati_csv$Date<-colonna_data
        
      }
      
      if(prima_col_data==TRUE){
      
        # trasformo il primo campo in data-ora
        dati_csv[,1]<- as.POSIXct(dati_csv[,1],tz="Etc/GMT-1",format=formato_data_ora)
        
        #ordino per la prima colonna
        dati_csv<-dati_csv[order(as.Date(dati_csv[,1], format=formato_data_ora)),]
        
      
        # rinomimo 1 colonna da Data a Date
        names(dati_csv)[1] <- "date"
        
      
      
        if(input$aggregazione!='aggr_no'){
          
            
            tipo_aggregazione<-input$aggregazione
            #cat(paste("Aggrego ",tipo_aggregazione))
            
            if(input$i75validi==TRUE){
            
              if(tipo_aggregazione=='aggr_orari'){
                dati_csv <- timeAverage(dati_csv, avg.time = "hour", data.thresh = 75, statistic = "mean", percentile = NA, interval = "hour", vector.ws = FALSE)
                
              }else if(tipo_aggregazione=='aggr_giornalieri'){
                dati_csv <- timeAverage(dati_csv, avg.time = "day", data.thresh = 75, statistic = "mean", percentile = NA, interval = "day", vector.ws = FALSE)
                
              }else if(tipo_aggregazione=='aggr_mensili'){
                dati_csv <- timeAverage(dati_csv, avg.time = "month", data.thresh = 75, statistic = "mean", percentile = NA, interval = "month", vector.ws = FALSE)
              }
            
            }
            
          } # FINE se impostata una aggregazione temporale
        
      }# fine se la prima colonna è una data-ora  
      
      
      
      if(input$arrotonda==TRUE){
          # arrotondo al numero specificato in arrotonda_decimali, solo alle colonne numeriche
          digits<-input$arrotondadecimnali
          dati_csv[,-1] <-  round(dati_csv[,-1], digits)
          
      }  
    
      
      #write.csv2(dati_csv, file='dati_csv.csv')
      
      # ritorno il data frame
      #dati_csv<- as.data.frame(dati_csv)
      
      #if(prima_col_data==TRUE){
        #colonna_data<- as.POSIXct(dati_csv$date, tz="Etc/GMT-1", format=formato_data_ora)
        #dati_csv$date<-colonna_data
      #  dati_csv[,1]<- as.POSIXct(dati_csv[,1],tz="Etc/GMT-1",format=formato_data_ora)
      #}
      
      
      dati_csv<-as.data.frame(dati_csv)
      
      
      
      dati_csv
      
    })
  }
  
  
  # END SERVER MODULO
  ########################################################################################################
  
