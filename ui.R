# R-DOUBLE app v. 0.0


require('dplyr')
require("openair")
require('ggplot2')
require('shinythemes')
require("RPostgreSQL")
require("DT")
require("BaTFLED3D")
require("gridExtra")
library("ggpubr")
library("tidyverse")
library("data.table")


source("./moduli/mod_selezione analizzatore.R")
source("./moduli/mod_upload_csv.R")



# limite upload file a 30MB
options(shiny.maxRequestSize = 30*1024^2)


dataframe_ecomanager<-data.frame()
dataframe_csv<-data.frame()

RV   <- reactiveValues(data = data.frame())
v_time_var <-data.frame()

#RV_CSV <- reactiveValues(data = data.frame())

temp_ecomanager<-NULL
temp_csv<-NULL


# imposto variabili globali delle date di osservazione
master_data_start <- as.Date(paste("01/01/",toString(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))), format='%d/%m/%Y',  tz = 'UTC')
master_data_end   <- Sys.Date()-1


########################################################################################################
# funzione per calcolo RMSE per il confronto di due serie
# m e q sono due vettori

RMSE = function(m, o){
  sqrt(mean((m - o)^2,na.rm=TRUE))
}
########################################################################################################





########################################################################################################
########################################################################################################
########################################################################################################
# UI ###################################################################################################
accesso_postresql<-'no'

navbarPage("R-DOUBLE - 09-04-2020 - Marco Stefanelli", theme = shinytheme("cerulean"), 
                 
                 # server per i messaggi popout       
                 #tags$head(tags$script(src = "message-handler.js")),     
                 
                 ##########################################################################################
                 
                 
                 tabPanel("Carica dati da file csv",
                          
                          fluidPage( 
                            wellPanel(
                              uploadModuleInput("datafile_csv")
                            ),
                            
                            wellPanel(
                              downloadButton('download_csv', 'Download dati CSV'),
                              hr(),
                              dataTableOutput("table_csv"),
                              plotOutput("plot_timeplot"),
                              verbatimTextOutput("sommario_statistico_csv")    
                              
                            ),
                            
                            wellPanel(
                              actionButton("p_accorpa_csv", "Aggiungi a dataset di lavoro",class = "btn-warning"),
                              #actionButton("p_reset_csv", "Reset", class='btn-success')
                            )
                          )
                 )
        #         
        #   conditionalPanel(
        #     condition = "accesso_postresql=='si'",
                 
        #         tabPanel("Carica dati da Ecomanager web",
        #                  fluidPage( 
                            
        #                    wellPanel(
        #                      h2("Carica dati da Ecomanager web"),
                              
                              # invoco funzione del modulo fenerazione UI, produce l'interfaccia utente
        #                      seleziona_analizzatoreInput("id_coord_analiz")
        #                    ),
        #                    wellPanel(
        #                      actionButton("p_accorpa_ecomanager", "Aggiungi a dataset di lavoro", class = "btn-warning")
        #                    ),
                            
        #                    wellPanel(
        #                      downloadButton('download_ecomanager', 'Download dati Ecoamanager'),
        #                      hr(),
        #                      DT::dataTableOutput("tabella_df_ecomanager"),
        #                      verbatimTextOutput("sommario_statistico_ecom")    
        #                   )
        #                  )   
        #         )
        #         
        #   )   
                 
                 
                 ,tabPanel("Dataset di lavoro",
                           
                           
                           fluidPage( 
                             
                             column(12, wellPanel(
                               
                               h3("Dataset di lavoro"),
                               actionButton("p_reset_df_lavoro", "Reset dataset", class='btn-success'),
                               downloadButton('download_lavoro', 'Download dati lavoro'),
                               
                               #dateRangeInput('range_lavoro',
                               #                label = 'Ritaglia Range di osservazione',
                               #               format = "dd-mm-yyyy"
                               #              #start = Sys.Date() - 2, end = Sys.Date() + 2
                               #),  
                               
                               hr(),
                               #verbatimTextOutput("struttura_df_lavoro"),  
                               DT::dataTableOutput("tabella_df_lavoro")
                               
                               
                               
                               
                             ))   
                           )  
                           
                           
                 )
                 ##########################################################################################
                 
                 
                 ,tabPanel("Analisi singola serie",
                           
                           # Give the page a title
                           titlePanel("Analisi di una serie del dataset"),
                           
                           # Generate a row with a sidebar
                           sidebarLayout(      
                             
                             # Define the sidebar with one input
                             sidebarPanel(
                               # select di scelta della serie, viene generata dinamicamente 
                               uiOutput("ui_select_serie")
                               
                             ),
                             
                             # Create a spot for the barplot
                             mainPanel(
                               
                               wellPanel(
                                 h4("Sommario statistico"),
                                 verbatimTextOutput("summary")  
                               ),
                               
                               wellPanel(
                                 h3('Valori anomali'),
                                 p('Valore anomalo: valore  con  scostamento  (positivo) dal terzo quartile superiore a 3 volte il range interquartile.'),
                                 verbatimTextOutput("parametri_dati_valori_anomali"),
                                 DT::dataTableOutput("dati_valori_anomali")
                               ),
                               
                               
                               wellPanel(
                                 plotOutput("plot_istog_dist_freq"),
                                 plotOutput("plot_boxplot")
                               ),
                               
                               wellPanel(
                                 h3('Variazione nel tempo'),
                                 plotOutput("dati_stazione_plot_timevariation"),
                                 DT::dataTableOutput("dati_plot_timevariation")
                               ),
                               
                               wellPanel(
                                 hr(),
                                 h3('Dataset Settimana media'),
                                 downloadButton('dwn_v_timev_settimana', 'Download settimana media'),
                                 tags$br(),
                                 DT::dataTableOutput("v_timev_settimana"),
                                 hr(),
                                 
                                 
                                 hr(),
                                 h3('Dataset ora media'),
                                 downloadButton('dwn_v_timev_ora', 'Download ora media'),
                                 tags$br(),
                                 DT::dataTableOutput("v_timev_ora"),
                                 hr(),
                                 
                                 
                                 hr(),
                                 h3('Dataset mese media'),
                                 downloadButton('dwn_v_timev_mese', 'Download mese media'),
                                 tags$br(),
                                 DT::dataTableOutput("v_timev_mese"),
                                 hr(),
                                 
                                 hr(),
                                 h3('Dataset giorno settimanale media'),
                                 downloadButton('dwn_v_timev_giorno', 'Download giorno media'),
                                 tags$br(),
                                 DT::dataTableOutput("v_timev_giorno"),
                                 hr()
                                 
                                 
                               )
                               
                             )
                             
                           )
                           
                           
                 )
                 ##########################################################################################
                 
                 
                 ,tabPanel("Analisi correlazione",
                           
                           # titolo verra' aggiunto dinamicamente
                           uiOutput("titolo_analisi_correlazione"),
                           
                           downloadButton('export'),
                           
                           #titlePanel("Analisi correlazione tra due serie"),
                           
                           
                           fluidRow(
                             column(6, wellPanel(
                               
                               h4("Scegli la prima serie da confrontare"),
                               uiOutput("corr_select_serie_1")
                             )
                             ),
                             
                             column(6, wellPanel(
                               h4("Scegli la seconda serie da confrontare"),
                               uiOutput("corr_select_serie_2")
                               
                             )
                             
                             )   
                           ), # fine riga scelta serie
                           
                           
                           fluidRow(
                             
                             column(12, wellPanel(
                               h4("Elaborazioni"),
                               
                               
                               hr(),
                               plotOutput("ac_plot_scatter"),
                               plotOutput("ac_plot_boxplot"),
                               plotOutput("plot_serie_sovrapposte"),
                                 
                               
                               
                               
                               hr(),
                               h4("Indici di correlazione"),
                               verbatimTextOutput("indici_correlazione"), 
                               
                               #hr(),
                               #h4("Parametri retta di regressione"),
                               #verbatimTextOutput("summary_regressione"), 
                               
                               hr(),
                               h4("Sommario statistico"),
                               verbatimTextOutput("summary_correlazione")
                             )  
                             )  
                             
                           ) # fine riga elaborazioni  
                           
                 ),# fine tab panel correlazioni  
                 
                 tabPanel(
                   dateRangeInput("date_oss", "Intervallo di Osservazione:",
                                  start = as.Date(paste("01/01/",toString(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))), format='%d/%m/%Y',  tz = 'UTC'),
                                  end   = Sys.Date()-1,
                                  
                                  #start = as.Date("26/03/2017", format='%d/%m/%Y'),
                                  #end  = as.Date("26/03/2017", format='%d/%m/%Y'),
                                  
                                  #start = as.POSIXct("26/03/2017", format='%d/%m/%Y'),
                                  #end  = as.POSIXct("26/03/2017", format='%d/%m/%Y'),
                                  
                                  format = "dd/mm/yyyy"
                   )
                 ) 
                 
                 
                 
                 
                 ##########################################################################################   
                 
                 
                 # questo js server per inviare messaggi tipo alert
                 ,tags$head(tags$script(src = "message-handler.js")) 
                 
) # end navbarpage        



