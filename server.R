
########################################################################################################
########################################################################################################
########################################################################################################
# SERVER ###############################################################################################




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



server <- function(input, output, session) {
  
  
  
  
  
  # click su p_reset ----------------------------------------------------------------
  observeEvent(input$p_reset_df_lavoro, {
    # reset
    RV$data <-data.frame()
    
  })   
  
  
  # click su p_accorpa_ecomanager ----------------------------------------------------------------
  observeEvent(input$p_accorpa_ecomanager, {
    cat('p_accorpa_ecomanager')
    dataframe_ecomanager<-loadData_ecomanager()
    accorpa_dataframe(dataframe_ecomanager)
    
  })   
  
  
  # click su p_reset_ecomanager ----------------------------------------------------------------
  observeEvent(input$p_reset_ecomanager, {
    cat('p_reset_ecomanager')
    dataframe_ecomanager <- saveData_ecomanager(data.frame())
    
  })
  
  
  # click su p_accorpa_csv ----------------------------------------------------------------
  observeEvent(input$p_accorpa_csv, {
    
    dataframe_csv<-aggiorna_df_csv()
    
    #write.csv(dataframe_csv,'dataframe_csv.csv')
    
    accorpa_dataframe(dataframe_csv)
    
    # Aggiorno le variabili globali delle date di osservazione
    # in modo da preimpostare l'eventuale estrazione dati da Postgres
    
    data_ora_start  <-min(dataframe_csv[,1])
    data_ora_stop   <-max(dataframe_csv[,1])
   
    
    #data_ora_start  <-min(as.Date(dataframe_csv$date, format = "%Y-%m-%d %H:%M"))
    #data_ora_stop   <-max(as.Date(dataframe_csv$date, format = "%Y-%m-%d %H:%M"))
    
    
    updateDateRangeInput(session, "date_oss",
                         
                         start = data_ora_start,
                         end = data_ora_stop
                         
                         
    )
    
    
    
    
  })   
  
  
  
  aggiorna_df_ecomanager<-reactive({
    
    cat('aggiorna_df_ecomanager')
    
    master_data_start<-input$date_oss[1]
    master_data_end<-input$date_oss[2]
    
    # ricevo il dataset da selezione analizzatOre della rete chiamando il relativo modulo
    df_selezione_analizzatore<-callModule(seleziona_analizzatore,"id_coord_analiz",master_data_start,master_data_end)
    #df_selezione_analizzatore<-df_lavoro
    
    str_test_data1=df_selezione_analizzatore$data_ora[1]
    str_test_data2=df_selezione_analizzatore$data_ora[2]
    
    # traformo in date..
    test_data1= as.POSIXct(str_test_data1,format="%Y-%m-%d",  tz = 'UTC')
    test_data2= as.POSIXct(str_test_data2,format="%Y-%m-%d",  tz = 'UTC')
    
    time_diff<-difftime( test_data2, test_data1, units="hours")
    
    #time_diff<-df_selezione_analizzatore$data_ora[2]-df_selezione_analizzatore$data_ora[1]
    
    cat(paste('time_diff=', as.numeric(time_diff)))
    
    if(as.numeric(time_diff)==1){
      # DF ORARIO
      tipo_dataframe='orario'
    }else{
      # DF GIORNALIERO
      tipo_dataframe='giornaliero'
    }
    
    # salvo per test
    df_selezione_analizzatore <<- df_selezione_analizzatore
    
    
    df_ecomanager<-loadData_ecomanager()
    if(is.null(df_ecomanager)){
      df_ecomanager<-data.frame()
    }
    
    # controllo se il dataframe ottenuto ha le stesse righe del df_ecomanager
    if(nrow(df_selezione_analizzatore)!=nrow(df_ecomanager)){
      
      
      # ricreo il df_ecomanager come una sequenza di dati orari/giornalieri prendendo come data start e stop gli estremi del df_selezione_analizzatore
      numero_righe<-nrow(df_selezione_analizzatore)
      r_data1<-df_selezione_analizzatore$data_ora[1]
      r_data2<-df_selezione_analizzatore$data_ora[numero_righe]
      
      cat(paste('tipo_dataframe=',tipo_dataframe, ' r_data1=', r_data1,' r_data2=', r_data2))
      
      if(tipo_dataframe=='orario'){
        #creo sequenza oraria
        sequenza <- seq(r_data1, r_data2, by="hour",  tz = 'UTC')
        
        #creo data frame con campo data_ora
        df_ecomanager <- data.frame(list(data_ora=sequenza))
        
      }else{
        
        #creo sequenza giornaliera
        sequenza <- seq(r_data1, r_data2, by="day",  tz = 'UTC')
        
        #creo data frame con campo data_ora
        df_ecomanager <- data.frame(list(data_ora=sequenza))
        
      }
      
      
    }  
    
    # ok procedo ad unione, presumo che il primo campo di ambedue i df sia 'data_ora'
    df_ecomanager<-merge(df_ecomanager, df_selezione_analizzatore, by='data_ora', all=TRUE)  
    
    
    df_ecomanager
    
  })
  
  
  
  
  
  ###################################################################################  
  
  output$tabella_df_ecomanager <-   DT::renderDataTable({
    saveData_ecomanager(aggiorna_df_ecomanager())
  })
  
  ###################################################################################
  
  
  
  
  
  
  ###################################################################################
  ###################################################################################
  ###################################################################################
  # funzioni modulo load file csv
  
  
  aggiorna_df_csv <- callModule(uploadModule, "datafile_csv")
  
  
  output$table_csv <- renderDataTable({
    
    
    #datatable(
    #  saveData_csv(aggiorna_df_csv()),rownames=TRUE, 
    #  filter="top",  
    #  options = list(pageLength = 24), 
     # class = 'cell-border stripe') %>%   
    #  formatDate(1, method = 'toLocaleString' 
     # )
    
    datatable(
      saveData_csv(aggiorna_df_csv()),
      rownames=TRUE, 
      filter="top",  
      options = list(pageLength = 5), 
      class = 'cell-border stripe') %>%   
      formatDate(1, method = 'toLocaleString' 
      )
    
    
    
    #saveData_csv(aggiorna_df_csv())
    
    
  })
  
  
  
  #########################################################################################
  # sommario statistico del data_frame di lavoro  
  output$sommario_statistico_csv <- renderPrint({
    dati<- aggiorna_df_csv()
    summary(dati)
    
  })
  ########
  
  
  
  
  
  #######################################################################################
  # genero grafico timeplot di tutte le serie
  
  output$plot_timeplot <- renderPlot({
    
    dati_csv<- aggiorna_df_csv()
    
    if( is.null(dati_csv) ) return()
    
    titolo_grafico<-'Time plot di tutte le serie'
    
    #lista edelle colonne
    pollutant=names(dati_csv)
    
    #estraggo tuttti meno la prima
    pollutant<-pollutant[-1]
    
    #rinomino prima colonna..
    colnames(dati_csv)[1]<-'date'
    
    timePlot(dati_csv,pollutant)
    
    grid()
    
    
  })
  #######################################################################################
  
  
  
  
  
  
  
  
  
  
  
  # funzioni per salvataggio / ripristino del data frame di lavoro
  
  ###################################################################################
  ###################################################################################
  saveData_ecomanager <- function(data) {
    #data <- as.data.frame(t(data))
    cat("saveData ecomanager \t")
    temp_ecomanager <<- data
    
  }
  
  ###################################################################################
  
  loadData_ecomanager <-function(){
    cat("loadData ecomanager \t")
    
    if (exists("temp_ecomanager") ) {
      temp_ecomanager
    }else{
      data.frame()
    }
    
  }
  ###################################################################################
  ###################################################################################
  
  
  
  ###################################################################################
  ###################################################################################
  saveData_csv <- function(data) {
    #data <- as.data.frame(t(data))
    cat("saveData csv \t")
    temp_csv <<- data
    
  }
  
  ###################################################################################
  
  loadData_csv <-function(){
    cat("loadData csv \t")
    
    if (exists("temp_csv")) {
      temp_csv
    }else{
      data.frame()
    }
    
  }
  ###################################################################################
  ###################################################################################
  
  
  
  
  
  ###################################################################################
  ###################################################################################
  
  accorpa_dataframe <-function(cur_dataframe){
    cat("\n accorpa_dataframe")
    # unisce il dataframe passato con il dataframe di lavoro RV$data
    
    if(nrow(cur_dataframe) >0){
      
      # rinomino la prima colonna di cur_dataframe in 'data_ora'..
      colnames(cur_dataframe)[1] <- "data_ora"
      
      n_righe<-nrow(RV$data)
      
      # se il df di lavoro ha zero righe, lo sovrascrivo con quello passato...
      if( n_righe==0){
        RV$data <- cur_dataframe
      }else{
        
        # merge...
        # unisco i due df
        
        df_out <- merge(RV$data, cur_dataframe, by='data_ora', all=TRUE)
        
        # solo per test scrivo il file csv
        #write.csv2(df_out,file='df_lavoro.csv')
        
        RV$data<-df_out
        
        
        
        
      }    
    }  
    
    
  }
  
  ###################################################################################
  ###################################################################################
  
  
  
  
  test_tipo_dataframe<-function(cur_dataframe){
    
    ## esegue test orario - giornaliero su dataset
    
    str_test_data1=cur_dataframe$data_ora[1]
    str_test_data2=cur_dataframe$data_ora[2]
    
    # traformo in date..
    test_data1= as.POSIXct(str_test_data1,format="%Y-%m-%d",  tz = 'UTC')
    test_data2= as.POSIXct(str_test_data2,format="%Y-%m-%d",  tz = 'UTC')
    
    time_diff<-difftime( test_data2, test_data1, units="hours")
    
    #time_diff<-df_selezione_analizzatore$data_ora[2]-df_selezione_analizzatore$data_ora[1]
    
    #cat(paste('time_diff=', as.numeric(time_diff)))
    
    if(as.numeric(time_diff)==1){
      # DF ORARIO
      tipo_dataframe='orario'
    }else{
      # DF GIORNALIERO
      tipo_dataframe='giornaliero'
    }
    
    tipo_dataframe
  }
  
  
  ###################################################################################
  ###################################################################################
  
  
  
  
  
  #######################################################################################
  # download dati csv
  #data <- mtcars
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("export_csv",  ".csv", sep="")
    },
    content = function(file) {
      write.csv(loadData_csv(), file)
    }
  )
  
  #######################################################################################
  
  #######################################################################################
  # download dati ecomanager
  #
  
  
  output$download_ecomanager <- downloadHandler(
    filename = function() {
      paste("export_ecomanager",  ".csv", sep="")
    },
    content = function(file) {
      
      
      write.csv( loadData_ecomanager(), file)
    }
  )
  
  #######################################################################################
  
  
  
  
  ###################################################################################  
  
  output$tabella_df_lavoro <-   DT::renderDataTable({
    
    req(RV$data)
    
    if(nrow(RV$data)>0){
      datatable(
        RV$data,rownames=TRUE, 
        filter="top",  
        options = list(pageLength = 24), 
        class = 'cell-border stripe') %>%   
        formatDate(1, method = 'toLocaleString' 
        )
    }
    
  })
  
  ###################################################################################
  
  
  
  
  
  ###################################################################################
  
  output$struttura_df_lavoro <- renderPrint({
    # stampa struttura
    str(RV$data)
  })  
  ###################################################################################
  
  
  
  ###################################################################################
  # aggiorno dinamicamente il controllo range_lavoro in funzione del range date
  # del df di lavoro
  
  observe({
    
    
    df_lavoro <- RV$data
    
    # salvo in locale
    df_lavoro <<- df_lavoro
    
    if(nrow(df_lavoro)>0 && inherits(df_lavoro[[1]], "Date")){
      
      mystartdate <- as.character(format(as.Date(min(df_lavoro$data_ora))),"dd-mm-yyyy")
      myenddate   <- as.character(format(as.Date(max(df_lavoro$data_ora))),"dd-mm-yyyy")
      
      updateDateRangeInput(session, "range_lavoro",
                           start = mystartdate,
                           end = myenddate
      )
    }  
  })
  ###################################################################################
  
  
  
  
  
  
  #######################################################################################
  # download dati df_lavoro
  #
  
  
  output$download_lavoro <- downloadHandler(
    filename = function() {
      paste("export_dataset_lavoro",  ".csv", sep="")
    },
    content = function(file) {
      
      write.table(RV$data, file, row.names=FALSE, na='', sep=";" ,quote=FALSE, dec='.' )
    }
  )
  
  #######################################################################################
  
  
  
  
  
  
  
  
  #########################################################################################
  # list dinamica scelta analizzatore
  output$ui_select_serie<- renderUI({
    
    df_lavoro <- RV$data
    
    test_df<-is.data.frame(df_lavoro)
    
    # elimono NA
    #df_lavoro[is.na(df_lavoro)] <- NULL
    
    if(nrow(df_lavoro)>0){
      
      
      # creo dataset con solo quelle colonne..
      data_num <- select_if(df_lavoro, is.numeric)
      
      lista_colonne <- names(data_num)
      
      selectInput("ui_select_serie", "Scelta serie", choices = lista_colonne)
    }  
    
    
  })
  #######################################################################################
  
  
  
  
  #######################################################################################
  # funzione che genera sommario statistico
  
  output$summary <- renderPrint({
    
    serie<-input$ui_select_serie 
    valori <-RV$data[serie]
    
    summary( valori)
  })
  #######################################################################################
  
  
  
  
  
  #######################################################################################
  # funzione che genera la stampa dei parametri per la determinazione di valori anomali
  
  output$parametri_dati_valori_anomali <- renderPrint({
    
    req(input$ui_select_serie )
    
    serie<-input$ui_select_serie 
    valori <-RV$data[serie]
    
    perc75              <-quantile(valori,.75,na.rm=TRUE)
    perc25              <-quantile(valori,.25,na.rm=TRUE)
    interquartile       <-perc75 - perc25
    #deviazione_standard <-sd(valori)
    
    valore_soglia <- interquartile * 3
    
    valori_anomali<- subset(RV$data, RV$data[serie] >valore_soglia)
    
    dati_valori_anomali <- paste('Interquartile= (75 - 25mo percentile) =',interquartile, "<br/>", 'valore_soglia=',valore_soglia)
    dati_valori_anomali
    
  })
  #######################################################################################
  
  
  #######################################################################################
  # funzione che genera la stampa dei dati di valori anomali
  
  output$dati_valori_anomali <- DT::renderDataTable({
    
    req(input$ui_select_serie )
    
    serie<-input$ui_select_serie 
    valori <-RV$data[serie]
    
    perc75<-quantile(valori,.75,na.rm=TRUE)
    perc25<-quantile(valori,.25,na.rm=TRUE)
    interquartile=perc75 - perc25
    valore_soglia <- interquartile * 3
    
    valori_anomali<- subset(RV$data, RV$data[serie] >valore_soglia)
    
    valori_anomali
    
  })
  #######################################################################################
  
  
  
  
  
  #######################################################################################
  # plot distribuzione frequenze
  output$plot_istog_dist_freq<-renderPlot({
    req(input$ui_select_serie)
    
    serie<-input$ui_select_serie 
    
    valori <-RV$data[serie]
    serie_x<- valori[,1]
    serie_x<-na.omit(serie_x)
    
    titolo_grafico<-paste(serie, " - Istogramma della distribuzione delle frequenze di valori")
    
    hist(serie_x,
         xlab=serie, 
         border="blue", 
         col="green",
         las=1,
         breaks=10, 
         main=titolo_grafico
    )     
    
    lines(density(serie_x), # density plot
          lwd = 2, # thickness of line
          col = "chocolate3")
    
    # media
    abline(v = mean(serie_x),
           col = "royalblue",
           lwd = 2)
    # mediana
    abline(v = median(serie_x),
           col = "red",
           lwd = 2)
    
    # legenda
    legend(x = "topright", # location of legend within plot area
           c("Density plot", "Media", "Mediana"),
           col = c("chocolate3", "royalblue", "red"),
           lwd = c(2, 2, 2))
    
    
    
  })
  #######################################################################################
  
  
  
  
  
  # plot BOXPLOT
  
  output$plot_boxplot<-renderPlot({
    
    req(input$ui_select_serie)
    
    serie<-input$ui_select_serie 
    
    valori <-RV$data[serie]
    serie_x<- valori[,1]
    
    titolo_grafico<-paste(serie, " - Box plot")
    
    boxplot(valori,
            main = titolo_grafico,
            xlab = "",
            ylab = serie,
            col = "green",
            border = "royalblue",
            horizontal = TRUE,
            notch = TRUE
    )
    
    
  })
  #######################################################################################
  
  
  
  
  #######################################################################################
  # genero grafico time variation
  
  output$dati_stazione_plot_timevariation <- renderPlot({
    
    req(input$ui_select_serie )
    dati_time_var()
    
    
  })
  #######################################################################################
  
  
  
  #######################################################################################
  #######################################################################################
  #######################################################################################
  ## tabelle originate dal pplot timeVariation
  
  #DT::dataTableOutput("v_timev_settimana"),
  #DT::dataTableOutput("v_timev_ora"),
  #DT::dataTableOutput("v_timev_mese"),
  #DT::dataTableOutput("v_timev_giornosett")
  
  ###################################################################################
  #
  dati_time_var <- reactive({
    # rende dinamicamente i dataset calcolati dalla funziopne timevarition
    req(input$ui_select_serie )
    v_time_var<-NULL
    
    df_lavoro<-RV$data
    
    serie <- input$ui_select_serie 
    data  <- df_lavoro[,1]
    
    # solo se prima colonna Data_ora
    
    if(names(df_lavoro)[1]=='data_ora'){
      
      matrice_dati_stazione <-df_lavoro[,c('data_ora',serie)]
      
      if(nrow(matrice_dati_stazione)>0){
        
        #rinomino prima colonna..
        colnames(matrice_dati_stazione)[1]<-'date'
        
        v_time_var<-timeVariation(matrice_dati_stazione,serie, normalise = FALSE)
        #timeVariation(matrice_dati_stazione,a_inquinanti)
        
      }
      
    }
    
    v_time_var
  })
  
  ###################################################################################
  # settimana media
  
  output$v_timev_settimana <-   DT::renderDataTable({
    dataset<-dati_time_var()
    dataset$data$day.hour
  })
  
  
  # download dataset
  output$dwn_v_timev_settimana <- downloadHandler(
    filename = function() {
      paste("dataset_settimana_media",  ".csv", sep="")
    },
    content = function(file) {
      write.csv(dati_time_var()$data$day.hour, file)
    }
  )
  #######################################################################################
  
  
  
  ###################################################################################
  # ora media
  
  output$v_timev_ora <-   DT::renderDataTable({
    dataset<-dati_time_var()
    dataset$data$hour
  })
  
  
  # download dataset
  output$dwn_v_timev_ora <- downloadHandler(
    filename = function() {
      paste("dataset_ora_media",  ".csv", sep="")
    },
    content = function(file) {
      write.csv(dati_time_var()$data$hour, file)
    }
  )
  #######################################################################################
  
  
  
  
  ###################################################################################
  # mese media
  
  output$v_timev_mese <-   DT::renderDataTable({
    dataset<-dati_time_var()
    dataset$data$month
  })
  
  
  # download dataset
  output$dwn_v_timev_mese <- downloadHandler(
    filename = function() {
      paste("dataset_mese_media",  ".csv", sep="")
    },
    content = function(file) {
      write.csv(dati_time_var()$data$month, file)
    }
  )
  #######################################################################################
  
  
  
  
  ###################################################################################
  # giorno media
  
  output$v_timev_giorno <-   DT::renderDataTable({
    dataset<-dati_time_var()
    dataset$data$day
  })
  
  
  # download dataset
  output$dwn_v_timev_giorno <- downloadHandler(
    filename = function() {
      paste("dataset_giorno_media",  ".csv", sep="")
    },
    content = function(file) {
      write.csv(dati_time_var()$data$day, file)
    }
  )
  #######################################################################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # da qui funzioni relative all'analidi di correlazione tra due serie
  #########################################################################################
  #########################################################################################
  #########################################################################################
  #########################################################################################
  #########################################################################################
  #########################################################################################
  
  
  ## vals will contain all plot and table grobs
  vals <- reactiveValues(
    ac_plot_scatter=NULL,
    ac_plot_boxplot=NULL,
    plot_serie_sovrapposte=NULL,
    indici_correlazione=NULL,
    summary_correlazione=NULL,
    titolo_analisi_correlazione=NULL
    )
  
  
  
  #########################################################################################
  # titolo della pagina dinamico
  output$titolo_analisi_correlazione<- renderUI({
    
    serier1_selezionata<-input$corr_1_ui_select_serie
    serier2_selezionata<-input$corr_2_ui_select_serie
    
    #mystartdate <- as.character(format(as.Date(min(df_lavoro$data_ora))),"%d-%m-%y")
    #myenddate   <- as.character(format(as.Date(max(df_lavoro$data_ora))),"%d-%m-%y")
    
    # date min e max in formato italiano
    mystartdate <-format(as.Date(min(df_lavoro$data_ora)), "%d/%m/%Y")
    myenddate   <-format(as.Date(max(df_lavoro$data_ora)), "%d/%m/%Y")
    
                         
    
    titolo<-paste("Analisi di correlazione tra ",serier1_selezionata, " e", 
                  serier2_selezionata, "\n Periodo di osservazione dal ", 
                  mystartdate, " al ",  myenddate )
    
    vals$titolo_analisi_correlazione<-text_grob(titolo, just = "centre", color = "blue", size='14', face='bold' )
    titlePanel(titolo)
    
  })
  #######################################################################################
  
  
  
  
  
  
  
  
  
  #########################################################################################
  # list dinamica scelta analizzatore - serie 1
  output$corr_select_serie_1<- renderUI({
    
    df_lavoro <- RV$data
    
    test_df<-is.data.frame(df_lavoro)
    
    # elimono NA
    #df_lavoro[is.na(df_lavoro)] <- NULL
    
    if(nrow(df_lavoro)>0){
      
      
      # creo dataset con solo quelle colonne..
      data_num <- select_if(df_lavoro, is.numeric)
      
      lista_colonne <- names(data_num)
      
      selectInput("corr_1_ui_select_serie", "Scelta serie", choices = lista_colonne)
    }  
    
    
  })
  #######################################################################################
  
  
  
  #########################################################################################
  # list dinamica scelta analizzatore - serie 1
  output$corr_select_serie_2<- renderUI({
    
    df_lavoro <- RV$data
    
    test_df<-is.data.frame(df_lavoro)
    
    # elimono NA
    #df_lavoro[is.na(df_lavoro)] <- NULL
    
    if(nrow(df_lavoro)>0){
      
      
      # creo dataset con solo quelle colonne..
      data_num <- select_if(df_lavoro, is.numeric)
      
      lista_colonne <- names(data_num)
      
      selectInput("corr_2_ui_select_serie", "Scelta serie", choices = lista_colonne,  selected=lista_colonne[2])
    }  
    
    
  })
  #######################################################################################
  
  
  
  
  
  #######################################################################################
  output$summary_correlazione<- renderPrint({
    # anche per testare il dataset delle due serie scelte...
    dati<-dataset_correlazione()
    
    # tabella del sommario
    obj_sommario<-do.call(cbind, lapply(dati, summary))
    
    # trasforno in data frame
    df_sommario<-as.data.frame(obj_sommario)
    
    # per eliminare la prima colonna anomale delle stringhe indicatori statistici
    #df_sommario<-cbind(df_sommario[,1],df_sommario[,2])
    rownames(df_sommario) <- NULL
    
    # aggiungo colonna delle differenze percentuali
    df_sommario$delta_perc<-round((df_sommario[,2]/df_sommario[,1]*100),1)-100
    
    # arrotondo 1 digit
    df_sommario<-round(df_sommario,1)
    
    # aggiungo colonna indicatore statistico
    if(nrow(df_sommario)==7){
      df_sommario$indicatore_statistico<-c("Minimo","1 quart.","Mediana","Media","3 quart.","Massimo","NA")
    }else{
      df_sommario$indicatore_statistico<-c("Minimo","1 quart.","Mediana","Media","3 quart.","Massimo")  
    }  
    
    # creo un nuovo df riordinando il df_sommario, prima colonna indicatore statistico
    ord_df_sommario<-cbind(df_sommario[,4],df_sommario[,1],df_sommario[,2],df_sommario[,3])
    
    # ordino correttamente i nomi delle colonne
    nomi_ord_df_sommario<-c(names(df_sommario[4]),names(df_sommario[1]),names(df_sommario[2]),names(df_sommario[3]))
    
    # assegno i nomi ordinati al df ord_df_sommario
    colnames(ord_df_sommario) <- nomi_ord_df_sommario
    
    rownames(ord_df_sommario) <- NULL  
    
    # ri - trasformo da dataframe a tabella
    summary_correlazione<-as.data.table(ord_df_sommario)
    
    
    
    # creo ik grob per export in pdf
    
    mytheme <- gridExtra::ttheme_default(
      core = list(fg_params=list(cex = 0.6)),
      colhead = list(fg_params=list(cex = 0.8)),
      rowhead = list(fg_params=list(cex = 0.8)))
    
    vals$summary_correlazione<- tableGrob(print(summary_correlazione, row.names = FALSE), theme = mytheme)
    
    #print(summary_correlazione, row.names = FALSE)
    
  })
  #######################################################################################
  
  
  
  
  
  
  
  #######################################################################################
  dataset_correlazione <- reactive({
    req(input$corr_1_ui_select_serie,input$corr_1_ui_select_serie)
    
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Elaborazione in corso...", value = 0)
    
    df_lavoro <- RV$data
    serier1_selezionata<-input$corr_1_ui_select_serie
    serier2_selezionata<-input$corr_2_ui_select_serie
    
    #unisco i dataset...
    df_out <- df_lavoro[,c(serier1_selezionata,serier2_selezionata)]
    
    progress$inc(1, detail = "Dataset pronto!")
    
    df_out
    
  })
  #######################################################################################    
  
  
  
  
  
  
  
  
  #######################################################################################
  # grafico scatter ANALISI CORRELAZIONE 
  output$ac_plot_scatter <- renderPlot({
    
    req(input$corr_1_ui_select_serie,input$corr_2_ui_select_serie)
    
    serie_1<-input$corr_1_ui_select_serie
    serie_2<-input$corr_2_ui_select_serie
    
    titolo_grafico=paste('Correlazione tra ',serie_1," e ",serie_2)
    
    dati<-dataset_correlazione()
    
    if(nrow(dati)>0){
      
      dati_x<-dati[,1]
      dati_y<-dati[,2]
      
      label_x<-names(dati[1])
      label_y<-names(dati[2])
      
      x_pos=round(max(dati_x,na.rm=TRUE)*0.1)
      y_pos=round(max(dati_y,na.rm=TRUE)*0.9)
      delta_y_pos=round(max(dati_y,na.rm=TRUE)*0.1)
      
      vals$ac_plot_scatter<- ggplot(dati, aes(x=dati_x, y=dati_y)) +
        stat_cor(label.x = x_pos, label.y = y_pos) +
        stat_regline_equation(label.x = x_pos, label.y = y_pos - delta_y_pos)+
        theme_light()+
        geom_point(shape=1) +    # Use hollow circles
        geom_smooth(method=lm) +      # Add linear regression line 
        ggtitle(titolo_grafico)+
        xlab(label_x)+ 
        ylab(label_y)+
        # forzo origine assi a zero
        xlim(0, max(dati_x))+
        ylim(0,max(dati_y))+
        
        theme(
          plot.title = element_text(color="blue", size=12, face="bold")
        )
      
      
      
      
    }else{
      vals$ac_plot_scatter<-plot(1, type="n", axes=F, xlab="", ylab="",  main="Dati non presenti nel DB Postgres")
    } 
    
    vals$ac_plot_scatter
    
  })
  #######################################################################################
  
  
  
  
  
  #######################################################################################
  # grafico comparazione con bxplot dei dati statistici
  output$ac_plot_boxplot <- renderPlot({
    
    req(input$corr_1_ui_select_serie,input$corr_2_ui_select_serie)
    
    serie_1<-input$corr_1_ui_select_serie
    serie_2<-input$corr_2_ui_select_serie
    
    titolo_grafico<-paste("Boxplot comparazione dati statistici di posizione tra ",serie_1, " e ",serie_2)
    
    dati<-dataset_correlazione()
    
      dati_x<-dati[,1]
      dati_y<-dati[,2]
      
      label_x<-names(dati[1])
      label_y<-names(dati[2])
      
      # effettuo rotazione della tabella in modo da avere una struttura:
      # inquinante, valore
      df_boxplot<-gather(dati, key='inquinante', value='valore')
      
      
      
      
      
     # con ggplot...
      vals$ac_plot_boxplot<- ggplot(df_boxplot, aes(x=as.factor(inquinante), y=valore)) + 
        geom_boxplot(fill="slateblue", alpha=0.2) +
        theme_light() +
        ggtitle(titolo_grafico)+
        labs(x="Serie", y = "Valore")+
        scale_fill_brewer(palette="Blues")+
        theme(
          plot.title = element_text(color="blue", size=12, face="bold")
        )
       
    
    vals$ac_plot_boxplot
    
  })
  ## END grafico comparazione con bxplot dei dati statistici
  #######################################################################################
  
  
  
  
  #######################################################################################
  # grafico con le due serie sovrapposte
  
  output$plot_serie_sovrapposte <- renderPlot({
    
    req(input$corr_1_ui_select_serie,input$corr_2_ui_select_serie)
    
    serie_1<-input$corr_1_ui_select_serie
    serie_2<-input$corr_2_ui_select_serie
    
    
    titolo_grafico<-paste("Grafico   serie ",serie_1, " vs ",serie_2)
    
    df_lavoro <- RV$data
    
    #estraggo solo le tre colonne che mis ervono...
    test_data <- df_lavoro[,c('data_ora',serie_1,serie_2)]
    
    
    vals$plot_serie_sovrapposte<-ggplot(test_data, aes(test_data[,1])) + 
      
      geom_line(aes(y = test_data[,2], colour = names(test_data[2]))) + 
      geom_line(aes(y = test_data[,3], colour = names(test_data[3]))) +
      theme_light() +
      theme(legend.title=element_blank()) +
      ylab("microg/m3")+ 
      xlab("Data")+
      ggtitle(titolo_grafico)+
      theme(
        plot.title = element_text(color="blue", size=12, face="bold")
      )
    
    
    
    vals$plot_serie_sovrapposte
    
  })
  #######################################################################################
  
  
  
  
  #######################################################################################
  output$indici_correlazione<- renderText({
    # calcola gli indici Pearson Kendall e RMSE come indicatore di precisione
    
    req(dataset_correlazione())
    
    dati<-dataset_correlazione()
    dati_x<-dati[,1]
    dati_y<-dati[,2]
    
    reg<-lm(dati_y ~ dati_x,data=dati)
    q<-round(reg$coefficients[1],digits =3)
    m<-round(reg$coefficients[2],digits =3)
    
    val_rmse<-round(RMSE(dati_x,dati_y),digits =3)
    val_nrmse<-round(nrmse(dati_x,dati_y),digits =3)
    
    pearson<-round(cor(dati_x,dati_y, method='pearson', use="complete.obs"),digits =3)
    kendall<-round(cor(dati_x,dati_y, method='kendall', use="complete.obs"),digits =3)
    
    valori_indici<-paste("Retta di correlazione: m=", m , " q=", q ,  " - Pearson=",pearson, " Kendall=",kendall, "\n"
                         ,"Radice Errore quadratico medio RMSE=",val_rmse, " NRMSE=",val_nrmse
                         )
    
    #paste("Retta di correlazione: m=", m , " q=", q , " - RMSE=",val_rmse, " - Pearson=",pearson, " Kendall=",kendall)
    
    nota_rmse<-paste("
    L'errore quadratico medio (in inglese Mean Squared Error, MSE) indica la discrepanza quadratica media fra i valori dei dati osservati ed i valori dei dati stimati.
La sua radice quadrata fornisce un ulteriore indice statistico, la cosiddetta radice dell'errore quadratico medio 
(in inglese root-mean-square error, RMSE oppure Root Mean Square Deviation, RMSD). 
Corrisponde, in italiano, alla varianza interna data dal rapporto fra la devianza interna 
(o devianza entro i gruppi) e la numerosita' totale. L'RMSE puo' essere anche calcolato come deviazione standard degli scarti.
L'MSE ed RMSE non sono quantita' a-dimensionali, bensi' assumono l'unita' di misura della grandezza considerata (RMSE) ed il suo quadrato (MSE).
Per eliminare la dipendenza dalle dimensioni si utilizza il valore normalizzato NRMSE, spesso espresso in percentuale
vedere https://it.qwe.wiki/wiki/Root-mean-square_deviation
                     ")    
    
    vals$indici_correlazione<-text_grob(valori_indici,  color = "blue")
    
    valori_indici
    
  })
  
  #######################################################################################
  
  
  
  
  ## clicking on the export button will generate a pdf file 
  ## containing all grobs
  output$export = downloadHandler(
    filename = function() {"Analisi correlazione tra due serie.pdf"},
    content = function(file) {
      #pdf(file, onefile = TRUE, paper='a4')
      pdf(file, width=8, height=11)
      
      serie_1<-input$corr_1_ui_select_serie
      serie_2<-input$corr_2_ui_select_serie
      
      
      titolo_pdf<-paste("Analisi correlazione tra ",serie_1, " vs ",serie_2)
      matrice_layout<-rbind(c(1),c(2),c(3),c(4),c(5))
      
      grid.arrange(
                  vals$titolo_analisi_correlazione
                  ,vals$ac_plot_scatter
                  ,vals$ac_plot_boxplot
                  ,vals$plot_serie_sovrapposte
                  ,vals$summary_correlazione
                  ,vals$indici_correlazione
                  ,nrow =6
                  
                  #,layout_matrix = matrice_layout
                  #,top="titolo_pdf"
                  #,nrow =5
                  #,ncol=1
                  #,heights = c(15,15,20,15,15)
                   ) 
      dev.off()
    }
  )
  
  
  
  
  
} # fine server
########################################################################################################
########################################################################################################
